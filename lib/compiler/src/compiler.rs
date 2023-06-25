use std::{iter::Peekable, mem::size_of, unreachable};

use cursor::{Col, Line};
use errors::{RloxError, RloxErrors};
use gc::{Heap, Function, StringInterner, Chunk, Value, RloxString, Object};
use instructions::{Instruction, Jump, Arity};
use log::trace;
use scanner::{token::TokenData, Token, TokenStream, TokenType};

use TokenType::*;

use crate::scope::Scope;

pub type Result<T> = std::result::Result<T, RloxErrors>;

pub struct CompilerError {
    error: CompilerErrorType,
    line: Line,
    col: Col,
}

impl From<CompilerError> for RloxError {
    fn from(error: CompilerError) -> Self {
        RloxError { line: error.line, col: error.col, message: error.error.to_string() }
    }
}

impl CompilerError {
    fn new(error: CompilerErrorType, token: Token) -> Self {
        Self { error, line: token.line(), col: token.col() }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum CompilerErrorType {
    #[error("Too many constants")]
    TooManyConstants,
    #[error("Too many locals")]
    TooManyLocals,
    #[error("Expected EOF")]
    ExpectedEof,
    #[error("Expected ')' after '{0}")]
    ExpectedRightParen(&'static str),
    #[error("Expected expression")]
    ExpectedExpression,
    #[error("Expected ';'")]
    ExpectedSemicolon,
    #[error("Expected variable name")]
    ExpectedVariableName,
    #[error("Invalid assignment target")]
    InvalidAssignmentTarget,
    #[error("Expected '}}' after block")]
    ExpectedRightBrace,
    #[error("Expected '{{' after {after}")]
    ExpectedLeftBrace { after: &'static str },
    #[error("Variable already in scope")]
    VariableAlreadyInScope,
    #[error("Expected '(' after '{0}'")]
    ExpectedLeftParen(&'static str),
    #[error("Too much code to jump over")]
    TooLargeJump,
    #[error("Expected function name")]
    ExpectedFunctionName,
    #[error("Too many function arguments")]
    TooManyArguments,
    #[error("Expected parameter name")]
    ExpectedParameterName,
    #[error("Can't return from top-level code")]
    ReturnOutsideFunction,
}

#[repr(u8)]
#[derive(
    Debug,
    Copy,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    num_enum::IntoPrimitive,
    num_enum::TryFromPrimitive,
)]
enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    pub fn next_higher_precedence(self) -> Self {
        let prim: u8 = self.into();
        (prim + 1).try_into().unwrap()
    }

    pub fn lower_or_equal(self, other: Self) -> bool {
        self as u8 <= other as u8
    }
}

#[derive(Debug)]
struct Local<'a> {
    name: &'a str,
    depth: Option<usize>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum FunctionType {
    Function,
    Script,
}

#[derive(Debug, PartialEq)]
enum Upvalue {
    Local(u8),
    Upvalue(u8),
}

// This corresponds to the stack of compilers in clox
#[derive(Debug)]
struct FunctionToCompile<'a> {
    ty: FunctionType,
    function: Function,
    locals: Vec<Local<'a>>,
    scope: Scope,
    upvalues: Vec<Upvalue>,
}

impl FunctionToCompile<'_> {
    fn script(interner: &mut StringInterner) -> Self {
        Self::new(
            FunctionType::Script,
            Function::new(Arity(0), "", interner),
            Scope::global(),
        )
    }

    fn new(ty: FunctionType, function: Function, scope: Scope) -> Self {
        let mut res = Self { ty, function, locals: Vec::with_capacity(u8::MAX as usize + 1), scope, upvalues: Vec::with_capacity(8) };

        res.locals.push(Local { name: "", depth: Some(0) });
        res
    }
}

#[derive(Debug)]
pub struct Compiler<'a, 'b> {
    token_stream: Peekable<TokenStream<'a>>,
    functions: Vec<FunctionToCompile<'a>>,
    interner: &'b mut StringInterner,
    heap: &'b mut Heap,
}

impl<'a, 'b> Compiler<'a, 'b> {
    pub fn new(source: &'a str, interner: &'b mut StringInterner, heap: &'b mut Heap) -> Self {
        // Don't think anyone will nest more than 8 functions in the real world
        let mut data_stack = Vec::with_capacity(8);
        data_stack.push(FunctionToCompile::script(interner));
        Self {
            token_stream: TokenStream::new(source).peekable(),
            functions: data_stack,
            interner,
            heap,
        }
    }

    fn current_function_type(&self) -> FunctionType {
        self.functions.last().unwrap().ty
    }

    fn current_function_mut(&mut self) -> &mut Function {
        &mut self.functions.last_mut().unwrap().function
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.current_function_mut().chunk
    }

    fn num_locals(&self) -> usize {
        self.locals().len()
    }

    fn locals(&self) -> &[Local] {
        &self.functions.last().unwrap().locals
    }

    fn locals_mut(&mut self) -> &mut Vec<Local<'a>> {
        &mut self.functions.last_mut().unwrap().locals
    }

    fn current_scope(&self) -> &Scope {
        &self.functions.last().unwrap().scope
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        &mut self.functions.last_mut().unwrap().scope
    }

    fn upvalues(&self) -> &[Upvalue] {
        &self.functions.last().unwrap().upvalues
    }

    fn upvalues_mut(&mut self) -> &mut Vec<Upvalue> {
        &mut self.functions.last_mut().unwrap().upvalues
    }

    pub fn compile(mut self) -> std::result::Result<Function, RloxErrors> {
        let mut errors = RloxErrors(Vec::new());

        let final_token = loop {
            if let Ok(token) = self.consume(Eof)? {
                break token;
            }

            match self.declaration() {
                Ok(()) => (),
                Err(mut e) => {
                    log::trace!("Hit error: {:?}, syncing...", e);
                    self.synchronize();
                    errors.0.append(&mut e);
                }
            }
        };

        self.return_nil(final_token.line());

        if errors.is_empty() {
            log::debug!(
                "Compiled chunk [{:?}]: {:?}",
                self.current_function_type(),
                self.current_chunk().clone().resolve(self.interner)
            );
            assert!(self.functions.len() == 1);
            Ok(self.functions.pop().unwrap().function)
        } else {
            Err(errors)
        }
    }

    fn return_nil(&mut self, line: Line) {
        self.current_chunk().write_instruction(Instruction::Nil, line);
        self.current_chunk().write_instruction(Instruction::Return, line);
    }

    fn declaration(&mut self) -> Result<()> {
        if self.consume(Var)?.is_ok() {
            self.var_declaration()
        } else if self.consume(Fun)?.is_ok() {
            self.fun_declaration()
        } else {
            self.statement()
        }
    }

    fn fun_declaration(&mut self) -> Result<()> {
        let (global, token) = self.parse_variable(CompilerErrorType::ExpectedFunctionName)?;
        self.mark_initialized();
        self.function(FunctionType::Function, token.lexeme())?;
        self.define_variable(global, token.line())?;
        Ok(())
    }

    fn function(&mut self, ty: FunctionType, name: &'a str) -> Result<()> {
        let function_token = self
            .consume_or_error(LeftParen, CompilerErrorType::ExpectedLeftParen("function name"))?;

        self.functions.push(FunctionToCompile::new(ty, Function::new(Arity(0), name, self.interner), self.current_scope().next_depth()));

        let mut arity = Arity(0);
        if self.peek().unwrap()? != RightParen {
            loop {
                arity.0 = arity.0.checked_add(1).ok_or_else(|| {
                    CompilerError::new(CompilerErrorType::TooManyArguments, function_token.clone())
                })?;

                let (constant_index, token) =
                    self.parse_variable(CompilerErrorType::ExpectedParameterName)?;
                self.define_variable(constant_index, token.line())?;

                if self.consume(Comma)?.is_err() {
                    break;
                }
            }
        }

        self.current_function_mut().arity = arity;

        self.consume_or_error(RightParen, CompilerErrorType::ExpectedRightParen("parameters"))?;

        self.consume_or_error(
            LeftBrace,
            CompilerErrorType::ExpectedLeftBrace { after: "parameters" },
        )?;

        let end_fun_line = self.block()?.line();

        // We don't call self.end_scope() here, as locals are popped by the VM when the function returns
        self.current_scope_mut().end_scope();

        let function = self.functions.pop().unwrap().function;

        log::debug!(
            "Compiled function [{:?}]: {:?}",
            function.name.resolve(self.interner),
            function.chunk.resolve(self.interner),
            );

        let constant_index = self.add_object_constant(function, &function_token)?;
        self.current_chunk()
            .write_instruction(Instruction::Closure { constant_index }, function_token.line());

        Ok(())
    }

    fn call(&mut self, token: &Token) -> Result<()> {
        let arg_count = self.argument_list()?.into();
        self.current_chunk().write_instruction(Instruction::Call { arg_count }, token.line());
        Ok(())
    }

    fn argument_list(&mut self) -> Result<u8> {
        let mut arg_count = 0;

        if self.peek().unwrap()? != RightParen {
            loop {
                self.expression()?;

                if arg_count == 255 {
                    return Err(CompilerError::new(
                        CompilerErrorType::TooManyArguments,
                        self.peek_token().unwrap()?,
                    )
                    .into());
                }

                arg_count += 1;

                if self.consume(Comma)?.is_err() {
                    break;
                }
            }
        }
        self.consume_or_error(
            TokenType::RightParen,
            CompilerErrorType::ExpectedRightParen("arguments"),
        )?;

        Ok(arg_count)
    }

    fn variable(&mut self, token: &Token<'a>, can_assign: bool) -> Result<()> {
        self.named_variable(token, can_assign)
    }

    fn named_variable(&mut self, identifier: &Token<'a>, can_assign: bool) -> Result<()> {
        let (get_instr, set_instr) = if let Some(stack_slot) = Self::resolve_local(self.locals(), identifier) {
            (Instruction::GetLocal { stack_slot }, Instruction::SetLocal { stack_slot })
        } else if let Some(upvalue_index) = Self::resolve_upvalue(&mut self.functions, identifier) {
            (Instruction::GetUpvalue { upvalue_index }, Instruction::SetUpvalue { upvalue_index })
        } else {
            let name = RloxString::new(identifier.lexeme(), self.interner);
            let constant_index = self.add_object_constant(name, identifier)?;
            (Instruction::GetGlobal { constant_index }, Instruction::SetGlobal { constant_index })
        };

        if can_assign && self.consume(Equal)?.is_ok() {
            self.expression()?;
            self.current_chunk().write_instruction(set_instr, identifier.line());
        } else {
            self.current_chunk().write_instruction(get_instr, identifier.line());
        }
        Ok(())
    }

    fn resolve_upvalue(functions: &mut [FunctionToCompile], identifier: &Token<'a>) -> Option<u8> {
        let previous = functions.iter().rev().nth(1)?;

        if let Some(index) = Self::resolve_local(&previous.locals, identifier) {
            return Some(Self::add_upvalue(&mut functions.last_mut().unwrap().upvalues, Upvalue::Local(index)));
        }

        let functions_len = functions.len();
        if let Some(upvalue) = Self::resolve_upvalue(&mut functions[..functions_len-1], identifier) {
            return Some(Self::add_upvalue(&mut functions.last_mut().unwrap().upvalues, Upvalue::Upvalue(upvalue)));
        }

        None
    }

    fn add_upvalue(upvalues: &mut Vec<Upvalue>, upvalue: Upvalue) -> u8 {
        match upvalues.iter().position(|u| u == &upvalue) {
            Some(index) => index as u8,
            None => {
                upvalues.push(upvalue);
                upvalues.len() as u8 - 1
            }
        }
    }

    fn resolve_local(locals: &[Local], identifier: &Token<'a>) -> Option<u8> {
        trace!("Resolving local {:?} from {:?}", identifier.lexeme(), locals);
        locals.iter().rev().enumerate().find_map(|(i, local)| {
            (local.depth.is_some() && local.name == identifier.lexeme())
                .then_some((locals.len() - i - 1) as u8)
        })
    }

    fn var_declaration(&mut self) -> Result<()> {
        let (global_constant_index, token) =
            self.parse_variable(CompilerErrorType::ExpectedVariableName)?;
        if self.consume(Equal)?.is_ok() {
            self.expression()?;
        } else {
            self.current_chunk().write_instruction(Instruction::Nil, token.line());
        }
        self.consume_or_error(Semicolon, CompilerErrorType::ExpectedSemicolon)?;
        self.define_variable(global_constant_index, token.line())
    }

    fn parse_variable(&mut self, error: CompilerErrorType) -> Result<(u8, Token<'a>)> {
        let token = self.consume_or_error(Identifier, error)?;

        self.declare_variable(&token)?;
        if self.current_scope().inside_local() {
            return Ok((0, token));
        }

        let name = RloxString::new(token.lexeme(), self.interner);
        let index =
            self.add_object_constant(name, &token)?;

        Ok((index, token))
    }

    fn declare_variable(&mut self, name: &Token<'a>) -> Result<()> {
        if self.current_scope().inside_global() {
            return Ok(());
        }

        if self
            .locals()
            .iter()
            .rev()
            .take_while(|local| match local.depth {
                Some(depth) => depth == self.current_scope().depth(),
                None => false,
            })
            .any(|local| local.name == name.lexeme())
        {
            return Err(CompilerError::new(
                CompilerErrorType::VariableAlreadyInScope,
                name.clone(),
            )
            .into());
        }

        self.add_local(name)
    }

    fn add_local(&mut self, name: &Token<'a>) -> Result<()> {
        if self.num_locals() == self.functions.last().unwrap().locals.capacity() {
            return Err(CompilerError::new(CompilerErrorType::TooManyLocals, name.clone()).into());
        }

        self.locals_mut().push(Local { name: name.lexeme(), depth: None });
        Ok(())
    }

    fn mark_initialized(&mut self) {
        if self.current_scope().inside_local() {
            self.locals_mut().last_mut().unwrap().depth = Some(self.current_scope().depth());
        }
    }

    fn define_variable(&mut self, constant_index: u8, line: Line) -> Result<()> {
        if self.current_scope().inside_local() {
            self.mark_initialized();
            return Ok(());
        }
        self.current_chunk().write_instruction(Instruction::DefineGlobal { constant_index }, line);
        Ok(())
    }

    fn statement(&mut self) -> Result<()> {
        if self.consume(Print)?.is_ok() {
            self.print_statement()
        } else if self.consume(If)?.is_ok() {
            self.if_statement()
        } else if self.consume(While)?.is_ok() {
            self.while_statement()
        } else if self.consume(Return)?.is_ok() {
            self.return_statement()
        } else if self.consume(LeftBrace)?.is_ok() {
            self.current_scope_mut().begin_scope();
            let right_brace_line = self.block()?.line();
            self.end_scope(right_brace_line);
            Ok(())
        } else {
            self.expression_statement()
        }
    }

    fn return_statement(&mut self) -> Result<()> {
        if self.current_function_type() == FunctionType::Script {
            return Err(CompilerError::new(
                CompilerErrorType::ReturnOutsideFunction,
                self.peek_token().unwrap().unwrap().clone(),
            )
            .into());
        }

        if let Ok(token) = self.consume(Semicolon)? {
            self.return_nil(token.line());
        } else {
            self.expression()?;
            let token = self.consume_or_error(Semicolon, CompilerErrorType::ExpectedSemicolon)?;
            self.current_chunk().write_instruction(Instruction::Return, token.line());
        }
        Ok(())
    }

    fn while_statement(&mut self) -> Result<()> {
        let loop_start = self.current_chunk().code().len() - 1;
        self.consume_or_error(LeftParen, CompilerErrorType::ExpectedLeftParen("while"))?;
        self.expression()?;
        let token =
            self.consume_or_error(RightParen, CompilerErrorType::ExpectedRightParen("while"))?;
        let line = token.line();

        let exit_jump = self.write_jump(Instruction::JumpIfFalse(Jump(0)), line);
        self.current_chunk().write_instruction(Instruction::Pop, line);
        self.statement()?;
        self.write_loop(loop_start, &token)?;

        self.patch_jump(exit_jump, token)?;
        self.current_chunk().write_instruction(Instruction::Pop, line);

        Ok(())
    }

    fn write_loop(&mut self, loop_start: usize, token: &Token) -> Result<()> {
        let jump = Jump(
            (self.current_chunk().code().len() - loop_start + size_of::<Jump>())
                .try_into()
                .map_err(|_| CompilerError::new(CompilerErrorType::TooLargeJump, token.clone()))?,
        );
        self.current_chunk().write_instruction(Instruction::Loop(jump), token.line());
        Ok(())
    }

    fn if_statement(&mut self) -> Result<()> {
        self.consume_or_error(LeftParen, CompilerErrorType::ExpectedLeftParen("if"))?;
        self.expression()?;
        let token =
            self.consume_or_error(RightParen, CompilerErrorType::ExpectedRightParen("if"))?;

        let then_jump = self.write_jump(Instruction::JumpIfFalse(Jump(0)), token.line());
        self.current_chunk().write_instruction(Instruction::Pop, token.line());
        self.statement()?;

        let else_jump = self.write_jump(Instruction::Jump(Jump(0)), token.line());

        self.patch_jump(then_jump, token.clone())?;
        self.current_chunk().write_instruction(Instruction::Pop, token.line());

        if self.consume(Else)?.is_ok() {
            self.statement()?;
        }
        self.patch_jump(else_jump, token)?;

        Ok(())
    }

    fn write_jump(&mut self, instruction: Instruction, line: Line) -> usize {
        self.current_chunk().write_instruction(instruction, line);
        self.current_chunk().code().len() - size_of::<Jump>()
    }

    fn patch_jump(&mut self, offset: usize, token: Token<'a>) -> Result<()> {
        let jump = self.current_chunk().code().len() - offset - size_of::<Jump>();

        self.current_chunk().patch_jump(
            offset,
            Jump(
                jump.try_into()
                    .map_err(|_| CompilerError::new(CompilerErrorType::TooLargeJump, token))?,
            ),
        );
        Ok(())
    }

    fn end_scope(&mut self, line: Line) {
        let last_local_in_scope = self
            .locals()
            .iter()
            .rev()
            .enumerate()
            .find_map(|(index, local)| (local.depth != Some(self.current_scope().depth())).then_some(index))
            .unwrap_or(0);

        let num_locals_to_pop = last_local_in_scope as u8;
        self.current_chunk().write_instruction(Instruction::PopN(num_locals_to_pop), line);

        let num_locals = self.num_locals();
        self.locals_mut().truncate(num_locals - last_local_in_scope);

        self.current_scope_mut().end_scope();
    }

    fn block(&mut self) -> Result<Token> {
        loop {
            match self.peek().unwrap()? {
                RightBrace | Eof => break,
                _ => self.declaration()?,
            }
        }

        self.consume_or_error(RightBrace, CompilerErrorType::ExpectedRightBrace)
    }

    fn print_statement(&mut self) -> Result<()> {
        self.expression()?;
        let line = self.consume_or_error(Semicolon, CompilerErrorType::ExpectedSemicolon)?.line();
        self.current_chunk().write_instruction(Instruction::Print, line);
        Ok(())
    }

    fn expression_statement(&mut self) -> Result<()> {
        self.expression()?;
        let line = self.consume_or_error(Semicolon, CompilerErrorType::ExpectedSemicolon)?.line();
        self.current_chunk().write_instruction(Instruction::Pop, line);
        Ok(())
    }

    fn expression(&mut self) -> Result<()> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn literal(&mut self, prefix_token: &Token<'a>) -> Result<()> {
        trace!("Compiling literal: {:?}", prefix_token);
        self.current_chunk().write_instruction(
            match prefix_token.ty() {
                True => Instruction::True,
                False => Instruction::False,
                Nil => Instruction::Nil,
                _ => unreachable!(),
            },
            prefix_token.line(),
        );

        Ok(())
    }

    fn number(&mut self, prefix_token: &Token<'a>) -> Result<()> {
        trace!("Compiling number: {:?}", prefix_token);
        let value = match prefix_token.data {
            TokenData::Number(n) => n,
            _ => unreachable!(),
        };
        let index = self.add_constant(Value::Number(value), prefix_token)?;

        self.current_chunk()
            .write_instruction(Instruction::Constant { index }, prefix_token.line());
        Ok(())
    }

    fn string(&mut self, prefix_token: &Token<'a>) -> Result<()> {
        let s = match prefix_token.data {
            TokenData::Str(s) => s,
            _ => unreachable!(),
        };

        let name = RloxString::new(s, self.interner);
        let index = self.add_object_constant(name, prefix_token)?;
        self.current_chunk()
            .write_instruction(Instruction::Constant { index }, prefix_token.line());

        Ok(())
    }

    fn grouping(&mut self, _: &Token<'a>) -> Result<()> {
        self.expression()?;
        self.consume_or_error(RightParen, CompilerErrorType::ExpectedRightParen("expression"))?;
        Ok(())
    }

    fn unary(&mut self, prefix_token: &Token<'a>) -> Result<()> {
        // Compile the operand
        self.parse_precedence(Precedence::Unary)?;
        self.current_chunk().write_instruction(
            match prefix_token.ty() {
                Minus => Instruction::Negate,
                Bang => Instruction::Not,
                _ => unreachable!(),
            },
            prefix_token.line(),
        );
        Ok(())
    }

    fn binary(&mut self, operator_token: &Token<'a>) -> Result<()> {
        let next_higher_prec = self.token_precendence(operator_token.ty()).next_higher_precedence();
        self.parse_precedence(next_higher_prec)?;

        match operator_token.ty() {
            Minus => {
                self.current_chunk().write_instruction(Instruction::Subtract, operator_token.line())
            }
            Plus => self.current_chunk().write_instruction(Instruction::Add, operator_token.line()),
            Slash => {
                self.current_chunk().write_instruction(Instruction::Divide, operator_token.line())
            }
            Star => {
                self.current_chunk().write_instruction(Instruction::Multiply, operator_token.line())
            }
            BangEqual => self
                .current_chunk()
                .write_instructions([Instruction::Equal, Instruction::Not], operator_token.line()),
            EqualEqual => {
                self.current_chunk().write_instruction(Instruction::Equal, operator_token.line())
            }
            Greater => {
                self.current_chunk().write_instruction(Instruction::Greater, operator_token.line())
            }
            GreaterEqual => self
                .current_chunk()
                .write_instructions([Instruction::Less, Instruction::Not], operator_token.line()),
            Less => {
                self.current_chunk().write_instruction(Instruction::Less, operator_token.line())
            }
            LessEqual => self.current_chunk().write_instructions(
                [Instruction::Greater, Instruction::Not],
                operator_token.line(),
            ),
            _ => unreachable!(),
        };
        Ok(())
    }

    fn advance_with_prefix_rule(&mut self, can_assign: bool) -> Result<()> {
        let token = self.advance()?;
        trace!("Advancing with prefix rule for {}", token);
        match token.ty() {
            Number => self.number(&token),
            True | False | Nil => self.literal(&token),
            LeftParen => self.grouping(&token),
            Minus | Bang => self.unary(&token),
            Str => self.string(&token),
            Identifier => self.variable(&token, can_assign),
            _ => {
                Err(CompilerError::new(CompilerErrorType::ExpectedExpression, token.clone()).into())
            }
        }
    }

    fn advance_with_infix_rule(&mut self) -> Result<()> {
        let token = self.advance()?;
        trace!("Advancing with infix rule for {}", token);
        match token.ty() {
            Plus | Minus | Slash | Star | EqualEqual | BangEqual | Greater | GreaterEqual
            | Less | LessEqual => self.binary(&token),
            LeftParen => self.call(&token),
            _ => {
                Err(CompilerError::new(CompilerErrorType::ExpectedExpression, token.clone()).into())
            }
        }
    }

    fn token_precendence(&mut self, token_type: TokenType) -> Precedence {
        match token_type {
            Plus | Minus => Precedence::Term,
            Slash | Star => Precedence::Factor,
            EqualEqual | BangEqual => Precedence::Equality,
            Greater | GreaterEqual | Less | LessEqual => Precedence::Comparison,
            LeftParen => Precedence::Call,
            _ => Precedence::None,
        }
    }

    fn peek_precendence(&mut self) -> Result<Precedence> {
        let peek = self.peek().unwrap()?;
        Ok(self.token_precendence(peek))
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<()> {
        let can_assign = precedence.lower_or_equal(Precedence::Assignment);
        trace!("Parsing precedence: {:?}, can_assign: {can_assign}", precedence);
        self.advance_with_prefix_rule(can_assign)?;

        while precedence <= self.peek_precendence()? {
            self.advance_with_infix_rule()?;
        }

        if can_assign {
            // If the equal has not been consumed, we tried assigning to an invalid target,
            // so report that error here
            if let Ok(token) = self.consume(Equal)? {
                return Err(
                    CompilerError::new(CompilerErrorType::InvalidAssignmentTarget, token).into()
                );
            }
        }

        Ok(())
    }

    fn synchronize(&mut self) {
        loop {
            log::trace!("Syncing... {:?}", self.peek_token());
            match self.peek_token() {
                Some(Ok(t)) => match t.ty() {
                    Semicolon => {
                        self.advance().unwrap();
                        return;
                    }
                    Class | Fun | Var | For | If | While | Print | Return | Eof => return,
                    _ => {}
                },
                Some(Err(_)) => (),
                None => return,
            }
            self.advance().unwrap();
        }
    }
}

// Helpers
impl<'a, 'b> Compiler<'a, 'b> {
    fn consume(
        &mut self,
        token_type: TokenType,
    ) -> Result<std::result::Result<Token<'a>, Token<'a>>> {
        match self.peek_token() {
            Some(Ok(t)) if t.ty() == token_type => Ok(Ok(self.advance().unwrap())),
            Some(Ok(t)) => Ok(Err(t)),
            Some(Err(err)) => Err(err),
            None => unreachable!("Should have hit Eof"),
        }
    }

    fn consume_or_error(
        &mut self,
        token_type: TokenType,
        error: CompilerErrorType,
    ) -> Result<Token<'a>> {
        match self.consume(token_type)? {
            Ok(token) => Ok(token),
            Err(token) => Err(CompilerError::new(error, token).into()),
        }
    }

    fn peek_token(&mut self) -> Option<Result<Token<'a>>> {
        self.token_stream.peek().cloned().map(|t| t.map_err(|e| e.into()))
    }

    fn peek(&mut self) -> Option<Result<TokenType>> {
        self.peek_token().map(|t| t.map(|t| t.data.into()))
    }

    fn advance(&mut self) -> Result<Token<'a>> {
        self.token_stream.next().unwrap().map_err(|e| e.into())
    }

    fn add_object_constant(&mut self, obj: impl Into<Object>, token: &Token<'a>) -> Result<u8> {
        let obj_ref = self.heap.alloc(obj.into());
        self.add_constant(obj_ref, token)
    }

    fn add_constant(&mut self, value: impl Into<Value>, token: &Token<'a>) -> Result<u8> {
        self.current_chunk()
            .add_constant(value.into())
            .ok_or(CompilerError::new(CompilerErrorType::TooManyConstants, token.clone()).into())
    }
}
