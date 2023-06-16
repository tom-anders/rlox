use std::{
    cell::RefCell, debug_assert, iter::Peekable, mem::size_of, println, rc::Rc, unreachable,
};

use bytecode::{
    chunk::Chunk,
    instructions::{Instruction, Jump, OpCode},
    value::{Function, Object, ObjectData, Value},
};
use cursor::{Col, Line};
use errors::{RloxError, RloxErrors};
use log::{debug, info, trace};
use scanner::{token::TokenData, Token, TokenStream, TokenType};

use TokenType::*;

pub type Result<T> = std::result::Result<T, RloxErrors>;

#[derive(Debug)]
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

    pub fn next_lower_precedence(self) -> Self {
        let prim: u8 = self.into();
        (prim - 1).try_into().unwrap()
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

mod scope;
use scope::Scope;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum FunctionType {
    Function,
    Script,
}

#[derive(Debug)]
struct CurrentFunction {
    ty: FunctionType,
    function: Function,
}

impl CurrentFunction {
    fn script() -> Self {
        Self { ty: FunctionType::Script, function: Function::default() }
    }

    fn new(ty: FunctionType, function: Function) -> Self {
        Self { ty, function }
    }
}

#[derive(Debug)]
pub struct Compiler<'a> {
    token_stream: Peekable<TokenStream<'a>>,
    current_function: CurrentFunction,
    locals: Vec<Local<'a>>,
    scope: Scope,
}

impl<'a> Compiler<'a> {
    fn new(
        token_stream: Peekable<TokenStream<'a>>,
        current_function: CurrentFunction,
        scope: Scope,
    ) -> Self {
        let mut locals = Vec::with_capacity(u8::MAX as usize + 1);
        locals.push(Local { name: "", depth: Some(0) });
        Self { token_stream, current_function, locals, scope }
    }

    pub fn from_source(source: &'a str) -> Self {
        Self::new(
            TokenStream::new(source).peekable(),
            CurrentFunction::script(),
            Scope::global(),
        )
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.current_function.function.chunk
    }

    pub fn compile(self) -> std::result::Result<Function, RloxErrors> {
        let (function, _) = self.compile_until_token(Eof)?;
        Ok(function)
    }

    fn compile_function(mut self, function_token: Token<'a>) -> std::result::Result<(Function, Peekable<TokenStream<'a>>), RloxErrors> {
        let mut arity = 0;
        if self.peek().unwrap()? != RightParen {
            loop {
                arity += 1;
                if arity > 255 {
                    return Err(CompilerError::new(CompilerErrorType::TooManyArguments, function_token).into());
                }

                let (constant_index, token) = self.parse_variable(CompilerErrorType::ExpectedParameterName)?;
                self.define_variable(constant_index, token.line())?;

                if self.consume(Comma)?.is_err() {
                    break;
                }
            }
        }
        self.current_function.function.arity = arity;

        self.consume_or_error(RightParen, CompilerErrorType::ExpectedRightParen("parameters"))?;
        self.consume_or_error(
            LeftBrace,
            CompilerErrorType::ExpectedLeftBrace { after: "parameters" },
        )?;

        self.compile_until_token(RightBrace)
    }

    fn compile_until_token(
        mut self,
        until_token: TokenType,
    ) -> std::result::Result<(Function, Peekable<TokenStream<'a>>), RloxErrors> {
        let mut errors = RloxErrors(Vec::new());

        let final_token = loop {
            if let Ok(token) = self.consume(until_token)? {
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
            log::trace!("Compiled chunk [{:?}]: {:?}", 
                self.current_function.ty.clone(),
                self.current_chunk());
            Ok((self.current_function.function, self.token_stream))
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

        let compiler =
            Self::new(self.token_stream.clone(), CurrentFunction::new(ty, Function::new(0, name.to_string())), Scope::local());

        let (function, token_stream) = compiler.compile_function(function_token.clone())?;
        self.token_stream = token_stream;

        let index = self.add_constant(Value::function(function), &function_token)?;

        self.current_chunk().write_instruction(Instruction::Constant { index }, Line(0));

        Ok(())
    }

    fn call(&mut self, token: &Token) -> Result<()> {
        let arg_count = self.argument_list()?;
        self.current_chunk().write_instruction(Instruction::Call { arg_count }, token.line());
        Ok(())
    }

    fn argument_list(&mut self) -> Result<u8> {
        let mut arg_count = 0;

        if self.peek().unwrap()? != RightParen {
            loop {
                self.expression()?;

                if arg_count == 255 {
                    return Err(CompilerError::new(CompilerErrorType::TooManyArguments, self.peek_token().unwrap()?).into());
                }

                arg_count += 1;

                if self.consume(Comma)?.is_err() {
                    break;
                }
            }
        }
        self.consume_or_error(TokenType::RightParen, CompilerErrorType::ExpectedRightParen("arguments"))?;

        Ok(arg_count)
    }

    fn variable(&mut self, token: &Token<'a>, can_assign: bool) -> Result<()> {
        self.named_variable(token, can_assign)
    }

    fn named_variable(&mut self, identifier: &Token<'a>, can_assign: bool) -> Result<()> {
        let (get_instr, set_instr) = match self.resolve_local(identifier) {
            Some(stack_slot) => {
                (Instruction::GetLocal { stack_slot }, Instruction::SetLocal { stack_slot })
            }
            None => {
                let name = Value::string(identifier.lexeme().to_string());
                let constant_index = self.add_constant(name, identifier)?;
                (
                    Instruction::GetGlobal { constant_index },
                    Instruction::SetGlobal { constant_index },
                )
            }
        };

        if can_assign && self.consume(Equal)?.is_ok() {
            self.expression()?;
            self.current_chunk().write_instruction(set_instr, identifier.line());
        } else {
            self.current_chunk().write_instruction(get_instr, identifier.line());
        }
        Ok(())
    }

    fn resolve_local(&self, identifier: &Token<'a>) -> Option<u8> {
        trace!("Resolving local {:?} from {:?}", identifier.lexeme(), self.locals);
        self.locals.iter().rev().enumerate().find_map(|(i, local)| {
            (local.depth.is_some() && local.name == identifier.lexeme())
                .then_some((self.locals.len() - i - 1) as u8)
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
        if self.scope.inside_local() {
            return Ok((0, token));
        }

        let index = self.add_constant(Value::string(token.lexeme()), &token)?;

        Ok((index, token))
    }

    fn declare_variable(&mut self, name: &Token<'a>) -> Result<()> {
        if self.scope.inside_global() {
            return Ok(());
        }

        if self
            .locals
            .iter()
            .rev()
            .take_while(|local| match local.depth {
                Some(depth) => depth == self.scope.depth(),
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
        if self.locals.len() == self.locals.capacity() {
            return Err(CompilerError::new(CompilerErrorType::TooManyLocals, name.clone()).into());
        }

        self.locals.push(Local { name: name.lexeme(), depth: None });
        Ok(())
    }

    fn mark_initialized(&mut self) {
        if self.scope.inside_local() {
            self.locals.last_mut().unwrap().depth = Some(self.scope.depth());
        }
    }

    fn define_variable(&mut self, constant_index: u8, line: Line) -> Result<()> {
        if self.scope.inside_local() {
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
        } else if self.consume(Return)?.is_ok() {
            self.return_statement()
        } else if self.consume(LeftBrace)?.is_ok() {
            self.scope.begin_scope();
            let right_brace_line = self.block()?.line();
            self.end_scope(right_brace_line);
            Ok(())
        } else {
            self.expression_statement()
        }
    }

    fn return_statement(&mut self) -> Result<()> {
        if self.current_function.ty == FunctionType::Script {
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
            .locals
            .iter()
            .rev()
            .enumerate()
            .find_map(|(index, local)| (local.depth != Some(self.scope.depth())).then_some(index))
            .unwrap_or(0);

        let num_locals_to_pop = (self.locals.len() - last_local_in_scope) as u8;
        self.current_chunk().write_instruction(Instruction::PopN(num_locals_to_pop), line);

        self.locals.truncate(last_local_in_scope);

        self.scope.end_scope();
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
        }
        .to_string();

        let index = self.add_constant(Value::Object(Box::new(Object::string(s))), prefix_token)?;
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
impl<'a> Compiler<'a> {
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

    fn add_constant(&mut self, value: Value, token: &Token<'a>) -> Result<u8> {
        self.current_chunk()
            .add_constant(value)
            .ok_or(CompilerError::new(CompilerErrorType::TooManyConstants, token.clone()).into())
    }
}
