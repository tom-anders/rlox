use std::{fmt::Display, iter::Peekable, mem::size_of, unreachable};

use cursor::Line;
use gc::{Chunk, Closure, ClosureRef, Function, Heap, Object, ObjectRef, Value};
use instructions::{Arity, CompiledUpvalue, Instruction, Jump};
use itertools::Itertools;
use log::trace;
use scanner::{token::TokenData, ScanError, ScanErrorType, Token, TokenStream, TokenType};

use strings::string_interner::StringInterner;
use TokenType::*;

use crate::scope::Scope;

pub type Result<T> = std::result::Result<T, CompilerErrors>;

#[derive(thiserror::Error, Debug, PartialEq)]
#[error("[line {line}] Error{}: {error}", if at.is_empty() { "".to_string() } else { format!(" at {at}") })]
pub struct CompilerError {
    error: CompilerErrorType,
    line: Line,
    at: String,
}

#[derive(thiserror::Error, Debug, PartialEq)]
pub struct CompilerErrors(pub Vec<CompilerError>);

impl Display for CompilerErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().map(|e| e.to_string()).collect::<Vec<_>>().join("\n"))
    }
}

impl From<CompilerError> for CompilerErrors {
    fn from(value: CompilerError) -> Self {
        Self(vec![value])
    }
}

impl CompilerError {
    pub fn new(error: CompilerErrorType, line: Line, at: impl ToString) -> Self {
        Self { error, line, at: at.to_string() }
    }
}

impl From<ScanError> for CompilerErrors {
    fn from(value: ScanError) -> Self {
        CompilerError {
            error: CompilerErrorType::ScanError(value.error),
            line: value.line,
            at: "".to_string(),
        }
        .into()
    }
}

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum CompilerErrorType {
    #[error("{0}")]
    ScanError(ScanErrorType),
    #[error("Too many constants in one chunk.")]
    TooManyConstants,
    #[error("Too many local variables in function.")]
    TooManyLocals,
    #[error("Expect EOF.")]
    ExpectedEof,
    #[error("Expect ')' after {0}.")]
    ExpectedRightParen(&'static str),
    #[error("Expect expression.")]
    ExpectedExpression,
    #[error("Expect ';'.")]
    ExpectedSemicolon,
    #[error("Expect ';' after {0}.")]
    ExpectedSemicolonAfter(&'static str),
    #[error("Expect variable name.")]
    ExpectedVariableName,
    #[error("Invalid assignment target.")]
    InvalidAssignmentTarget,
    #[error("Expect '}}' after {0}.")]
    ExpectedRightBrace(&'static str),
    #[error("Expect '{{' {0}.")]
    ExpectedLeftBrace(&'static str),
    #[error("Already a variable with this name in this scope.")]
    VariableAlreadyInScope,
    #[error("Expect '(' after '{0}'.")]
    ExpectedLeftParen(&'static str),
    #[error("Too much code to jump over.")]
    TooLargeJump,
    #[error("Loop body too large.")]
    LoopBodyTooLarge,
    #[error("Expect function name.")]
    ExpectedFunctionName,
    #[error("Can't have more than 255 arguments.")]
    TooManyArguments,
    #[error("Can't have more than 255 parameters.")]
    TooManyParameters,
    #[error("Expect parameter name.")]
    ExpectedParameterName,
    #[error("Can't return from top-level code.")]
    ReturnOutsideFunction,
    #[error("Expect class name.")]
    ExpectedClassName,
    #[error("Expect property name after '.'.")]
    ExpectedPropertyName,
    #[error("Expect method name.")]
    ExpectedMethodName,
    #[error("Can't use 'this' outside of a class.")]
    ThisOutsideClass,
    #[error("Can't return a value from an initializer.")]
    InitializerCannotReturn,
    #[error("Expect superclass name.")]
    ExpectedSuperclassName,
    #[error("A class can't inherit from itself.")]
    InheritanceFromSelf,
    #[error("Expect '.' after 'super'.")]
    ExpectedDotAfterSuper,
    #[error("Expect superclass method name.")]
    ExpectedSuperclassMethodName,
    #[error("Can't use 'super' outside of a class.")]
    SuperOutsideClass,
    #[error("Can't use 'super' in a class with no superclass.")]
    SuperclasslessClass,
    #[error("Too many closure variables in function.")]
    TooManyUpvalues,
    #[error("Can't read local variable in its own initializer.")]
    LocalInItsOwnInitializer,
}

impl CompilerErrorType {
    fn at(self, token: &Token) -> CompilerErrors {
        CompilerError::new(
            self,
            token.line(),
            match token.ty() {
                Eof => "end".to_string(),
                _ => format!("'{}'", token.lexeme()),
            },
        )
        .into()
    }
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
    is_captured: bool,
}

impl<'a> Local<'a> {
    pub fn new(name: &'a str, depth: Option<usize>) -> Local<'a> {
        Self { name, depth, is_captured: false }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum FunctionType {
    Function,
    Initializer,
    Script,
    Method,
}

// This corresponds to the stack of compilers in clox
#[derive(Debug)]
struct FunctionToCompile<'a> {
    ty: FunctionType,
    function: Function,
    locals: Vec<Local<'a>>,
    scope: Scope,
    upvalues: Vec<CompiledUpvalue>,
}

impl FunctionToCompile<'_> {
    fn script(interner: &mut StringInterner) -> Self {
        Self::new(FunctionType::Script, Function::new(Arity(0), "", interner), Scope::global())
    }

    fn new(ty: FunctionType, function: Function, scope: Scope) -> Self {
        let mut res = Self {
            ty,
            function,
            locals: Vec::with_capacity(u8::MAX as usize + 1),
            scope,
            upvalues: Vec::with_capacity(8),
        };

        res.locals
            .push(Local::new(if ty != FunctionType::Function { "this" } else { "" }, Some(0)));
        res
    }
}

#[derive(Debug, Clone)]
struct ClassToCompile {
    has_superclass: bool,
}

impl ClassToCompile {
    fn new() -> Self {
        Self { has_superclass: false }
    }
}

#[derive(Debug)]
pub struct Compiler<'a, 'b> {
    token_stream: Peekable<TokenStream<'a>>,
    current_token: Option<Token<'a>>,
    functions: Vec<FunctionToCompile<'a>>,
    classes: Vec<ClassToCompile>,
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
            current_token: None,
            functions: data_stack,
            classes: Vec::new(),
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

    fn current_class(&self) -> Option<&ClassToCompile> {
        self.classes.last()
    }

    fn current_class_mut(&mut self) -> Option<&mut ClassToCompile> {
        self.classes.last_mut()
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

    pub fn compile(mut self) -> std::result::Result<ClosureRef, CompilerErrors> {
        let mut errors = CompilerErrors(Vec::new());

        let final_token = loop {
            if let Ok(token) = self.consume(Eof)? {
                break token;
            }

            match self.declaration() {
                Ok(()) => (),
                Err(mut e) => {
                    log::trace!("Hit error: {:?}, syncing...", e);
                    self.synchronize();
                    errors.0.append(&mut e.0);
                }
            }
        };

        self.emit_return(final_token.line());

        if errors.0.is_empty() {
            log::debug!(
                "Compiled chunk [{:?}]: {:?}",
                self.current_function_type(),
                self.current_chunk()
            );
            assert!(self.functions.len() == 1);

            let function = self.heap.alloc(self.functions.pop().unwrap().function);
            let closure = self.heap.alloc(Closure::new(function.into(), vec![]));
            Ok(closure.into())
        } else {
            Err(errors)
        }
    }

    fn emit_return(&mut self, line: Line) {
        let return_value = if self.current_function_type() == FunctionType::Initializer {
            Instruction::GetLocal { stack_slot: 0 }
        } else {
            Instruction::Nil
        };
        self.current_chunk().write_instruction(return_value, line);
        self.current_chunk().write_instruction(Instruction::Return, line);
    }

    fn declaration(&mut self) -> Result<()> {
        if self.consume(Var)?.is_ok() {
            self.var_declaration()
        } else if self.consume(Class)?.is_ok() {
            self.class_declaration()
        } else if self.consume(Fun)?.is_ok() {
            self.fun_declaration()
        } else {
            self.statement()
        }
    }

    fn class_declaration(&mut self) -> Result<()> {
        let class_ident =
            self.consume_or_error(Identifier, CompilerErrorType::ExpectedClassName)?;
        let constant_index = self.add_identifier_constant(&class_ident)?;

        self.declare_variable(&class_ident)?;
        self.current_chunk()
            .write_instruction(Instruction::Class { constant_index }, class_ident.line());

        self.define_variable(constant_index, class_ident.line())?;

        self.classes.push(ClassToCompile::new());

        if self.consume(Less)?.is_ok() {
            let super_class_ident =
                self.consume_or_error(Identifier, CompilerErrorType::ExpectedSuperclassName)?;
            self.variable(&super_class_ident, false)?;

            self.current_scope_mut().begin_scope();
            self.add_local(&Token::synthetic_identifier("super"))?;
            self.define_variable(0, super_class_ident.line())?;

            self.variable(&class_ident, false)?;

            if super_class_ident.lexeme() == class_ident.lexeme() {
                return Err(CompilerErrorType::InheritanceFromSelf.at(&class_ident));
            }

            // TODO I think  we should create a new helper write_instruction()
            self.current_chunk().write_instruction(Instruction::Inherit, super_class_ident.line());

            self.current_class_mut().unwrap().has_superclass = true;
        };

        self.variable(&class_ident, false)?;
        self.consume_or_error(LeftBrace, CompilerErrorType::ExpectedLeftBrace("after class body"))?;

        loop {
            match self.peek().unwrap()? {
                RightBrace | Eof => break,
                _ => {
                    self.method()?;
                }
            }
        }

        let right_brace =
            self.consume_or_error(RightBrace, CompilerErrorType::ExpectedRightBrace("class body"))?;

        // pop the class name
        self.current_chunk().write_instruction(Instruction::Pop, class_ident.line());

        if self.classes.pop().unwrap().has_superclass {
            self.end_scope(right_brace.line());
        }

        Ok(())
    }

    fn method(&mut self) -> Result<()> {
        let identifier =
            self.consume_or_error(Identifier, CompilerErrorType::ExpectedMethodName)?;
        let constant_index = self.add_identifier_constant(&identifier)?;
        let function_type = if identifier.lexeme() == "init" {
            FunctionType::Initializer
        } else {
            FunctionType::Method
        };
        self.function(function_type, identifier.lexeme())?;
        self.current_chunk()
            .write_instruction(Instruction::Method { constant_index }, identifier.line());
        Ok(())
    }

    fn fun_declaration(&mut self) -> Result<()> {
        let (global, token) = self.parse_variable(CompilerErrorType::ExpectedFunctionName)?;
        self.mark_initialized();
        self.function(FunctionType::Function, token.lexeme())?;
        self.define_variable(global, token.line())?;
        Ok(())
    }

    fn parameter_list(&mut self) -> Result<Arity> {
        let mut arity = Arity(0);
        if self.peek().unwrap()? != RightParen {
            loop {
                let next_param = self.peek_token().unwrap()?;
                arity.0 = arity
                    .0
                    .checked_add(1)
                    .ok_or_else(|| CompilerErrorType::TooManyParameters.at(next_param))?;

                let (constant_index, token) =
                    self.parse_variable(CompilerErrorType::ExpectedParameterName)?;
                self.define_variable(constant_index, token.line())?;

                if self.consume(Comma)?.is_err() {
                    break;
                }
            }
        }

        self.consume_or_error(RightParen, CompilerErrorType::ExpectedRightParen("parameters"))?;

        Ok(arity)
    }

    fn function(&mut self, ty: FunctionType, name: &'a str) -> Result<()> {
        let function_token = self
            .consume_or_error(LeftParen, CompilerErrorType::ExpectedLeftParen("function name"))?;

        self.functions.push(FunctionToCompile::new(
            ty,
            Function::new(Arity(0), name, self.interner),
            self.current_scope().next_depth(),
        ));

        let parameter_list_errors = match self.parameter_list() {
            Ok(arity) => {
                self.current_function_mut().arity = arity;
                CompilerErrors(Vec::new())
            }
            Err(e) => {
                // If there's an error in the parameter list, synchronize so that we still parse
                // the function body correctly (instead of getting errors like "Expected left brace")
                self.synchronize_until(LeftBrace);
                e
            }
        };

        self.consume_or_error(
            LeftBrace,
            CompilerErrorType::ExpectedLeftBrace("before function body"),
        )?;

        let end_fun_line = self.block()?.line();

        // We don't call self.end_scope() here, as locals are popped by the VM when the function returns
        self.current_scope_mut().end_scope();

        self.emit_return(end_fun_line);

        let compiled_function = self.functions.pop().unwrap();

        log::debug!(
            "Compiled function [{:?}]: {:?}",
            compiled_function.function.name,
            compiled_function.function.chunk,
        );

        let constant_index =
            self.add_object_constant(compiled_function.function, &function_token)?;
        self.current_chunk().write_instruction(
            Instruction::Closure {
                constant_index,
                upvalue_count: compiled_function.upvalues.len() as u8,
            },
            function_token.line(),
        );

        self.current_chunk().write_bytes(
            compiled_function.upvalues.iter().flat_map(|u| match u {
                CompiledUpvalue::Local(index) => [0_u8, *index],
                CompiledUpvalue::Upvalue(index) => [1_u8, *index],
            }),
            function_token.line(),
        );

        if parameter_list_errors.0.is_empty() {
            Ok(())
        } else {
            Err(parameter_list_errors)
        }
    }

    fn call(&mut self, token: &Token) -> Result<()> {
        let arg_count = self.argument_list()?.into();
        self.current_chunk().write_instruction(Instruction::Call { arg_count }, token.line());
        Ok(())
    }

    fn dot(&mut self, _: &Token, can_assign: bool) -> Result<()> {
        let identifier =
            self.consume_or_error(Identifier, CompilerErrorType::ExpectedPropertyName)?;
        let constant_index = self.add_identifier_constant(&identifier)?;

        if can_assign && self.consume(Equal)?.is_ok() {
            self.expression()?;
            self.current_chunk()
                .write_instruction(Instruction::SetProperty { constant_index }, identifier.line());
        } else if self.consume(LeftParen)?.is_ok() {
            let arg_count = self.argument_list()?.into();
            self.current_chunk().write_instruction(
                Instruction::Invoke { constant_index, arg_count },
                identifier.line(),
            );
        } else {
            self.current_chunk()
                .write_instruction(Instruction::GetProperty { constant_index }, identifier.line());
        }

        Ok(())
    }

    fn argument_list(&mut self) -> Result<u8> {
        let mut arg_count = 0;

        if self.peek().unwrap()? != RightParen {
            loop {
                let arg_token = self.peek_token().unwrap()?.clone();
                self.expression()?;

                if arg_count == 255 {
                    return Err(CompilerErrorType::TooManyArguments.at(&arg_token));
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

    fn variable(&mut self, identifier: &Token<'a>, can_assign: bool) -> Result<()> {
        let (get_instr, set_instr) = if let Some(stack_slot) =
            Self::resolve_local(self.locals(), identifier)?
        {
            log::debug!("Resolved local {:?} @{stack_slot}", identifier);
            (Instruction::GetLocal { stack_slot }, Instruction::SetLocal { stack_slot })
        } else if let Some(upvalue_index) = Self::resolve_upvalue(&mut self.functions, identifier)?
        {
            log::debug!("Resolved upvalue {:?} @{upvalue_index}", identifier);
            (Instruction::GetUpvalue { upvalue_index }, Instruction::SetUpvalue { upvalue_index })
        } else {
            let constant_index = self.add_identifier_constant(identifier)?;
            log::debug!("Resolved global {:?} @{constant_index}", identifier);
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

    fn resolve_upvalue(
        functions: &mut [FunctionToCompile],
        identifier: &Token<'a>,
    ) -> Result<Option<u8>> {
        log::debug!("Trying to resolve upvalue for {:?}", identifier);

        let previous = match functions.iter_mut().rev().nth(1) {
            None => return Ok(None),
            Some(previous) => previous,
        };

        if let Some(index) = Self::resolve_local(&previous.locals, identifier)? {
            previous.locals[index as usize].is_captured = true;
            return Ok(Some(Self::add_upvalue(
                &mut functions.last_mut().unwrap().upvalues,
                CompiledUpvalue::Local(index),
                identifier,
            )?));
        }

        let functions_len = functions.len();
        if let Some(upvalue) =
            Self::resolve_upvalue(&mut functions[..functions_len - 1], identifier)?
        {
            return Ok(Some(Self::add_upvalue(
                &mut functions.last_mut().unwrap().upvalues,
                CompiledUpvalue::Upvalue(upvalue),
                identifier,
            )?));
        }

        Ok(None)
    }

    fn add_upvalue(
        upvalues: &mut Vec<CompiledUpvalue>,
        upvalue: CompiledUpvalue,
        identifier: &Token<'a>,
    ) -> Result<u8> {
        match upvalues.iter().position(|u| u == &upvalue) {
            Some(index) => Ok(index as u8),
            None => {
                if upvalues.len() == u8::MAX as usize + 1 {
                    return Err(CompilerErrorType::TooManyUpvalues.at(identifier));
                }

                upvalues.push(upvalue);
                Ok((upvalues.len() - 1) as u8)
            }
        }
    }

    fn resolve_local(locals: &[Local], identifier: &Token<'a>) -> Result<Option<u8>> {
        trace!("Trying to resolve local {:?} from {:?}", identifier.lexeme(), locals);
        locals
            .iter()
            .rev()
            .enumerate()
            .find_map(|(i, local)| {
                if local.name == identifier.lexeme() {
                    match local.depth {
                        Some(_) => Some(Ok((locals.len() - i - 1) as u8)),
                        None => {
                            Some(Err(CompilerErrorType::LocalInItsOwnInitializer.at(identifier)))
                        }
                    }
                } else {
                    None
                }
            })
            .transpose()
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

        let index = self.add_identifier_constant(&token)?;

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
            return Err(CompilerErrorType::VariableAlreadyInScope.at(name));
        }

        self.add_local(name)
    }

    fn add_local(&mut self, name: &Token<'a>) -> Result<()> {
        if self.num_locals() == self.functions.last().unwrap().locals.capacity() {
            return Err(CompilerErrorType::TooManyLocals.at(name));
        }
        log::debug!("Adding local {:?}", name);

        self.locals_mut().push(Local::new(name.lexeme(), None));
        Ok(())
    }

    fn mark_initialized(&mut self) {
        if self.current_scope().inside_local() {
            self.locals_mut().last_mut().unwrap().depth = Some(self.current_scope().depth());
        }
    }

    fn define_variable(&mut self, constant_index: u8, line: Line) -> Result<()> {
        if self.current_scope().inside_local() {
            log::debug!("Defining local {:?}", self.locals().last());
            self.mark_initialized();
            return Ok(());
        }
        log::debug!(
            "Defining global {constant_index} -> {:?}",
            self.current_chunk().constants()[constant_index as usize]
        );
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
        } else if let Ok(for_token) = self.consume(For)? {
            self.for_statement(for_token)
        } else if let Ok(return_token) = self.consume(Return)? {
            self.return_statement(&return_token)
        } else if self.consume(LeftBrace)?.is_ok() {
            self.current_scope_mut().begin_scope();
            let right_brace_line = self.block()?.line();
            self.end_scope(right_brace_line);
            Ok(())
        } else {
            self.expression_statement()
        }
    }

    fn return_statement(&mut self, return_token: &Token<'a>) -> Result<()> {
        if self.current_function_type() == FunctionType::Script {
            return Err(CompilerErrorType::ReturnOutsideFunction.at(return_token));
        }

        if let Ok(token) = self.consume(Semicolon)? {
            self.emit_return(token.line());
        } else {
            self.expression()?;
            let token = self.consume_or_error(Semicolon, CompilerErrorType::ExpectedSemicolon)?;
            self.current_chunk().write_instruction(Instruction::Return, token.line());

            if self.current_function_type() == FunctionType::Initializer {
                return Err(CompilerErrorType::InitializerCannotReturn.at(return_token));
            }
        }
        Ok(())
    }

    fn for_statement(&mut self, for_token: Token<'a>) -> Result<()> {
        // scopes the varibale that might have been declared in the initializer
        // Variables declared in the following block are scoped inside self.statement()
        self.current_scope_mut().begin_scope();

        self.consume_or_error(LeftParen, CompilerErrorType::ExpectedLeftParen("'for'"))?;

        if self.consume(Semicolon)?.is_ok() {
            // No initializer.
        } else if self.consume(Var)?.is_ok() {
            self.var_declaration()?;
        } else {
            self.expression_statement()?;
        }

        let mut loop_start = self.current_chunk().code().len();

        let mut exit_jump = None;
        if self.consume(Semicolon)?.is_err() {
            self.expression()?;
            self.consume_or_error(
                Semicolon,
                CompilerErrorType::ExpectedSemicolonAfter("loop condition"),
            )?;

            exit_jump = Some(self.write_jump(Instruction::JumpIfFalse(Jump(0)), for_token.line()));
            self.current_chunk().write_instruction(Instruction::Pop, for_token.line());
            // Condition
        }

        if self.consume(RightParen)?.is_err() {
            let body_jump = self.write_jump(Instruction::Jump(Jump(0)), for_token.line());
            let increment_start = self.current_chunk().code().len();
            self.expression()?;
            self.current_chunk().write_instruction(Instruction::Pop, for_token.line());
            let right_paren = self.consume_or_error(
                RightParen,
                CompilerErrorType::ExpectedRightParen("for clauses"),
            )?;

            self.write_loop(loop_start, &right_paren)?;
            loop_start = increment_start;
            self.patch_jump(body_jump, &right_paren)?;
        }

        self.statement()?;

        self.write_loop(loop_start, &for_token)?;

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump, &for_token)?;
            self.current_chunk().write_instruction(Instruction::Pop, for_token.line());
        }

        self.end_scope(for_token.line());

        Ok(())
    }

    fn while_statement(&mut self) -> Result<()> {
        let loop_start = self.current_chunk().code().len();
        self.consume_or_error(LeftParen, CompilerErrorType::ExpectedLeftParen("while"))?;
        self.expression()?;
        let token =
            self.consume_or_error(RightParen, CompilerErrorType::ExpectedRightParen("while"))?;
        let line = token.line();

        let exit_jump = self.write_jump(Instruction::JumpIfFalse(Jump(0)), line);
        self.current_chunk().write_instruction(Instruction::Pop, line);
        self.statement()?;

        self.write_loop(loop_start, &self.current_token.as_ref().unwrap().clone())?;

        self.patch_jump(exit_jump, &token)?;
        self.current_chunk().write_instruction(Instruction::Pop, line);

        Ok(())
    }

    fn write_loop(&mut self, loop_start: usize, token: &Token) -> Result<()> {
        let jump = Jump(
            (self.current_chunk().code().len() - loop_start + 1 + size_of::<Jump>())
                .try_into()
                .map_err(|_| CompilerErrorType::LoopBodyTooLarge.at(token))?,
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

        self.patch_jump(then_jump, &token)?;
        self.current_chunk().write_instruction(Instruction::Pop, token.line());

        if self.consume(Else)?.is_ok() {
            self.statement()?;
        }
        self.patch_jump(else_jump, &token)?;

        Ok(())
    }

    fn and(&mut self, token: Token<'a>) -> Result<()> {
        let end_jump = self.write_jump(Instruction::JumpIfFalse(Jump(0)), token.line());
        self.current_chunk().write_instruction(Instruction::Pop, token.line());
        self.parse_precedence(Precedence::And)?;
        self.patch_jump(end_jump, &token)?;
        Ok(())
    }

    fn or(&mut self, token: Token<'a>) -> Result<()> {
        let else_jump = self.write_jump(Instruction::JumpIfFalse(Jump(0)), token.line());
        let end_jump = self.write_jump(Instruction::Jump(Jump(0)), token.line());

        self.patch_jump(else_jump, &token)?;
        self.current_chunk().write_instruction(Instruction::Pop, token.line());

        self.parse_precedence(Precedence::Or)?;

        self.patch_jump(end_jump, &token)?;

        Ok(())
    }

    fn write_jump(&mut self, instruction: Instruction, line: Line) -> usize {
        self.current_chunk().write_instruction(instruction, line);
        self.current_chunk().code().len() - size_of::<Jump>()
    }

    fn patch_jump(&mut self, offset: usize, token: &Token<'a>) -> Result<()> {
        let jump = self.current_chunk().code().len() - offset - size_of::<Jump>();

        self.current_chunk().patch_jump(
            offset,
            Jump(jump.try_into().map_err(|_| CompilerErrorType::TooLargeJump.at(token))?),
        );
        Ok(())
    }

    fn end_scope(&mut self, line: Line) {
        let last_local_in_scope = self
            .locals()
            .iter()
            .rev()
            .enumerate()
            .find_map(|(index, local)| {
                (local.depth != Some(self.current_scope().depth())).then_some(index)
            })
            .unwrap_or(0);

        let num_locals = self.num_locals();
        self.locals_mut()
            // Remove all locals that where in the scope that just ended
            .drain(num_locals - last_local_in_scope..)
            // Reverse the order such that we pop locals from the stack in the correct order.
            // For non-captured locals this does not matter, but for captured locals we emit
            // CloseUpvalue instead of Pop, for which the VM expects the corresponding local
            // to be on top of the stack
            .rev()
            .collect_vec()
            // Divide them into chunks, using captured locals as the separator
            .split_inclusive(|local| local.is_captured)
            .for_each(|locals| {
                // split_inclusive puts the separator at the end, which is a local that was
                // captured, so close the upvalue instead of emitting a pop.
                // If there was no capture at all, we can just do another pop instead.
                if let Some(Local { is_captured: true, .. }) = locals.last() {
                    self.current_chunk()
                        .write_instruction(Instruction::PopN(locals.len() as u8 - 1), line);
                    self.current_chunk().write_instruction(Instruction::CloseUpvalue, line);
                } else {
                    self.current_chunk()
                        .write_instruction(Instruction::PopN(locals.len() as u8), line);
                }
            });

        self.current_scope_mut().end_scope();
    }

    fn block(&mut self) -> Result<Token> {
        let mut errors = CompilerErrors(Vec::new());

        loop {
            if matches!(self.peek().unwrap()?, RightBrace | Eof) {
                break;
            }
            match self.declaration() {
                Ok(()) => (),
                Err(mut e) => {
                    log::trace!("Hit error inside block: {:?}, syncing...", e);
                    self.synchronize();
                    errors.0.append(&mut e.0);
                }
            }
        }

        if errors.0.is_empty() {
            self.consume_or_error(RightBrace, CompilerErrorType::ExpectedRightBrace("block"))
        } else {
            Err(errors)
        }
    }

    fn print_statement(&mut self) -> Result<()> {
        self.expression()?;
        let line = self.consume_or_error(Semicolon, CompilerErrorType::ExpectedSemicolon)?.line();
        self.current_chunk().write_instruction(Instruction::Print, line);
        Ok(())
    }

    fn expression_statement(&mut self) -> Result<()> {
        self.expression()?;
        let line = self
            .consume_or_error(Semicolon, CompilerErrorType::ExpectedSemicolonAfter("expression"))?
            .line();
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

    fn this(&mut self, token: &Token<'a>) -> Result<()> {
        if self.classes.is_empty() {
            return Err(CompilerErrorType::ThisOutsideClass.at(token));
        }
        self.variable(token, false)
    }

    fn super_(&mut self, super_token: &Token<'a>) -> Result<()> {
        let current_class = self
            .current_class()
            .cloned()
            .ok_or_else(|| CompilerErrorType::SuperOutsideClass.at(super_token))?;
        if !current_class.has_superclass {
            return Err(CompilerErrorType::SuperclasslessClass.at(super_token));
        }

        self.consume_or_error(Dot, CompilerErrorType::ExpectedDotAfterSuper)?;
        let identifier =
            self.consume_or_error(Identifier, CompilerErrorType::ExpectedSuperclassMethodName)?;
        let constant_index = self.add_identifier_constant(&identifier)?;

        self.variable(&Token::synthetic_identifier("this"), false)?;

        if self.consume(LeftParen)?.is_ok() {
            let arg_count = self.argument_list()?.into();
            self.variable(super_token, false)?;
            self.current_chunk().write_instruction(
                Instruction::InvokeSuper { constant_index, arg_count },
                super_token.line(),
            );
        } else {
            self.variable(super_token, false)?;

            self.current_chunk()
                .write_instruction(Instruction::GetSuper { constant_index }, super_token.line());
        }

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

        let name = self.interner.intern(s);
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
            This => self.this(&token),
            Super => self.super_(&token),
            LeftParen => self.grouping(&token),
            Minus | Bang => self.unary(&token),
            Str => self.string(&token),
            Identifier => self.variable(&token, can_assign),
            _ => Err(CompilerErrorType::ExpectedExpression.at(&token)),
        }
    }

    fn advance_with_infix_rule(&mut self, can_assign: bool) -> Result<()> {
        let token = self.advance()?;
        trace!("Advancing with infix rule for {}", token);
        match token.ty() {
            Plus | Minus | Slash | Star | EqualEqual | BangEqual | Greater | GreaterEqual
            | Less | LessEqual => self.binary(&token),
            LeftParen => self.call(&token),
            Dot => self.dot(&token, can_assign),
            And => self.and(token),
            Or => self.or(token),
            _ => Err(CompilerErrorType::ExpectedExpression.at(&token)),
        }
    }

    fn token_precendence(&mut self, token_type: TokenType) -> Precedence {
        match token_type {
            Plus | Minus => Precedence::Term,
            Slash | Star => Precedence::Factor,
            EqualEqual | BangEqual => Precedence::Equality,
            Greater | GreaterEqual | Less | LessEqual => Precedence::Comparison,
            LeftParen | Dot => Precedence::Call,
            And => Precedence::And,
            Or => Precedence::Or,
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
            self.advance_with_infix_rule(can_assign)?;
        }

        if can_assign {
            // If the equal has not been consumed, we tried assigning to an invalid target,
            // so report that error here
            if let Ok(token) = self.consume(Equal)? {
                return Err(CompilerErrorType::InvalidAssignmentTarget.at(&token));
            }
        }

        Ok(())
    }

    fn synchronize(&mut self) {
        self.synchronize_until(None);
    }

    fn synchronize_until(&mut self, until: impl Into<Option<TokenType>> + Copy) {
        loop {
            log::trace!("Syncing... {:?}", self.peek_token());
            match self.peek_token() {
                Some(Ok(t)) => match t.ty() {
                    Semicolon => {
                        self.advance().unwrap();
                        return;
                    }
                    Class | Fun | Var | For | If | While | Print | Return | Eof => return,
                    ty => {
                        if until.into().is_some_and(|until| until == ty) {
                            return;
                        }
                        self.advance().unwrap();
                    }
                },
                Some(Err(_)) => {
                    self.advance().unwrap_err();
                }
                None => return,
            }
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
            Some(Ok(t)) => Ok(Err(t.clone())),
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
            Err(token) => Err(error.at(&token)),
        }
    }

    fn peek_token(&mut self) -> Option<Result<&Token<'a>>> {
        match self.token_stream.peek()? {
            Ok(token) => Some(Ok(token)),
            Err(scan_error) => Some(Err(scan_error.clone().into())),
        }
    }

    fn peek(&mut self) -> Option<Result<TokenType>> {
        self.peek_token().map(|t| t.map(|t| t.data.into()))
    }

    fn advance(&mut self) -> Result<Token<'a>> {
        match self.token_stream.next().unwrap() {
            Ok(token) => {
                self.current_token = Some(token.clone());
                Ok(token)
            }
            Err(scan_error) => Err(scan_error.into()),
        }
    }

    fn add_identifier_constant(&mut self, token: &Token<'a>) -> Result<u8> {
        let name = self.interner.intern(token.lexeme());
        match self.current_chunk().constants().iter().position(|v| v.equals_string(&name)) {
            Some(index) => Ok(index.try_into().unwrap()),
            None => self.add_object_constant(name, token),
        }
    }

    fn add_object_constant(&mut self, obj: impl Into<Object>, token: &Token<'a>) -> Result<u8> {
        let obj_ref: ObjectRef = self.heap.alloc(obj);
        self.add_constant(obj_ref, token)
    }

    fn add_constant(&mut self, value: impl Into<Value>, token: &Token<'a>) -> Result<u8> {
        self.current_chunk()
            .add_constant(value.into())
            .ok_or(CompilerErrorType::TooManyConstants.at(token))
    }
}
