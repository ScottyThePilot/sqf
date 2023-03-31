pub mod database;
pub mod lexer;

use crate::{Statements, Statement, Expression, NularCommand, UnaryCommand, BinaryCommand};
use self::database::{Database, is_special_command};
use self::lexer::{Token, Span, Control, Operator};

use chumsky::prelude::*;
use chumsky::Stream;



#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct SourceLocation {
  pub line: usize,
  pub offset: usize
}

pub fn run(database: &Database, input: impl Into<String>) -> Result<Statements, ParserError> {
  let input = input.into();
  let mut tokens = self::lexer::run(&input).map_err(ParserError::LexingError)?;
  self::lexer::strip_comments(&mut tokens);
  run_for_tokens(database, input, tokens).map_err(ParserError::ParsingError)
}

pub fn run_for_tokens<I>(database: &Database, source: impl Into<String>, input: I) -> Result<Statements, Vec<Simple<Token>>>
where I: IntoIterator<Item = (Token, Span)> {
  let source = Source::new(source.into());
  let eoi = source.total_len..source.total_len + 1;
  let statements = parser(database, &source)
    .parse(Stream::from_iter(eoi, input.into_iter()))?;
  Ok(statements)
}

fn parser<'a>(database: &'a Database, source: &'a Source)
-> impl Parser<Token, Statements, Error = Simple<Token>> + 'a {
  statements(database, source).then_ignore(end())
}

fn statements<'a>(database: &'a Database, source: &'a Source)
-> impl Parser<Token, Statements, Error = Simple<Token>> + 'a {
  recursive(|statements| {
    let expression = recursive(|expression| {
      let value = select! { |span|
        Token::Number(number) => Expression::Number(number),
        Token::String(string) => Expression::String(string),
        // i know you can *technically* redefine true and false to be something else in SQF,
        // so this isn't *technically* correct, but if you're doing evil things like that,
        // you don't deserve parity
        Token::Identifier(id) if id.eq_ignore_ascii_case("true") => Expression::Boolean(true),
        Token::Identifier(id) if id.eq_ignore_ascii_case("false") => Expression::Boolean(false),
        Token::Identifier(id) if database.has_nular_command(&id) => {
          Expression::NularCommand(NularCommand { name: id }, source.locate(span))
        }
      };

      let array_open = just(Token::Control(Control::SquareBracketOpen));
      let array_close = just(Token::Control(Control::SquareBracketClose));
      let array = expression.clone()
        .separated_by(just(Token::Control(Control::Separator))).allow_trailing()
        .map_with_span(|value, span| Expression::Array(value, source.locate(span)))
        .delimited_by(array_open, array_close);

      let paren_open = just(Token::Control(Control::RoundBracketOpen));
      let paren_close = just(Token::Control(Control::RoundBracketClose));
      let parenthesized = expression.clone().delimited_by(paren_open, paren_close);

      let code_block_open = just(Token::Control(Control::CurlyBracketOpen));
      let code_block_close = just(Token::Control(Control::CurlyBracketClose));
      let code_block = statements.delimited_by(code_block_open, code_block_close)
        .map(Expression::Code);

      let variable = variable(database)
        .map_with_span(|value, span| Expression::Variable(value, source.locate(span)));
      let base = choice((value, array, parenthesized, code_block, variable));

      // Precedence 10 (Unary commands)
      let base = unary_command(database)
        .map_with_span(|value, span| (value, span))
        .repeated().then(base)
        .foldr(|(unary_command, span), expression| {
          Expression::UnaryCommand(unary_command, Box::new(expression), source.locate(span))
        });

      let locate = |value: BinaryCommand, span: Span| {
        (value, source.locate(span))
      };

      // Precedence 9 (Select)
      let base = apply_binary_command(base.boxed(), locate, {
        just(Token::Operator(Operator::Select)).to(BinaryCommand::Select)
      });

      // Precedence 8 (Exponent)
      let base = apply_binary_command(base.boxed(), locate, {
        just(Token::Operator(Operator::Exp)).to(BinaryCommand::Exp)
      });

      // Precedence 7 (Multiply, Divide, Remainder, Modulo, ATAN2)
      let base = apply_binary_command(base.boxed(), locate, select! {
        Token::Operator(Operator::Mul) => BinaryCommand::Mul,
        Token::Operator(Operator::Div) => BinaryCommand::Div,
        Token::Operator(Operator::Rem) => BinaryCommand::Rem,
        Token::Identifier(id) if id.eq_ignore_ascii_case("mod") => BinaryCommand::Mod,
        Token::Identifier(id) if id.eq_ignore_ascii_case("atan2") => BinaryCommand::Atan2
      });

      // Precedence 6 (Add, Subtract, Max, Min)
      let base = apply_binary_command(base.boxed(), locate, select! {
        Token::Operator(Operator::Add) => BinaryCommand::Add,
        Token::Operator(Operator::Sub) => BinaryCommand::Sub,
        Token::Identifier(id) if id.eq_ignore_ascii_case("max") => BinaryCommand::Max,
        Token::Identifier(id) if id.eq_ignore_ascii_case("min") => BinaryCommand::Min
      });

      // Precedence 5 (Else)
      let base = apply_binary_command(base.boxed(), locate, {
        keyword("else").to(BinaryCommand::Else)
      });

      // Precedence 4 (All other binary operators)
      let base = apply_binary_command(base.boxed(), locate, binary_command(database));

      // Precedence 3 (Equals, Not Equals, Greater, Less, GreaterEquals, LessEquals, Config Path)
      let base = apply_binary_command(base.boxed(), locate, select! {
        Token::Operator(Operator::Eq) => BinaryCommand::Eq,
        Token::Operator(Operator::NotEq) => BinaryCommand::NotEq,
        Token::Operator(Operator::Greater) => BinaryCommand::Greater,
        Token::Operator(Operator::Less) => BinaryCommand::Less,
        Token::Operator(Operator::GreaterEq) => BinaryCommand::GreaterEq,
        Token::Operator(Operator::LessEq) => BinaryCommand::LessEq,
        Token::Operator(Operator::ConfigPath) => BinaryCommand::ConfigPath
      });

      // Precedence 2 (And)
      let base = apply_binary_command(base.boxed(), locate, select! {
        Token::Operator(Operator::And) => BinaryCommand::And,
        Token::Identifier(id) if id.eq_ignore_ascii_case("and") => BinaryCommand::And
      });

      // Precedence 1 (Or)
      let base = apply_binary_command(base.boxed(), locate, select! {
        Token::Operator(Operator::Or) => BinaryCommand::Or,
        Token::Identifier(id) if id.eq_ignore_ascii_case("or") => BinaryCommand::Or
      });

      base
    });

    // assignment without terminator, optionally including `private`
    let assignment = variable(database)
      .then_ignore(just(Token::Operator(Operator::Assign)))
      .then(expression.clone());
    let assignment = keyword("private")
      .or_not().map(|v| v.is_some()).then(assignment)
      .map_with_span(|(local, (variable, expression)), span| match local {
        true => Statement::AssignLocal(variable, expression, source.locate(span)),
        false => Statement::AssignGlobal(variable, expression, source.locate(span))
      });
    assignment.or(expression.map(Statement::Expression))
      .separated_by(just(Token::Control(Control::Terminator))).allow_trailing()
      .map_with_span(|content, span| {
        Statements { content, source: source.extract(span) }
      })
  })
}

fn apply_binary_command(
  base: impl Parser<Token, Expression, Error = Simple<Token>> + Clone,
  locate: impl Fn(BinaryCommand, Span) -> (BinaryCommand, SourceLocation),
  command: impl Parser<Token, BinaryCommand, Error = Simple<Token>>
) -> impl Parser<Token, Expression, Error = Simple<Token>> {
  let command = command.map_with_span(locate);
  base.clone().then(command.then(base).repeated())
    .foldl(|expr1, ((command, location), expr2)| {
      Expression::BinaryCommand(command, Box::new(expr1), Box::new(expr2), location)
    })
}

/// Matches unary commands, including special ones
fn unary_command(database: &Database) -> impl Parser<Token, UnaryCommand, Error = Simple<Token>> + '_ {
  select! {
    Token::Operator(Operator::Add) => UnaryCommand::Plus,
    Token::Operator(Operator::Sub) => UnaryCommand::Minus,
    Token::Operator(Operator::Not) => UnaryCommand::Not,
    Token::Identifier(id) if database.has_unary_command(&id) => UnaryCommand::Named(id)
  }
}

/// Matches binary commands, not including special ones
fn binary_command(database: &Database) -> impl Parser<Token, BinaryCommand, Error = Simple<Token>> + '_ {
  select! {
    Token::Operator(Operator::Associate) => BinaryCommand::Associate,
    Token::Identifier(id) if database.has_binary_command(&id) => BinaryCommand::Named(id)
  }
}

/// Matches any identifier that is a valid variable name (any that is not considered a command name)
fn variable(database: &Database) -> impl Parser<Token, String, Error = Simple<Token>> + '_ {
  select!(Token::Identifier(id) if !database.has_command(&id) && !is_special_command(&id) => id)
}

/// Matches a specific keyword identifier
fn keyword(name: &'static str) -> impl Parser<Token, (), Error = Simple<Token>> {
  select!(Token::Identifier(id) if id.eq_ignore_ascii_case(name) => ())
}

struct Source {
  source: String,
  total_len: usize,
  lines: Vec<usize>
}

impl Source {
  fn new(source: String) -> Self {
    let mut total_len = 0;
    let mut lines = vec![0];
    for ch in source.chars() {
      total_len += 1;
      *lines.last_mut().unwrap() += 1;
      if ch == '\n' {
        lines.push(0);
      };
    };

    Source {
      source,
      total_len,
      lines
    }
  }

  fn locate(&self, span: Span) -> SourceLocation {
    let mut current_offset = span.start;
    for (i, &line_len) in self.lines.iter().enumerate() {
      if current_offset > line_len {
        current_offset -= line_len;
      } else {
        return SourceLocation { line: i + 1, offset: span.start };
      };
    };

    panic!()
  }

  fn extract(&self, span: Span) -> String {
    self.source.chars()
      .skip(span.start).take(span.end - span.start)
      .collect::<String>()
  }
}

// TODO: `std::error::Error` implementation
#[derive(Debug)]
pub enum ParserError {
  LexingError(Vec<Simple<char>>),
  ParsingError(Vec<Simple<Token>>)
}
