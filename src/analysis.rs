//! Contains utilities for extracting more complex structures from a parsed AST
//! such as if statements, switch statements or for loops.

use crate::{Expression, Statements, Statement, Scalar};

macro_rules! match_option {
  ($input:expr, $pattern:pat $(if $guard:expr)? => $output:expr $(,)?) => {
    match $input {
      $pattern $(if $guard)? => Some($output),
      _ => None
    }
  };
}

macro_rules! name_eq {
  ($command:expr, $name:expr) => {
    $command.to_str().eq_ignore_ascii_case($name)
  };
}

impl Statement {
  pub fn expression(&self) -> Option<&Expression> {
    match_option!(self, Statement::Expression(e) => e)
  }
}

impl Expression {
  pub fn code(&self) -> Option<&Statements> {
    match_option!(self, Expression::Code(s) => s)
  }

  pub fn string(&self) -> Option<&String> {
    match_option!(self, Expression::String(v, ..) => v)
  }

  pub fn number(&self) -> Option<Scalar<f32>> {
    match_option!(self, &Expression::Number(v, ..) => v)
  }

  pub fn boolean(&self) -> Option<bool> {
    match_option!(self, &Expression::Boolean(v, ..) => v)
  }

  pub fn array_vec(&self) -> Option<&Vec<Expression>> {
    match_option!(self, Expression::Array(a, ..) => a)
  }

  pub fn array<const N: usize>(&self) -> Option<&[Expression; N]> {
    self.array_vec().and_then(|slice| slice.as_slice().try_into().ok())
  }

  pub fn nular_command(&self, name: &str) -> Option<()> {
    match_option!(self, Expression::NularCommand(cmd, ..) if name_eq!(cmd, name) => ())
  }

  pub fn unary_command(&self, name: &str) -> Option<&Self> {
    match_option!(self, Expression::UnaryCommand(cmd, e, ..) if name_eq!(cmd, name) => e)
  }

  pub fn binary_command(&self, name: &str) -> Option<(&Self, &Self)> {
    match_option!(self, Expression::BinaryCommand(cmd, e1, e2, ..) if name_eq!(cmd, name) => (e1, e2))
  }

  pub fn variable(&self) -> Option<&String> {
    match_option!(self, Expression::Variable(v, ..) => v)
  }

  #[inline]
  pub fn if_construct(&self) -> Option<IfConstruct> {
    IfConstruct::extract(self)
  }

  #[inline]
  pub fn switch_construct(&self) -> Option<SwitchConstruct> {
    SwitchConstruct::extract(self)
  }
}

/// Matches any of the following from an [`Expression`]:
/// - <code>if [Expression] then { [Statements] }</code>
/// - <code>if [Expression] then { [Statements] } else { [Statements] }</code>
/// - <code>if [Expression] exitWith { [Statements] }</code>
#[derive(Debug)]
pub enum IfConstruct {
  IfThen {
    condition: Expression,
    then_body: Statements,
  },
  IfThenElse {
    condition: Expression,
    then_body: Statements,
    else_body: Statements
  },
  IfExitWith {
    condition: Expression,
    body: Statements
  }
}

impl IfConstruct {
  pub const fn condition(&self) -> &Expression {
    match self {
      IfConstruct::IfThen { condition, .. } => condition,
      IfConstruct::IfThenElse { condition, .. } => condition,
      IfConstruct::IfExitWith { condition, .. } => condition
    }
  }

  pub const fn body(&self) -> &Statements {
    match self {
      IfConstruct::IfThen { then_body, .. } => then_body,
      IfConstruct::IfThenElse { then_body, .. } => then_body,
      IfConstruct::IfExitWith { body, .. } => body
    }
  }

  pub fn extract(expression: &Expression) -> Option<Self> {
    if let Some((front, back)) = expression.binary_command("exitwith") {
      let condition = front.unary_command("if")?.clone();
      let body = back.code()?.clone();
      Some(IfConstruct::IfExitWith { condition, body })
    } else {
      let (front, back) = expression.binary_command("then")?;
      let condition = front.unary_command("if")?.clone();
      if let Some((front, back)) = back.binary_command("else") {
        let then_body = front.code()?.clone();
        let else_body = back.code()?.clone();
        Some(IfConstruct::IfThenElse { condition, then_body, else_body })
      } else {
        let then_body = back.code()?.clone();
        Some(IfConstruct::IfThen { condition, then_body })
      }
    }
  }
}

/// Matches the following from an [`Expression`]:
/// - <code>switch [Expression] do { [CaseConstructs][CaseConstruct] }</code>
#[derive(Debug)]
pub struct SwitchConstruct {
  pub inspectee: Expression,
  pub cases: Vec<CaseConstruct>
}

impl SwitchConstruct {
  pub fn extract(expression: &Expression) -> Option<Self> {
    let (front, back) = expression.binary_command("do")?;
    let inspectee = front.unary_command("switch")?;
    let body = back.code()?.content.as_slice();

    let cases = body.iter()
      .map(|statement| CaseConstruct::extract(statement.expression()?))
      .collect::<Option<Vec<CaseConstruct>>>()?;
    Some(SwitchConstruct {
      inspectee: inspectee.clone(),
      cases
    })
  }
}

/// Matches any of the following from an [`Expression`]:
/// - <code>case [Expression]: { [Statements] }</code>
/// - <code>case [Expression]</code>
/// - <code>default { [Statements] }</code>
#[derive(Debug)]
pub enum CaseConstruct {
  Case {
    pattern: Expression,
    body: Statements
  },
  CaseFallthrough {
    pattern: Expression
  },
  Default {
    body: Statements
  }
}

impl CaseConstruct {
  pub fn extract(expression: &Expression) -> Option<Self> {
    if let Some(body) = expression.unary_command("default") {
      let body = body.code()?.clone();
      Some(CaseConstruct::Default { body })
    } else if let Some(pattern) = expression.unary_command("case") {
      let pattern = pattern.clone();
      Some(CaseConstruct::CaseFallthrough { pattern })
    } else if let Some((case, body)) = expression.binary_command(":") {
      let pattern = case.unary_command("case")?.clone();
      let body = body.code()?.clone();
      Some(CaseConstruct::Case { pattern, body })
    } else {
      None
    }
  }
}

#[derive(Debug)]
pub struct WhileConstruct {
  pub condition: Statements,
  pub body: Statements
}

#[derive(Debug)]
pub enum ForConstruct {
  ForDo {
    begin: Statements,
    condition: Statements,
    step: Statements,
    body: Statements
  },
  ForFromToDo {
    variable: String,
    range_start: Scalar<f32>,
    range_end: Scalar<f32>,
    step: Option<Scalar<f32>>,
    body: Statements
  }
}

impl ForConstruct {
  pub fn extract(expression: &Expression) -> Option<Self> {
    let (front, body) = expression.binary_command("do")?;
    let body = body.code()?;
    if let Some(front) = front.unary_command("for") {
      let [begin, condition, step] = front.array()?;

      Some(ForConstruct::ForDo {
        begin: begin.code()?.clone(),
        condition: condition.code()?.clone(),
        step: step.code()?.clone(),
        body: body.clone()
      })
    } else {
      let (front, step) = front.binary_command("step")
        .map(|(front, step)| (front, Some(step)))
        .unwrap_or_else(|| (front, None));
      let (front, range_end) = front.binary_command("to")?;
      let (front, range_start) = front.binary_command("from")?;
      let variable = front.unary_command("for")?;

      Some(ForConstruct::ForFromToDo {
        variable: variable.string()?.clone(),
        range_start: range_start.number()?,
        range_end: range_end.number()?,
        step: match step {
          Some(step) => Some(step.number()?),
          None => None
        },
        body: body.clone()
      })
    }
  }
}
