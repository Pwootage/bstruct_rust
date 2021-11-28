use pest::iterators::Pair;
use pest::{Parser, Span};

#[derive(Parser)]
#[grammar = "bstruct.pest"]
pub struct BstructParser;

#[derive(Clone, Debug)]
pub enum ASTRootStatement<'i> {
  Struct(ASTStruct<'i>),
  Enum(ASTEnum<'i>)
}

#[derive(Clone, Debug)]
pub struct ASTStruct<'i> {
  pub name: ASTIdentifier<'i>,
  pub template: Option<ASTTemplateDef<'i>>,
  pub extends: Option<ASTExtendsDecl<'i>>,
  pub size: Option<ASTInt>,
  pub vtable: Option<ASTInt>,
  pub members: Vec<ASTStructMember<'i>>,
}

#[derive(Clone, Debug)]
pub struct ASTStructMember<'i> {
  pub type_name: ASTType<'i>,
  pub name: ASTIdentifier<'i>,
  pub offset: Option<ASTInt>,
  pub bit: Option<ASTInt>,
  pub bit_length: Option<ASTInt>,
}

#[derive(Clone, Debug)]
pub struct ASTTemplateDef<'i> {
  pub templates: Vec<ASTIdentifier<'i>>
}

#[derive(Clone, Debug)]
pub struct ASTTemplateValues<'a> {
  pub type_names: Vec<ASTType<'a>>
}

#[derive(Clone, Debug)]
pub struct ASTExtendsDecl<'i> {
  pub extends: Vec<ASTIdentifier<'i>>
}

#[derive(Clone, Debug)]
pub struct ASTEnum<'i> {
  pub name: ASTIdentifier<'i>,
  pub values: Vec<ASTEnumValue<'i>>,
  pub ext: Option<ASTIdentifier<'i>>,
}

#[derive(Clone, Debug)]
pub struct ASTEnumValue<'i> {
  pub name: ASTIdentifier<'i>,
  pub value: Option<ASTInt>,
}

pub type ASTIdentifier<'i> = Span<'i>;

#[derive(Clone, Debug)]
pub enum ASTInt {
  Hex(usize),
  Decimal(usize),
  Binary(usize),
}

#[derive(Clone, Debug)]
pub struct ASTType<'i> {
  pub pointer: bool,
  pub name: ASTIdentifier<'i>,
  pub template: Option<ASTTemplateValues<'i>>,
  pub array_size: Option<ASTInt>,
}

// parse methods

pub fn parse_bstruct_file(inp: &str) -> Vec<ASTRootStatement> {
  let mut root_statements: Vec<ASTRootStatement> = vec![];
  let parse = BstructParser::parse(Rule::start, &inp);
  match parse {
    Ok(successful_parse) => {
      for pair in successful_parse {
        // print_parse(pair, 0);
        if let Some(res) = parse_root_statement(pair) {
          root_statements.push(res);
        }
      }
    }
    Err(err) => {
      println!("{}", err);
      println!("{:?}", err);
    }
  }
  root_statements
}

fn parse_root_statement(pair: Pair<Rule>) -> Option<ASTRootStatement> {
  let rule = pair.as_rule();
  match rule {
    Rule::struct_decl => {
      return Some(ASTRootStatement::Struct(parse_struct(pair)));
    }
    Rule::enum_decl => {
      return Some(ASTRootStatement::Enum(parse_enum(pair)));
    }
    Rule::EOI => None,
    _ => panic!("Didn't handle rule {:?}", pair),
  }
}

fn parse_enum(pair: Pair<Rule>) -> ASTEnum {
  let mut pairs = pair.into_inner();
  let name = pairs.next().unwrap().as_span();
  let mut values = vec![];
  let mut ext = None;

  for pair in pairs {
    match pair.as_rule() {
      Rule::identifier => ext = Some(pair.as_span()),
      Rule::enum_value => {
        let mut pairs = pair.into_inner();
        values.push(ASTEnumValue {
          name: pairs.next().unwrap().as_span(),
          value: pairs.next().map(|v| parse_int(v)),
        })
      }
      _ => panic!("Didn't handle rule {:?}", pair),
    }
  }

  ASTEnum { name, values, ext }
}

fn parse_struct(pair: Pair<Rule>) -> ASTStruct {
  let mut pairs = pair.into_inner();

  let name = pairs.next().unwrap().as_span();
  let mut template: Option<ASTTemplateDef> = None;
  let mut extends: Option<ASTExtendsDecl> = None;
  let mut size: Option<ASTInt> = None;
  let mut vtable: Option<ASTInt> = None;
  let mut members: Vec<ASTStructMember> = vec![];
  for pair in pairs {
    match pair.as_rule() {
      Rule::extends_decl => {
        extends = Some(parse_extends_decl(pair));
      }
      Rule::template_def => {
        template = Some(parse_template_def(pair));
      }
      Rule::size_decl => {
        size = Some(parse_int(pair.into_inner().next().unwrap()));
      }
      Rule::vtable_decl => {
        vtable = Some(parse_int(pair.into_inner().next().unwrap()));
      }
      Rule::struct_member => members.push(parse_struct_member(pair)),
      _ => panic!("Unhandled rule {:?}", pair),
    }
  }
  ASTStruct {
    name,
    template,
    extends,
    size,
    vtable,
    members,
  }
}

fn parse_struct_member(pair: Pair<Rule>) -> ASTStructMember {
  let mut pairs = pair.into_inner();
  let type_name = parse_typename(pairs.next().unwrap());
  let name = pairs.next().unwrap().as_span();
  let mut offset = None;
  let mut bit = None;
  let mut bit_length = None;

  if pairs.peek().map(|v|v.as_rule()) == Some(Rule::int) {
    offset = Some(parse_int(pairs.next().unwrap()));
  }

  if (pairs.peek().map(|v|v.as_rule())) == Some(Rule::bit_length) {
    let mut pairs = pairs.next().unwrap().into_inner();
    bit = Some(parse_int(pairs.next().unwrap()));
    bit_length = Some(parse_int(pairs.next().unwrap()));
  }


  ASTStructMember {
    type_name,
    name,
    offset,
    bit,
    bit_length,
  }
}

fn parse_typename(pair: Pair<Rule>) -> ASTType {
  let mut pairs = pair.into_inner();
  let mut pointer = false;
  if pairs.peek().unwrap().as_rule() == Rule::pointer_status {
    pointer = true;
    pairs.next();
  }
  let name = pairs.next().unwrap().as_span();
  let mut template_types: Vec<ASTType> = vec![];
  let mut array_size = None;

  // Everything from here on is optional

  if pairs.peek().map(|v| v.as_rule()) == Some(Rule::template_values) {
    for pair in pairs.next().unwrap().into_inner() {
      template_types.push(parse_typename(pair));
    }
  }

  if pairs.peek().map(|v| v.as_rule()) == Some(Rule::array_size) {
    let mut pairs = pairs.next().unwrap().into_inner();
    array_size = Some(parse_int(pairs.next().unwrap()));
  }

  let template = if template_types.is_empty() {
    None
  } else {
    Some(ASTTemplateValues {
      type_names: template_types,
    })
  };

  ASTType {
    pointer,
    name,
    template,
    array_size,
  }
}

fn parse_int(pair: Pair<Rule>) -> ASTInt {
  let pair = pair.into_inner().next().unwrap();
  let s = pair.as_span().as_str();
  match pair.as_rule() {
    Rule::hex_int => {
      let without_prefix = &s[2..];
      ASTInt::Hex(usize::from_str_radix(without_prefix, 16).unwrap())
    }
    Rule::binary_int => {
      let without_prefix = &s[2..];
      ASTInt::Binary(usize::from_str_radix(without_prefix, 2).unwrap())
    }
    Rule::decimal_int => ASTInt::Decimal(usize::from_str_radix(s, 10).unwrap()),
    _ => panic!("Unhandled rule {:?}", pair),
  }
}

fn parse_template_def(pair: Pair<Rule>) -> ASTTemplateDef {
  let mut templates = vec![];

  for pair in pair.into_inner() {
    templates.push(pair.as_span())
  }

  ASTTemplateDef { templates }
}

fn parse_extends_decl(pair: Pair<Rule>) -> ASTExtendsDecl {
  let pairs = pair.into_inner();
  let mut extends = vec![];

  for pair in pairs {
    extends.push(pair.as_span());
  }

  ASTExtendsDecl { extends }
}