extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::fs;

mod bstruct_ast;
mod bstruct_link;
mod bstruct_json;
use crate::bstruct_ast::{parse_bstruct_file, ParseError};
use crate::bstruct_json::compile_to_json;
use crate::bstruct_link::{BStructLinker, LinkError};

#[derive(Parser)]
#[grammar = "bstruct.pest"]
pub struct BstructParser;

fn main() {
  let inp = fs::read_to_string("example.bs").expect("unable to read file");

  let root_statements = match parse_bstruct_file(&inp) {
    Ok(it) => it,
    Err(err) =>  {
      match err {
        ParseError::PestError(it) => panic!("Error parsing: {}", it),
      }
    }
  };

  // for stmt in &root_statements {
  //   println!("{:?}", stmt);
  // }

  let mut linker = BStructLinker::new();
  match linker.link(&root_statements) {
    Ok(_) => {}
    Err(err) => {
      match err {
        LinkError::UnknownType(it) => panic!("Unknown type {}", it.as_str()),
        LinkError::EnumDoesNotExtendIntegerPrimitive(it) => panic!("Enum must extend integer primitive (found {})", it.as_str()),
        LinkError::CicrularReference(it) => panic!("Reference loop found: {}", it.iter().map(|v|v.value.as_str()).collect::<Vec<&str>>().join(" -> ")),
        LinkError::TODO(msg, id) => panic!("TODO: {} {}", msg, id.as_str()),
        LinkError::StructsCanOnlyExtendStructs { s, parent } => panic!("Structs can only extend other structs: {}: {}", s.value, parent.value),
        LinkError::AttemptToSpecializeNonTemplatedType(it) => panic!("Attempt to specialize type which does not have a template: {}", it.value),
      }
    }
  }

  for e in linker.get_enums() {
    println!("{:?}", e);
  }

  for s in linker.get_structs() {
    println!("{}", s.name.value);
    for member in &s.members {
      println!("  {:?}", member);
    }
  }

  let compiled = compile_to_json(linker.get_structs(), linker.get_enums());
  println!("{}", compiled);
}
