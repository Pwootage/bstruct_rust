extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::fs;

mod bstruct_ast;
mod bstruct_link;
use crate::bstruct_ast::{parse_bstruct_file, ParseError, ParseResult};
use crate::bstruct_link::{BStructLinker, LinkError, LinkResult};

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
      }
    }
  }

  for e in linker.get_enums() {
    println!("{:?}", e);
  }

  for s in linker.get_structs() {
    println!("{:?}", s);
  }
}
