extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::fs;

mod bstruct_ast;
use crate::bstruct_ast::parse_bstruct_file;

#[derive(Parser)]
#[grammar = "bstruct.pest"]
pub struct BstructParser;

fn main() {
  let inp = fs::read_to_string("example.bs").expect("unable to read file");

  let root_statements = parse_bstruct_file(&inp);

  for stmt in root_statements {
    println!("{:?}", stmt);
  }
}
