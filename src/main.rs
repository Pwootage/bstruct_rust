extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::fs;
use pest::iterators::{Pair};
use pest::Parser;

#[derive(Parser)]
#[grammar = "bstruct.pest"]
pub struct BstructParser;

fn main() {
  let inp = fs::read_to_string("example.bs")
    .expect("unable to read file");
  let parse = BstructParser::parse(Rule::start, &inp);
  match parse {
    Ok(successful_parse) => {
      for pair in successful_parse {
        print_parse(pair, 0);
      }
    }
    Err(err) => {
      println!("{}", err);
      println!("{:?}", err);
    }
  }
}

fn print_parse(pair: Pair<Rule>, nesting: usize) {
  let rule = pair.as_rule();
  println!("{}{:?}", " ".repeat(nesting), rule);
  for child in pair.into_inner() {
    print_parse(child, nesting + 2);
  }
}
