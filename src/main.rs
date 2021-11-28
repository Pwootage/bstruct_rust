extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::iterators::Pair;
use pest::Parser;
use std::fs;

mod bstruct_ast;
use bstruct_ast::{ASTRootStatement};
use crate::ASTRootStatement::{ASTEnum, ASTStruct};

#[derive(Parser)]
#[grammar = "bstruct.pest"]
pub struct BstructParser;

fn main() {
    let inp = fs::read_to_string("example.bs").expect("unable to read file");
    let parse = BstructParser::parse(Rule::start, &inp);
    match parse {
        Ok(successful_parse) => {
            for pair in successful_parse {
                // print_parse(pair, 0);
                let res = parse_root_statement(pair);
                println!("{:?}", res);
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

fn parse_root_statement(pair: Pair<Rule>) -> Option<ASTRootStatement> {
    let rule = pair.as_rule();
    match rule {
        Rule::struct_decl => {
            return  Some(ASTStruct {

            });
        }
        Rule::enum_decl => {
            return Some(ASTEnum {

            });
        }
        Rule::EOI => {
            return None;
        }
        default => {
            panic!("Didn't handle rule {:?}", rule);
        }
    }
}
