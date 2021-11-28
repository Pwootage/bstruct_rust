#[derive(Clone, Debug)]
pub enum ASTRootStatement {
  ASTStruct,
  ASTEnum
}

#[derive(Clone, Debug)]
pub struct ASTStruct {
  name: String,
  template: Option<ASTTemplateDef>,
  extends: Option<ASTExtendsDecl>,
  size: Option<u32>,
  vtable: Option<u32>,
  members: Vec<ASTStructMember>,
}

#[derive(Clone, Debug)]
pub struct ASTStructMember {

}

#[derive(Clone, Debug)]
pub struct ASTTemplateDef {

}

#[derive(Clone, Debug)]
pub struct ASTExtendsDecl {

}

#[derive(Clone, Debug)]
pub struct ASTEnum {

}