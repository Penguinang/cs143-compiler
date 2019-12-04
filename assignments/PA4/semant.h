#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include <map>
using std::map;
using std::pair;
using std::make_pair;

#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

template<typename SymTbl>
class ScopeGuard {
  SymTbl &tbl;
public:
  ScopeGuard(SymTbl &tbl) : tbl(tbl) { tbl.enterscope(); }
  ~ScopeGuard() { tbl.exitscope(); }
};

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;

  Classes basic_classes;
  Classes user_classes;
  map<Symbol, Class_> classMap;

  enum NameType {
    CLASS_NAME,
    METHOD_NAME,
    ATTRIBUTE_NAME,
    FORMAL_NAME,
    LET_NAME,
    CASE_NAME,
    SELF_NAME // self
  };

  /**
   * Object name, Class name, Method name
   */
  SymbolTable<Symbol, pair<NameType, tree_node*>> symbolTable;
  void addid(Symbol symbol, NameType nameType, tree_node *node) {
    symbolTable.addid(symbol, new pair<NameType, tree_node*>(nameType, node));
  }

  Symbol getidtype(NameType type, tree_node *node) {
    assert(type == ATTRIBUTE_NAME || type == FORMAL_NAME || type == LET_NAME || type == CASE_NAME);
    assert(node);

    switch (type)
    {
    case ATTRIBUTE_NAME:
      return dynamic_cast<attr_class*>(node)->get_type_decl();
    case FORMAL_NAME:
      return dynamic_cast<formal_class*>(node)->get_type_decl();
    case LET_NAME:
      return dynamic_cast<let_class*>(node)->get_type_decl();
    case CASE_NAME:
      return dynamic_cast<branch_class*>(node)->get_type_decl();
    default:
      assert("wrong identifier type" && false);
    }
  }
  bool isBaseClass(Symbol typeBase, Symbol typeDerv);
  bool isTypeConvertable(Symbol source, Symbol target) {
    return source == target || isBaseClass(target, source);
  }
  Symbol base(Symbol derv) {
    return dynamic_cast<class__class*>(classMap[derv])->get_parent();
  }
  tree_node *findBaseClassMethod(Symbol dervClass, Symbol methodName);
  Symbol getFirstCommonBase(Symbol classA, Symbol classB);
  
  
  using Scope = ScopeGuard<decltype(symbolTable)>;
  Scope globalScope;

  /**
   * check inheritance relationship
   * 1. no cycle
   * 2. no inheritance from Int, Bool, String
   * 3. no inheritance from undeclared class
   */
  bool checkInheritance();

  void traverseMethodSignature(Class_ ClassNode, Class_);
  void traverseClass(Class_ ClassNode, Class_, bool basic_class);
  void traverseFeature(Feature FeatureNode, Class_ ClassNode, bool basic_class);
  void traverseAttribute(Feature AttrNode, Class_ ClassNode, bool basic_class);
  void traverseMethod(Feature MethodNode, Class_ ClassNode, bool basic_class);
  void traverseBranch(Case CaseNode, Class_ ClassNode);

  void traverseExpression(Expression ExpressionNode, Class_ ClassNode);
  void traverseAssignExp(assign_class *assign_exp, Class_ ClassNode);
  void traverseStcDispatchExp(static_dispatch_class *assign_exp, Class_ ClassNode);
  void traverseDispatchExp(dispatch_class *assign_exp, Class_ ClassNode);
  void traverseCondExp(cond_class *exp, Class_ ClassNode);
  void traverseLoopExp(loop_class *exp, Class_ ClassNode);
  void traverseTypcaseExp(typcase_class *exp, Class_ ClassNode);
  void traverseBlockExp(block_class *exp, Class_ ClassNode);
  void traverseLetExp(let_class *exp, Class_ ClassNode);
  void traversePlusExp(plus_class *exp, Class_ ClassNode);
  void traverseSubExp(sub_class *exp, Class_ ClassNode);
  void traverseMulExp(mul_class *exp, Class_ ClassNode);
  void traverseDivideExp(divide_class *exp, Class_ ClassNode);
  void traverseNegExp(neg_class *exp, Class_ ClassNode);
  void traverseLtExp(lt_class *exp, Class_ ClassNode);
  void traverseEqExp(eq_class *exp, Class_ ClassNode);
  void traverseLeqExp(leq_class *exp, Class_ ClassNode);
  void traverseCompExp(comp_class *exp, Class_ ClassNode);
  void traverseIntConstExp(int_const_class *exp, Class_ ClassNode);
  void traverseBoolConstExp(bool_const_class *exp, Class_ ClassNode);
  void traverseStrConstExp(string_const_class *exp, Class_ ClassNode);
  void traverseNewExp(new__class *exp, Class_ ClassNode);
  void traverseIsvoidExp(isvoid_class *exp, Class_ ClassNode);
  void traverseNoexpExp(no_expr_class *exp, Class_ ClassNode);
  void traverseObjectExp(object_class *exp, Class_ ClassNode);

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};


#endif

