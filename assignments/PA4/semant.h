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
    CASE_NAME
  };

  /**
   * Object name, Class name, Method name
   */
  SymbolTable<Symbol, pair<NameType, tree_node*>> symbolTable;
  void addid(Symbol symbol, NameType nameType, tree_node *node) {
    symbolTable.addid(symbol, new pair<NameType, tree_node*>(nameType, node));
  }
  
  using Scope = ScopeGuard<decltype(symbolTable)>;
  Scope globalScope;

  /**
   * check inheritance relationship
   * 1. no cycle
   * 2. no inheritance from Int, Bool, String
   * 3. no inheritance from undeclared class
   */
  bool checkInheritance();

  void traverseClass(Class_ ClassNode);
  void traverseFeature(Feature FeatureNode);
  void traverseAttribute(Feature AttrNode);
  void traverseMethod(Feature MethodNode);
  void traverseBranch(Case CaseNode);

  void traverseExpression(Expression ExpressionNode);
  void traverseAssignExp(assign_class *assign_exp);
  void traverseStcDispatchExp(static_dispatch_class *assign_exp);
  void traverseDispatchExp(dispatch_class *assign_exp);
  void traverseCondExp(cond_class *exp);
  void traverseLoopExp(loop_class *exp);
  void traverseTypcaseExp(typcase_class *exp);
  void traverseBlockExp(block_class *exp);
  void traverseLetExp(let_class *exp);
  void traversePlusExp(plus_class *exp);
  void traverseSubExp(sub_class *exp);
  void traverseMulExp(mul_class *exp);
  void traverseDivideExp(divide_class *exp);
  void traverseNegExp(neg_class *exp);
  void traverseLtExp(lt_class *exp);
  void traverseEqExp(eq_class *exp);
  void traverseLeqExp(leq_class *exp);
  void traverseCompExp(comp_class *exp);
  void traverseIntConstExp(int_const_class *exp);
  void traverseBoolConstExp(bool_const_class *exp);
  void traverseStrConstExp(string_const_class *exp);
  void traverseNewExp(new__class *exp);
  void traverseIsvoidExp(isvoid_class *exp);
  void traverseNoexpExp(no_expr_class *exp);
  void traverseObjectExp(object_class *exp);

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};


#endif

