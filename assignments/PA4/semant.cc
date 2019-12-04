

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string>
using std::string;
#include <stack>
using std::stack;
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr), 
    symbolTable(), /* enter global scope */ globalScope(symbolTable) {
    /* Fill this in */

    user_classes = classes;
    for(auto i = user_classes->first(); user_classes->more(i); i = user_classes->next(i)) {
        Class_ ClassNode = user_classes->nth(i);
        class__class *classNode = dynamic_cast<class__class*>(ClassNode);
        classMap[classNode->get_name()] = ClassNode;
        addid(classNode->get_name(), CLASS_NAME, ClassNode);
    }

    install_basic_classes();
    if(checkInheritance()) {
        for(auto i = basic_classes->first(); basic_classes->more(i); i = basic_classes->next(i)) {
            Class_ ClassNode = basic_classes->nth(i);
            traverseMethodSignature(ClassNode, ClassNode);
        }
        for(auto i = user_classes->first(); user_classes->more(i); i = user_classes->next(i)) {
            Class_ ClassNode = user_classes->nth(i);
            traverseMethodSignature(ClassNode, ClassNode);
        }
        for(auto i = basic_classes->first(); basic_classes->more(i); i = basic_classes->next(i)) {
            Class_ ClassNode = basic_classes->nth(i);
            traverseClass(ClassNode, ClassNode, true);
        }
        for(auto i = user_classes->first(); user_classes->more(i); i = user_classes->next(i)) {
            Class_ ClassNode = user_classes->nth(i);
            traverseClass(ClassNode, ClassNode, false);
        }
    }
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

    basic_classes = single_Classes(Object_class);
    basic_classes = append_Classes(basic_classes, single_Classes(IO_class));
    basic_classes = append_Classes(basic_classes, single_Classes(Int_class));
    basic_classes = append_Classes(basic_classes, single_Classes(Bool_class));
    basic_classes = append_Classes(basic_classes, single_Classes(Str_class));

    classMap[Object] = Object_class;
    classMap[IO] = IO_class;
    classMap[Int] = Int_class;
    classMap[Bool] = Bool_class;
    classMap[Str] = Str_class;

    // xxx these basic class node are not in ast root
    addid(Object, CLASS_NAME, Object_class);
    addid(IO, CLASS_NAME, IO_class);
    addid(Int, CLASS_NAME, Int_class);
    addid(Bool, CLASS_NAME, Bool_class);
    addid(Str, CLASS_NAME, Str_class);
}


bool ClassTable::checkInheritance() {
    for(auto class_pair : classMap) {
        auto classNode = dynamic_cast<class__class*>(class_pair.second);
        auto childSymbol = classNode->get_name();
        auto parentSymbol = classNode->get_parent();
        if(parentSymbol == Int || parentSymbol == Bool || parentSymbol == Str) {
            semant_error(classNode) << classNode->get_name() << ": inheriting from Int, Bool, String is forbidden" << endl;
            return false;
        }

        if(childSymbol != Object)
        while(parentSymbol != Object && parentSymbol != No_class) {
            if(classMap.find(parentSymbol) == classMap.end()) {
                semant_error(classNode) << classNode->get_name() << ": unknown parent class " << parentSymbol->get_string() << endl;
                return false;
            }
            if(parentSymbol == childSymbol) {
                semant_error(classNode) << classNode->get_name() << ": a inheritance cycle found" << endl;
                return false;
            }
            parentSymbol = dynamic_cast<class__class*>(classMap[parentSymbol])->get_parent();
        }
    }
    return true;    
}

void ClassTable::traverseMethodSignature(Class_ ClassNode, Class_) {
    class__class *classNode = dynamic_cast<class__class*>(ClassNode);
    Features features = classNode->get_features();
    for(int i = features->first(); features->more(i); i = features->next(i)) {
        Feature feature = features->nth(i);
        if(!dynamic_cast<method_class*>(feature))
            continue;
        
        method_class *methodNode = dynamic_cast<method_class*>(feature);

        // Class.methodName as symbol
        string compoundName = string(classNode->get_name()->get_string()) + "." + string(methodNode->get_name()->get_string());
        Symbol compoundSymbol = idtable.add_string(const_cast<char*>(compoundName.c_str()));
        addid(compoundSymbol, METHOD_NAME, methodNode);
    }
}

void ClassTable::traverseClass(Class_ ClassNode, Class_, bool basic_class) {
    Scope classScope(symbolTable);
    addid(self, SELF_NAME, ClassNode);
    addid(SELF_TYPE, CLASS_NAME, ClassNode);
    classMap[SELF_TYPE] = ClassNode;
    class__class *classNode = dynamic_cast<class__class*>(ClassNode);
    Features features = classNode->get_features();
    for(int i = features->first(); features->more(i); i = features->next(i)) {
        traverseFeature(features->nth(i), ClassNode, basic_class);
    }
    classMap.erase(SELF_TYPE);
}

void ClassTable::traverseFeature(Feature FeatureNode, Class_ ClassNode, bool basic_class) {
    if(dynamic_cast<attr_class*>(FeatureNode)) {
        traverseAttribute(dynamic_cast<attr_class*>(FeatureNode), ClassNode, basic_class);
    } else {
        traverseMethod(dynamic_cast<method_class*>(FeatureNode), ClassNode, basic_class);
    }
}

void ClassTable::traverseAttribute(Feature AttrNode, Class_ ClassNode, bool basic_class) {
    attr_class *attrNode = dynamic_cast<attr_class*>(AttrNode);
    if(!basic_class && classMap.find(attrNode->get_type_decl()) == classMap.end()) {
        semant_error(ClassNode->get_filename(), attrNode) << "undeclared type name " << attrNode->get_type_decl() << endl;
    } else {
        addid(attrNode->get_name(), ATTRIBUTE_NAME, attrNode);
    }
    traverseExpression(attrNode->get_init(), ClassNode);
    if(attrNode->get_init()->get_type() != No_type && !isTypeConvertable(attrNode->get_init()->get_type(), attrNode->get_type_decl())) {
        semant_error(ClassNode->get_filename(), attrNode) << "init expression  cannot cast to type " << attrNode->get_type_decl() << endl;
    }
}

void ClassTable::traverseMethod(Feature MethodNode, Class_ ClassNode, bool basic_class) {
    method_class *methodNode = dynamic_cast<method_class*>(MethodNode);
    Scope methodScope(symbolTable);
    if(classMap.find(methodNode->get_return_type()) == classMap.end()) {
        semant_error(ClassNode->get_filename(), methodNode) << "undeclared type name " << methodNode->get_return_type() 
            << " in definition of function " << methodNode->get_name() << endl;
    }
    // formals
    Formals formals = methodNode->get_formals();
    for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Formal FormalNode = formals->nth(i);
        formal_class *formalNode = dynamic_cast<formal_class*>(FormalNode);
        if(classMap.find(formalNode->get_type_decl()) == classMap.end()) {
            semant_error(ClassNode->get_filename(), formalNode) << "undeclared type name " << formalNode->get_type_decl() << endl;
        }
        addid(formalNode->get_name(), FORMAL_NAME, formalNode);
    }

    // body
    traverseExpression(methodNode->get_expr(), ClassNode);
}

void ClassTable::traverseBranch(Case CaseNode, Class_ ClassNode) {
    Scope caseScope(symbolTable);
    branch_class *caseNode = dynamic_cast<branch_class*>(CaseNode);
    if(classMap.find(caseNode->get_type_decl()) != classMap.end()) {
        semant_error(ClassNode->get_filename(), caseNode) << "undeclared type name " << caseNode->get_type_decl() << endl;
    }
    addid(caseNode->get_name(), CASE_NAME, caseNode);
    traverseExpression(caseNode->get_expr(), ClassNode);
}

void ClassTable::traverseExpression(Expression ExpressionNode, Class_ ClassNode) {
    if(dynamic_cast<assign_class*>(ExpressionNode)) {
        traverseAssignExp(dynamic_cast<assign_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<static_dispatch_class*>(ExpressionNode)) {
        traverseStcDispatchExp(dynamic_cast<static_dispatch_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<dispatch_class*>(ExpressionNode)) {
        traverseDispatchExp(dynamic_cast<dispatch_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<cond_class*>(ExpressionNode)) {
        traverseCondExp(dynamic_cast<cond_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<loop_class*>(ExpressionNode)) {
        traverseLoopExp(dynamic_cast<loop_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<typcase_class*>(ExpressionNode)) {
        traverseTypcaseExp(dynamic_cast<typcase_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<block_class*>(ExpressionNode)) {
        traverseBlockExp(dynamic_cast<block_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<let_class*>(ExpressionNode)) {
        traverseLetExp(dynamic_cast<let_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<plus_class*>(ExpressionNode)) {
        traversePlusExp(dynamic_cast<plus_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<sub_class*>(ExpressionNode)) {
        traverseSubExp(dynamic_cast<sub_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<mul_class*>(ExpressionNode)) {
        traverseMulExp(dynamic_cast<mul_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<divide_class*>(ExpressionNode)) {
        traverseDivideExp(dynamic_cast<divide_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<neg_class*>(ExpressionNode)) {
        traverseNegExp(dynamic_cast<neg_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<lt_class*>(ExpressionNode)) {
        traverseLtExp(dynamic_cast<lt_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<eq_class*>(ExpressionNode)) {
        traverseEqExp(dynamic_cast<eq_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<leq_class*>(ExpressionNode)) {
        traverseLeqExp(dynamic_cast<leq_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<comp_class*>(ExpressionNode)) {
        traverseCompExp(dynamic_cast<comp_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<int_const_class*>(ExpressionNode)) {
        traverseIntConstExp(dynamic_cast<int_const_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<bool_const_class*>(ExpressionNode)) {
        traverseBoolConstExp(dynamic_cast<bool_const_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<string_const_class*>(ExpressionNode)) {
        traverseStrConstExp(dynamic_cast<string_const_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<new__class*>(ExpressionNode)) {
        traverseNewExp(dynamic_cast<new__class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<isvoid_class*>(ExpressionNode)) {
        traverseIsvoidExp(dynamic_cast<isvoid_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<no_expr_class*>(ExpressionNode)) {
        traverseNoexpExp(dynamic_cast<no_expr_class*>(ExpressionNode), ClassNode);
    } else if(dynamic_cast<object_class*>(ExpressionNode)) {
        traverseObjectExp(dynamic_cast<object_class*>(ExpressionNode), ClassNode);
    } else {
        assert("unknown expression type" && false);
    }
    assert(ExpressionNode->get_type());
}

void ClassTable::traverseAssignExp(assign_class *exp, Class_ ClassNode) {
    auto *identifier = symbolTable.lookup(exp->get_name());
    if(!identifier) {
        semant_error(ClassNode->get_filename(), exp) << "undeclared identifier `" << exp->get_name() << "`" << endl;
    }
    traverseExpression(exp->get_expr(), ClassNode);
    if(identifier) {
        Symbol idtype = getidtype(identifier->first, identifier->second), exptype = exp->get_expr()->get_type();
        if(!isTypeConvertable(exptype, idtype)) {
            semant_error(ClassNode->get_filename(), exp) << "assignment of uncompitable type `" << idtype << "` and `" << exptype << "`" << endl;
        }
        exp->set_type(idtype);
    } else {
        exp->set_type(No_type);
    }
}
void ClassTable::traverseStcDispatchExp(static_dispatch_class *exp, Class_ ClassNode) {
    traverseExpression(exp->get_expr(), ClassNode);
    if(!isTypeConvertable(exp->get_expr()->get_type(), exp->get_type())) {
        semant_error(ClassNode->get_filename(), exp) << "cannot cast " << exp->get_expr()->get_type() << " to " << exp->get_type() << endl;
    }
    string compoundName = string(exp->get_type()->get_string()) + "." + string(exp->get_name()->get_string());
    Symbol compoundSymbol = idtable.add_string(const_cast<char*>(compoundName.c_str()));
    auto method = symbolTable.lookup(compoundSymbol);
    if(!method) {
        semant_error(ClassNode->get_filename(), exp) << "undefined method " << exp->get_name() << endl;
        exp->set_type(No_type);
        return;
    }
    auto methodNode = dynamic_cast<method_class*>(method->second);
    exp->set_type(methodNode->get_return_type());
    if(methodNode->get_formals()->len() != exp->get_actual()->len()) {
        semant_error(ClassNode->get_filename(), exp) << "provided parameter number is not correct" << endl;
        return;
    }

    for(int i = exp->get_actual()->first(); exp->get_actual()->more(i); i = exp->get_actual()->next(i)) {
        Expression actual = exp->get_actual()->nth(i);
        traverseExpression(actual, ClassNode);
        Formal formal = methodNode->get_formals()->nth(i);
        formal_class *formalNode = dynamic_cast<formal_class*>(formal);
        Symbol actual_type = actual->get_type(), formal_type = formalNode->get_type_decl();
        if(!isTypeConvertable(actual_type, formal_type)) {
            semant_error(ClassNode->get_filename(), exp) << i << "'th actual type cannot match formal type" << endl;
        }
    }
}
void ClassTable::traverseDispatchExp(dispatch_class *exp, Class_ ClassNode) {
    traverseExpression(exp->get_expr(), ClassNode);
    auto method = findBaseClassMethod(exp->get_expr()->get_type(), exp->get_name());
    if(!method) {
        semant_error(ClassNode->get_filename(), exp) << "undefined method " << exp->get_name() << endl;
        findBaseClassMethod(exp->get_expr()->get_type(), exp->get_name());
        exp->set_type(No_type);
        return;
    }
    auto methodNode = dynamic_cast<method_class*>(method);
    exp->set_type(methodNode->get_return_type());
    if(methodNode->get_formals()->len() != exp->get_actual()->len()) {
        semant_error(ClassNode->get_filename(), exp) << "provided parameter number is not correct" << endl;
        return;
    }

    for(int i = exp->get_actual()->first(); exp->get_actual()->more(i); i = exp->get_actual()->next(i)) {
        Expression actual = exp->get_actual()->nth(i);
        traverseExpression(actual, ClassNode);
        Formal formal = methodNode->get_formals()->nth(i);
        formal_class *formalNode = dynamic_cast<formal_class*>(formal);
        Symbol actual_type = actual->get_type(), formal_type = formalNode->get_type_decl();
        if(!isTypeConvertable(actual_type, formal_type)) {
            semant_error(ClassNode->get_filename(), exp) << i << "'th actual type cannot match formal type" << endl;
        }
    }
}
void ClassTable::traverseCondExp(cond_class *exp, Class_ ClassNode) {
    traverseExpression(exp->get_pred(), ClassNode);
    if(exp->get_pred()->get_type() != Bool) {
        semant_error(ClassNode->get_filename(), exp) << "conditional predication must be of bool type" << endl;
    }
    traverseExpression(exp->get_then_exp(), ClassNode);
    traverseExpression(exp->get_else_exp(), ClassNode);
    exp->set_type(getFirstCommonBase(exp->get_then_exp()->get_type(), exp->get_else_exp()->get_type()));
}
void ClassTable::traverseLoopExp(loop_class *exp, Class_ ClassNode) {
    traverseExpression(exp->get_pred(), ClassNode);
    if(exp->get_pred()->get_type() != Bool) {
        semant_error(ClassNode->get_filename(), exp) << "conditional predication must be of bool type" << endl;
    }
    traverseExpression(exp->get_body(), ClassNode);
    exp->set_type(Object);
}
void ClassTable::traverseTypcaseExp(typcase_class *exp, Class_ ClassNode) {
    traverseExpression(exp, ClassNode);
    Symbol returnType = nullptr;
    Cases cases = exp->get_cases();
    for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
        traverseBranch(cases->nth(i), ClassNode);
        auto branchNode = dynamic_cast<branch_class*>(cases->nth(i));
        auto expType = branchNode->get_expr()->get_type();
        if(!returnType) {
            returnType = expType;
        } else {
            returnType = getFirstCommonBase(returnType, expType);
        }
    }
    exp->set_type(returnType);
}
void ClassTable::traverseBlockExp(block_class *exp, Class_ ClassNode) {
    auto body = exp->get_body();
    for(int i = body->first(); body->more(i); i = body->next(i)) {
        traverseExpression(body->nth(i), ClassNode);
        exp->set_type(body->nth(i)->get_type());
    }
}
void ClassTable::traverseLetExp(let_class *exp, Class_ ClassNode) {
    Scope letScope(symbolTable);
    if(classMap.find(exp->get_type_decl()) == classMap.end()) {
        semant_error(ClassNode->get_filename(), exp) << "undeclared type name " << exp->get_type_decl() << endl;
    }
    addid(exp->get_identifier(), LET_NAME, exp);

    traverseExpression(exp->get_init(), ClassNode);
    if(exp->get_init()->get_type() != No_type && !isTypeConvertable(exp->get_init()->get_type(), exp->get_type_decl())) {
        semant_error(ClassNode->get_filename(), exp) << "init expression  cannot cast to type " << exp->get_type_decl() << endl;
    }
    traverseExpression(exp->get_body(), ClassNode);
    exp->set_type(exp->get_body()->get_type());
}
void ClassTable::traversePlusExp(plus_class *exp, Class_ ClassNode) {
    auto e1 = exp->get_e1(), e2 = exp->get_e2();
    traverseExpression(e1, ClassNode);
    traverseExpression(e2, ClassNode);
    if(e1->get_type() != Int || e2->get_type() != Int) {
        semant_error(ClassNode->get_filename(), exp) << "plus operand must be Int" << endl;
        traverseExpression(e1, ClassNode);
        traverseExpression(e2, ClassNode);
    }
    exp->set_type(Int);
}
void ClassTable::traverseSubExp(sub_class *exp, Class_ ClassNode) {
    auto e1 = exp->get_e1(), e2 = exp->get_e2();
    traverseExpression(e1, ClassNode);
    traverseExpression(e2, ClassNode);
    if(e1->get_type() != Int || e2->get_type() != Int) {
        semant_error(ClassNode->get_filename(), exp) << "sub operand must be Int" << endl;
    }
    exp->set_type(Int);
}
void ClassTable::traverseMulExp(mul_class *exp, Class_ ClassNode) {
    auto e1 = exp->get_e1(), e2 = exp->get_e2();
    traverseExpression(e1, ClassNode);
    traverseExpression(e2, ClassNode);
    if(e1->get_type() != Int || e2->get_type() != Int) {
        semant_error(ClassNode->get_filename(), exp) << "mul operand must be Int" << endl;
    }
    exp->set_type(Int);
}
void ClassTable::traverseDivideExp(divide_class *exp, Class_ ClassNode) {
    auto e1 = exp->get_e1(), e2 = exp->get_e2();
    traverseExpression(e1, ClassNode);
    traverseExpression(e2, ClassNode);
    if(e1->get_type() != Int || e2->get_type() != Int) {
        semant_error(ClassNode->get_filename(), exp) << "divide operand must be Int" << endl;
    }
    exp->set_type(Int);
}
void ClassTable::traverseNegExp(neg_class *exp, Class_ ClassNode) {
    auto e1 = exp->get_e1();
    traverseExpression(e1, ClassNode);
    if(e1->get_type() != Int) {
        semant_error(ClassNode->get_filename(), exp) << "neg operand must be Int" << endl;
    }
    exp->set_type(Int);
}
void ClassTable::traverseLtExp(lt_class *exp, Class_ ClassNode) {
    auto e1 = exp->get_e1(), e2 = exp->get_e2();
    traverseExpression(e1, ClassNode);
    traverseExpression(e2, ClassNode);
    if(e1->get_type() != Int || e2->get_type() != Int) {
        semant_error(ClassNode->get_filename(), exp) << "< operand must be Int" << endl;
    }
    exp->set_type(Bool);    
}
void ClassTable::traverseEqExp(eq_class *exp, Class_ ClassNode) {
    auto e1 = exp->get_e1(), e2 = exp->get_e2();
    traverseExpression(e1, ClassNode);
    traverseExpression(e2, ClassNode);
    if(e1->get_type() == Int || e1->get_type() == Bool || e1->get_type() == Str) {
        if(e2->get_type() != e1->get_type()) {
            semant_error(ClassNode->get_filename(), exp) << "operand of equality comparision of basic type must be same" << endl;
        }
    }
    exp->set_type(Bool);
}
void ClassTable::traverseLeqExp(leq_class *exp, Class_ ClassNode) {
    auto e1 = exp->get_e1(), e2 = exp->get_e2();
    traverseExpression(e1, ClassNode);
    traverseExpression(e2, ClassNode);
    if(e1->get_type() != Int || e2->get_type() != Int) {
        semant_error(ClassNode->get_filename(), exp) << "<= operand must be Int" << endl;
    }
    exp->set_type(Bool);    
}
void ClassTable::traverseCompExp(comp_class *exp, Class_ ClassNode) {
    auto e1 = exp->get_e1();
    traverseExpression(e1, ClassNode);
    if(e1->get_type() != Int) {
        semant_error(ClassNode->get_filename(), exp) << "neg operand must be Int" << endl;
    }
    exp->set_type(Int);
}
void ClassTable::traverseIntConstExp(int_const_class *exp, Class_ ClassNode) {
    exp->set_type(Int);
}
void ClassTable::traverseBoolConstExp(bool_const_class *exp, Class_ ClassNode) {
    exp->set_type(Bool);
}
void ClassTable::traverseStrConstExp(string_const_class *exp, Class_ ClassNode) {
    exp->set_type(Str);
}
void ClassTable::traverseNewExp(new__class *exp, Class_ ClassNode) {
    Symbol type = exp->get_type_name();
    if(classMap.find(type) == classMap.end()) {
        semant_error(ClassNode->get_filename(), exp) << "undeclared type name " << type << endl;
    }
    exp->set_type(type);
}
void ClassTable::traverseIsvoidExp(isvoid_class *exp, Class_ ClassNode) {
    traverseExpression(exp->get_e1(), ClassNode);
    exp->set_type(Bool);
}
void ClassTable::traverseNoexpExp(no_expr_class *exp, Class_ ClassNode) {
    exp->set_type(No_type);
}
void ClassTable::traverseObjectExp(object_class *exp, Class_ ClassNode) {
    auto name = exp->get_name();
    auto symbolEt = symbolTable.lookup(name);
    if(!symbolEt) {
        semant_error(ClassNode->get_filename(), exp) << "undeclared name " << name << endl;
        exp->set_type(No_type);
        return;
    }
    if(symbolEt->first != ATTRIBUTE_NAME && symbolEt->first != FORMAL_NAME &&
        symbolEt->first != LET_NAME && symbolEt->first != CASE_NAME && symbolEt->first != SELF_NAME) {
        semant_error(ClassNode->get_filename(), exp) << "expected a object name" << endl;
        exp->set_type(No_type);
    }

    if(symbolEt->first == LET_NAME) {
        auto node = dynamic_cast<let_class*>(symbolEt->second);
        exp->set_type(node->get_type_decl());
    }

    if(symbolEt->first == ATTRIBUTE_NAME) {
        auto node = dynamic_cast<attr_class*>(symbolEt->second);
        exp->set_type(node->get_type_decl());
    }
    
    if(symbolEt->first == FORMAL_NAME) {
        auto node = dynamic_cast<formal_class*>(symbolEt->second);
        exp->set_type(node->get_type_decl());
    }

    if(symbolEt->first == CASE_NAME) {
        auto node = dynamic_cast<branch_class*>(symbolEt->second);
        exp->set_type(node->get_type_decl());
    }

    if(symbolEt->first == SELF_NAME) {
        auto node = dynamic_cast<class__class*>(symbolEt->second);
        exp->set_type(node->get_name());
    }
}

bool ClassTable::isBaseClass(Symbol typeBase, Symbol typeDerv) {
    while(typeDerv != No_class) {
        typeDerv = dynamic_cast<class__class*>(classMap[typeDerv])->get_parent();
        if(typeDerv == typeBase) {
        return true;
        }
    }
    return false;
}

tree_node *ClassTable::findBaseClassMethod(Symbol dervClass, Symbol methodName) {
    for(Symbol baseClass = dervClass; baseClass != No_class; baseClass = base(baseClass)) {
      string compoundName = string(baseClass->get_string()) + "." + string(methodName->get_string());
      Symbol compoundSymbol = idtable.add_string(const_cast<char*>(compoundName.c_str()));
      auto method = symbolTable.lookup(compoundSymbol);
      if(method)
        return method->second;
    }
    return nullptr;
}

Symbol ClassTable::getFirstCommonBase(Symbol classA, Symbol classB) {
    stack<Symbol> ancestorA, ancestorB;
    for(Symbol ancestor = classA; ancestor != No_class; ancestor = base(ancestor)){
        ancestorA.push(ancestor);
    }
    for(Symbol ancestor = classB; ancestor != No_class; ancestor = base(ancestor)){
        ancestorB.push(ancestor);
    }
    Symbol lastBase = Object;
    while(!ancestorA.empty() && !ancestorB.empty() && ancestorA.top() == ancestorB.top()) {
        lastBase = ancestorA.top();
        ancestorA.pop();
        ancestorB.pop();
    }
    return lastBase;
}


////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error(ClassNode)                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}


