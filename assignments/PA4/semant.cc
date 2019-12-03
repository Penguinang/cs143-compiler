

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
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
        for(auto i = user_classes->first(); user_classes->more(i); i = user_classes->next(i)) {
            Class_ ClassNode = user_classes->nth(i);
            traverseClass(ClassNode);
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

void ClassTable::traverseClass(Class_ ClassNode) {
    Scope classScope(symbolTable);
    class__class *classNode = dynamic_cast<class__class*>(ClassNode);
    Features features = classNode->get_features();
    for(int i = features->first(); features->more(i); i = features->next(i)) {
        traverseFeature(features->nth(i));
    }
}

void ClassTable::traverseFeature(Feature FeatureNode) {
    if(dynamic_cast<attr_class*>(FeatureNode)) {
        traverseAttribute(dynamic_cast<attr_class*>(FeatureNode));
    } else {
        traverseMethod(dynamic_cast<method_class*>(FeatureNode));
    }
}

void ClassTable::traverseAttribute(Feature AttrNode) {
    attr_class *attrNode = dynamic_cast<attr_class*>(AttrNode);
    if(classMap.find(attrNode->get_type_decl()) == classMap.end()) {
        semant_error() << "undeclared type name " << attrNode->get_type_decl() << endl;
    } else {
        addid(attrNode->get_name(), ATTRIBUTE_NAME, attrNode);
    }
}

void ClassTable::traverseMethod(Feature MethodNode) {
    method_class *methodNode = dynamic_cast<method_class*>(MethodNode);
    addid(methodNode->get_name(), METHOD_NAME, methodNode);

    Scope methodScope(symbolTable);
    if(classMap.find(methodNode->get_return_type()) == classMap.end()) {
        semant_error() << "undeclared type name " << methodNode->get_return_type() << endl;
    }
    // formals
    Formals formals = methodNode->get_formals();
    for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Formal FormalNode = formals->nth(i);
        formal_class *formalNode = dynamic_cast<formal_class*>(FormalNode);
        if(classMap.find(formalNode->get_type_decl()) == classMap.end()) {
            semant_error() << "undeclared type name " << formalNode->get_type_decl() << endl;
        }
        addid(formalNode->get_name(), FORMAL_NAME, formalNode);
    }

    // body
    traverseExpression(methodNode->get_expr());
}

void ClassTable::traverseBranch(Case CaseNode) {
    Scope caseScope(symbolTable);
    branch_class *caseNode = dynamic_cast<branch_class*>(CaseNode);
    if(classMap.find(caseNode->get_type_decl()) != classMap.end()) {
        semant_error() << "undeclared type name " << caseNode->get_type_decl() << endl;
    }
    addid(caseNode->get_name(), CASE_NAME, caseNode);
    traverseExpression(caseNode->get_expr());
}

void ClassTable::traverseExpression(Expression ExpressionNode) {
    if(dynamic_cast<assign_class*>(ExpressionNode)) {
        traverseAssignExp(dynamic_cast<assign_class*>(ExpressionNode));
    } else if(dynamic_cast<static_dispatch_class*>(ExpressionNode)) {
        traverseStcDispatchExp(dynamic_cast<static_dispatch_class*>(ExpressionNode));
    } else if(dynamic_cast<dispatch_class*>(ExpressionNode)) {
        traverseDispatchExp(dynamic_cast<dispatch_class*>(ExpressionNode));
    } else if(dynamic_cast<cond_class*>(ExpressionNode)) {
        traverseCondExp(dynamic_cast<cond_class*>(ExpressionNode));
    } else if(dynamic_cast<loop_class*>(ExpressionNode)) {
        traverseLoopExp(dynamic_cast<loop_class*>(ExpressionNode));
    } else if(dynamic_cast<typcase_class*>(ExpressionNode)) {
        traverseTypcaseExp(dynamic_cast<typcase_class*>(ExpressionNode));
    } else if(dynamic_cast<block_class*>(ExpressionNode)) {
        traverseBlockExp(dynamic_cast<block_class*>(ExpressionNode));
    } else if(dynamic_cast<let_class*>(ExpressionNode)) {
        traverseLetExp(dynamic_cast<let_class*>(ExpressionNode));
    } else if(dynamic_cast<plus_class*>(ExpressionNode)) {
        traversePlusExp(dynamic_cast<plus_class*>(ExpressionNode));
    } else if(dynamic_cast<sub_class*>(ExpressionNode)) {
        traverseSubExp(dynamic_cast<sub_class*>(ExpressionNode));
    } else if(dynamic_cast<mul_class*>(ExpressionNode)) {
        traverseMulExp(dynamic_cast<mul_class*>(ExpressionNode));
    } else if(dynamic_cast<divide_class*>(ExpressionNode)) {
        traverseDivideExp(dynamic_cast<divide_class*>(ExpressionNode));
    } else if(dynamic_cast<neg_class*>(ExpressionNode)) {
        traverseNegExp(dynamic_cast<neg_class*>(ExpressionNode));
    } else if(dynamic_cast<lt_class*>(ExpressionNode)) {
        traverseLtExp(dynamic_cast<lt_class*>(ExpressionNode));
    } else if(dynamic_cast<eq_class*>(ExpressionNode)) {
        traverseEqExp(dynamic_cast<eq_class*>(ExpressionNode));
    } else if(dynamic_cast<leq_class*>(ExpressionNode)) {
        traverseLeqExp(dynamic_cast<leq_class*>(ExpressionNode));
    } else if(dynamic_cast<comp_class*>(ExpressionNode)) {
        traverseCompExp(dynamic_cast<comp_class*>(ExpressionNode));
    } else if(dynamic_cast<int_const_class*>(ExpressionNode)) {
        traverseIntConstExp(dynamic_cast<int_const_class*>(ExpressionNode));
    } else if(dynamic_cast<bool_const_class*>(ExpressionNode)) {
        traverseBoolConstExp(dynamic_cast<bool_const_class*>(ExpressionNode));
    } else if(dynamic_cast<string_const_class*>(ExpressionNode)) {
        traverseStrConstExp(dynamic_cast<string_const_class*>(ExpressionNode));
    } else if(dynamic_cast<new__class*>(ExpressionNode)) {
        traverseNewExp(dynamic_cast<new__class*>(ExpressionNode));
    } else if(dynamic_cast<isvoid_class*>(ExpressionNode)) {
        traverseIsvoidExp(dynamic_cast<isvoid_class*>(ExpressionNode));
    } else if(dynamic_cast<no_expr_class*>(ExpressionNode)) {
        traverseNoexpExp(dynamic_cast<no_expr_class*>(ExpressionNode));
    } else if(dynamic_cast<object_class*>(ExpressionNode)) {
        traverseObjectExp(dynamic_cast<object_class*>(ExpressionNode));
    } else {
        assert("unknown expression type");
    }
}

void ClassTable::traverseAssignExp(assign_class *assign_exp) {
    
}
void ClassTable::traverseStcDispatchExp(static_dispatch_class *assign_exp) {

}
void ClassTable::traverseDispatchExp(dispatch_class *assign_exp) {

}
void ClassTable::traverseCondExp(cond_class *exp) {

}
void ClassTable::traverseLoopExp(loop_class *exp) {

}
void ClassTable::traverseTypcaseExp(typcase_class *exp) {
    Cases cases = exp->get_cases();
    for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
        traverseBranch(cases->nth(i));
    }
}
void ClassTable::traverseBlockExp(block_class *exp) {

}
void ClassTable::traverseLetExp(let_class *exp) {
    Scope letScope(symbolTable);
    if(classMap.find(exp->get_type_decl()) == classMap.end()) {
        semant_error() << "undeclared type name " << exp->get_type_decl() << endl;
    }
    addid(exp->get_identifier(), LET_NAME, exp);

    traverseExpression(exp->get_init());
    traverseExpression(exp->get_body());
}
void ClassTable::traversePlusExp(plus_class *exp) {

}
void ClassTable::traverseSubExp(sub_class *exp) {

}
void ClassTable::traverseMulExp(mul_class *exp) {

}
void ClassTable::traverseDivideExp(divide_class *exp) {

}
void ClassTable::traverseNegExp(neg_class *exp) {

}
void ClassTable::traverseLtExp(lt_class *exp) {

}
void ClassTable::traverseEqExp(eq_class *exp) {

}
void ClassTable::traverseLeqExp(leq_class *exp) {

}
void ClassTable::traverseCompExp(comp_class *exp) {

}
void ClassTable::traverseIntConstExp(int_const_class *exp) {

}
void ClassTable::traverseBoolConstExp(bool_const_class *exp) {

}
void ClassTable::traverseStrConstExp(string_const_class *exp) {

}
void ClassTable::traverseNewExp(new__class *exp) {

}
void ClassTable::traverseIsvoidExp(isvoid_class *exp) {

}
void ClassTable::traverseNoexpExp(no_expr_class *exp) {

}
void ClassTable::traverseObjectExp(object_class *exp) {

}


////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
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


