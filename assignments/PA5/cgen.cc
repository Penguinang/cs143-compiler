
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include <stack>
using std::stack;

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream &str, char *s);
extern int cgen_debug;
static int curSpFpSize = 0; // by word

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol arg, arg2, Bool, concat, cool_abort, copy, Int, in_int, in_string, IO,
    length, Main, main_meth, No_class, No_type, Object, out_int, out_string,
    prim_slot, self, SELF_TYPE, Str, str_field, substr, type_name, val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void) {
    arg = idtable.add_string("arg");
    arg2 = idtable.add_string("arg2");
    Bool = idtable.add_string("Bool");
    concat = idtable.add_string("concat");
    cool_abort = idtable.add_string("abort");
    copy = idtable.add_string("copy");
    Int = idtable.add_string("Int");
    in_int = idtable.add_string("in_int");
    in_string = idtable.add_string("in_string");
    IO = idtable.add_string("IO");
    length = idtable.add_string("length");
    Main = idtable.add_string("Main");
    main_meth = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class = idtable.add_string("_no_class");
    No_type = idtable.add_string("_no_type");
    Object = idtable.add_string("Object");
    out_int = idtable.add_string("out_int");
    out_string = idtable.add_string("out_string");
    prim_slot = idtable.add_string("_prim_slot");
    self = idtable.add_string("self");
    SELF_TYPE = idtable.add_string("SELF_TYPE");
    Str = idtable.add_string("String");
    str_field = idtable.add_string("_str_field");
    substr = idtable.add_string("substr");
    type_name = idtable.add_string("type_name");
    val = idtable.add_string("_val");
}

static char *gc_init_names[] = {"_NoGC_Init", "_GenGC_Init", "_ScnGC_Init"};
static char *gc_collect_names[] = {"_NoGC_Collect", "_GenGC_Collect",
                                   "_ScnGC_Collect"};

//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);
IntEntryP intDefault;
StringEntryP stringDefault;

int curLabel = 1;

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) {
    // spim wants comments to start with '#'
    os << "# start of generated code\n";

    initialize_constants();
    CgenClassTable *codegen_classtable = new CgenClassTable(classes, os);

    os << "\n# end of generated code\n";
}

//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg,
                      ostream &s) {
    s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
      << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg,
                       ostream &s) {
    s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream &s) {
    s << LI << dest_reg << " " << val << endl;
}

static void emit_load_address(char *dest_reg, char *address, ostream &s) {
    s << LA << dest_reg << " " << address << endl;
}

static void emit_partial_load_address(char *dest_reg, ostream &s) {
    s << LA << dest_reg << " ";
}

static void emit_load_bool(char *dest, const BoolConst &b, ostream &s) {
    emit_partial_load_address(dest, s);
    b.code_ref(s);
    s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream &s) {
    emit_partial_load_address(dest, s);
    str->code_ref(s);
    s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream &s) {
    emit_partial_load_address(dest, s);
    i->code_ref(s);
    s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream &s) {
    s << MOVE << dest_reg << " " << source_reg << endl;
}

static void emit_neg(char *dest, char *src1, ostream &s) {
    s << NEG << dest << " " << src1 << endl;
}

static void emit_add(char *dest, char *src1, char *src2, ostream &s) {
    s << ADD << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addu(char *dest, char *src1, char *src2, ostream &s) {
    s << ADDU << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addiu(char *dest, char *src1, int imm, ostream &s) {
    s << ADDIU << dest << " " << src1 << " " << imm << endl;
}

static void emit_div(char *dest, char *src1, char *src2, ostream &s) {
    s << DIV << dest << " " << src1 << " " << src2 << endl;
}

static void emit_mul(char *dest, char *src1, char *src2, ostream &s) {
    s << MUL << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sub(char *dest, char *src1, char *src2, ostream &s) {
    s << SUB << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sll(char *dest, char *src1, int num, ostream &s) {
    s << SLL << dest << " " << src1 << " " << num << endl;
}

static void emit_jalr(char *dest, ostream &s) {
    s << JALR << "\t" << dest << endl;
}

static void emit_jal(char *address, ostream &s) { s << JAL << address << endl; }

static void emit_return(ostream &s) { s << RET << endl; }

static void emit_gc_assign(ostream &s) { s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream &s) {
    s << sym << DISPTAB_SUFFIX;
}

static void emit_init_ref(Symbol sym, ostream &s) {
    s << sym << CLASSINIT_SUFFIX;
}

static void emit_label_ref(int l, ostream &s) { s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream &s) {
    s << sym << PROTOBJ_SUFFIX;
}

static void emit_method_ref(Symbol classname, Symbol methodname, ostream &s) {
    s << classname << METHOD_SEP << methodname;
}

static void emit_label_def(int l, ostream &s) {
    emit_label_ref(l, s);
    s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s) {
    s << BEQZ << source << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s) {
    s << BEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s) {
    s << BNE << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s) {
    s << BLEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s) {
    s << BLT << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s) {
    s << BLT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s) {
    s << BGT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_branch(int l, ostream &s) {
    s << BRANCH;
    emit_label_ref(l, s);
    s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream &str) {
    emit_store(reg, 0, SP, str);
    emit_addiu(SP, SP, -4, str);
    curSpFpSize -= -1;
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream &s) {
    emit_load(dest, DEFAULT_OBJFIELDS, source, s);
}

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream &s) {
    emit_store(source, DEFAULT_OBJFIELDS, dest, s);
}

static void emit_test_collector(ostream &s) {
    emit_push(ACC, s);
    emit_move(ACC, SP, s);  // stack end
    emit_move(A1, ZERO, s); // allocate nothing
    s << JAL << gc_collect_names[cgen_Memmgr] << endl;
    emit_addiu(SP, SP, 4, s);
    curSpFpSize -= 1;
    emit_load(ACC, 0, SP, s);
}

static void emit_gc_check(char *source, ostream &s) {
    if (source != (char *)A1)
        emit_move(A1, source, s);
    s << JAL << "_gc_check" << endl;
}

inline void emit_xor(char const *dest, char const *source1, char const *source2,
                     ostream &s) {
    s << XOR << dest << " " << source1 << " " << source2 << endl;
}

inline void emit_condition(char const *source1, char const *source2,
                           ostream &s) {
    emit_xor(ACC, source1, source2, s);
}

inline void emit_load_protobj(Symbol sym, ostream &s) {
    emit_partial_load_address(ACC, s);
    emit_protobj_ref(sym, s);
    s << endl;
}

inline void emit_method_call(Symbol className, Symbol methodName, ostream &s) {
    s << JAL;
    emit_method_ref(className, methodName, s);
    s << endl;
}
inline void emit_init_call(Symbol className, ostream &s) {
    s << JAL;
    emit_init_ref(className, s);
    s << endl;
}
inline void emit_untracked_pop(ostream &s, int word = 1) {
    if (word >= 1)
        emit_addiu(SP, SP, WORD_SIZE * word, s);
}
// tracked pop
inline void emit_pop(ostream &s) {
    emit_untracked_pop(s);
    curSpFpSize -= 1;
}
inline void emit_pop(char *reg, ostream &s) {
    emit_pop(s);
    emit_load(reg, 0, SP, s);
}
inline void pseudo_pop() { curSpFpSize -= 1; }

inline void emit_static_new(Symbol className, ostream &s) {
    emit_load_protobj(className, s);
    emit_method_call(Object, copy, s);
    emit_init_call(className, s);
}

//! emit_dynamic_new: may modify T1 
inline void emit_dynamic_new(int classTag, ostream &s) {
    emit_load_address(T1, "class_objTab", s);
    emit_load(ACC, classTag * 2, T1, s);
    emit_method_call(Object, copy, s);
    emit_load_address(T1, "class_objTab", s);
    emit_load(T1, classTag * 2 + 1, T1, s);
    emit_jalr(T1, s);
}
//! emit_dynamic_new: may modify T1 
inline void emit_dynamic_new(char *reg, ostream &s) {
    emit_sll(reg, reg, 3, s);
    emit_load_address(T1, "class_objTab", s);
    emit_add(T1, reg, T1, s);
    StackUsage stack(s);
    stack.push(T1);

    emit_load(ACC, 0, T1, s);   // _protObj
    emit_method_call(Object, copy, s);
    emit_load(T1, 1, SP, s);
    emit_load(T1, 1, T1, s);    // _init
    emit_jalr(T1, s);
}

inline void emit_load_prim1(char *reg, ostream &s) {
    emit_load(reg, DEFAULT_OBJFIELDS, reg, s);
}
///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream &s) { s << STRCONST_PREFIX << index; }

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream &s, int stringclasstag) {
    IntEntryP lensym = inttable.add_int(len);

    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                          // label
      << WORD << stringclasstag << endl // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len + 4) / 4)
      << endl // size
      << WORD;

    /***** Add dispatch information for class String ******/
    emit_disptable_ref(Str, s);

    s << endl; // dispatch table
    s << WORD;
    lensym->code_ref(s);
    s << endl;                    // string length
    emit_string_constant(s, str); // ascii string
    s << ALIGN;                   // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream &s, int stringclasstag) {
    for (List<StringEntry> *l = tbl; l; l = l->tl())
        l->hd()->code_def(s, stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s) { s << INTCONST_PREFIX << index; }

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag) {
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                                           // label
      << WORD << intclasstag << endl                     // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl // object size
      << WORD;

    /***** Add dispatch information for class Int ******/
    emit_disptable_ref(Int, s);
    s << endl;                // dispatch table
    s << WORD << str << endl; // integer value
}

//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag) {
    for (List<IntEntry> *l = tbl; l; l = l->tl())
        l->hd()->code_def(s, intclasstag);
}

//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream &s) const { s << BOOLCONST_PREFIX << val; }

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream &s, int boolclasstag) {
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                                            // label
      << WORD << boolclasstag << endl                     // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl // object size
      << WORD;

    /***** Add dispatch information for class Bool ******/
    emit_disptable_ref(Bool, s);
    s << endl;                // dispatch table
    s << WORD << val << endl; // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data() {
    Symbol main = idtable.lookup_string(MAINNAME);
    Symbol string = idtable.lookup_string(STRINGNAME);
    Symbol integer = idtable.lookup_string(INTNAME);
    Symbol boolc = idtable.lookup_string(BOOLNAME);

    str << "\t.data\n" << ALIGN;
    //
    // The following global names must be defined first.
    //
    str << GLOBAL << CLASSNAMETAB << endl;
    str << GLOBAL;
    emit_protobj_ref(main, str);
    str << endl;
    str << GLOBAL;
    emit_protobj_ref(integer, str);
    str << endl;
    str << GLOBAL;
    emit_protobj_ref(string, str);
    str << endl;
    str << GLOBAL;
    falsebool.code_ref(str);
    str << endl;
    str << GLOBAL;
    truebool.code_ref(str);
    str << endl;
    str << GLOBAL << INTTAG << endl;
    str << GLOBAL << BOOLTAG << endl;
    str << GLOBAL << STRINGTAG << endl;

    //
    // We also need to know the tag of the Int, String, and Bool classes
    // during code generation.
    //
    str << INTTAG << LABEL << WORD << intclasstag << endl;
    str << BOOLTAG << LABEL << WORD << boolclasstag << endl;
    str << STRINGTAG << LABEL << WORD << stringclasstag << endl;
}

//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text() {
    str << GLOBAL << HEAP_START << endl
        << HEAP_START << LABEL << WORD << 0 << endl
        << "\t.text" << endl
        << GLOBAL;
    emit_init_ref(idtable.add_string("Main"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Int"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("String"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Bool"), str);
    str << endl << GLOBAL;
    emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"),
                    str);
    str << endl;
}

void CgenClassTable::code_bools(int boolclasstag) {
    falsebool.code_def(str, boolclasstag);
    truebool.code_def(str, boolclasstag);
}

void CgenClassTable::code_select_gc() {
    //
    // Generate GC choice constants (pointers to GC functions)
    //
    str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
    str << "_MemMgr_INITIALIZER:" << endl;
    str << WORD << gc_init_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
    str << "_MemMgr_COLLECTOR:" << endl;
    str << WORD << gc_collect_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_TEST" << endl;
    str << "_MemMgr_TEST:" << endl;
    str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}

//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants() {
    //
    // Add constants that are required by the code generator.
    //
    stringDefault = stringtable.add_string("");
    intDefault = inttable.add_string("0");


    stringtable.code_string_table(str, stringclasstag);
    inttable.code_string_table(str, intclasstag);
    code_bools(boolclasstag);
}

void CgenClassTable::code_class_nameTab() {
    str << CLASSNAMETAB << LABEL;
    stack<StringEntry*> classNames;
    for (auto l = nds; l; l = l->tl()) {
        auto nameEntry =
            stringtable.lookup_string(l->hd()->get_name()->get_string());
        classNames.push(nameEntry);
    }

    while(!classNames.empty()) {
        auto nameEntry = classNames.top();
        str << WORD;
        nameEntry->code_ref(str);
        str << endl;
        classNames.pop();
    }
}
void CgenClassTable::code_class_objTab() {
    str << CLASSOBJTAB << LABEL;
    stack<StringEntry*> classNames;
    for (auto l = nds; l; l = l->tl()) {
        auto nameEntry =
            stringtable.lookup_string(l->hd()->get_name()->get_string());
        classNames.push(nameEntry);
    }

    while(!classNames.empty()) {
        auto nameEntry = classNames.top();
        str << WORD;
        emit_protobj_ref(nameEntry, str);
        str << endl;
        str << WORD;
        emit_init_ref(nameEntry, str);
        str << endl;
        classNames.pop();
    }
}
void CgenClassTable::code_dispTab() {
    for (auto l = nds; l; l = l->tl()) {
        auto classNode = l->hd();
        emit_disptable_ref(classNode->get_name(), str);
        str << LABEL;
        code_class_dispTab(classNode);
    }
}
map<Symbol, pair<Symbol, size_t>> CgenClassTable::get_class_dispTab(CgenNode *classNode) {
    map<Symbol, pair<Symbol, size_t>> methodTable;
    if (classNode->get_parentnd()) {
        methodTable = std::move(get_class_dispTab(classNode->get_parentnd()));
    }
    auto features = classNode->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        if (auto methodNode = dynamic_cast<method_class *>(features->nth(i))) {
            auto methodName = methodNode->get_name();
            auto className = classNode->get_name();
            if(methodTable.find(methodName) == methodTable.end()) {
                methodTable[methodName] = {className, methodTable.size()};
            } else {
                int index = methodTable[methodName].second;
                methodTable[methodName] = {className, index};
            }
        }
    }
    return methodTable;
}
void CgenClassTable::code_class_dispTab(CgenNode *classNode) {
    auto methodTable = get_class_dispTab(classNode);
    vector<tuple<Symbol, Symbol, size_t>> sortedTable;
    sortedTable.reserve(methodTable.size());
    transform(methodTable.begin(), methodTable.end(), back_inserter(sortedTable), [](auto &mapedType){
        return tuple<Symbol, Symbol, size_t>{mapedType.first, mapedType.second.first, mapedType.second.second};
    });
    sort(sortedTable.begin(), sortedTable.end(), [](auto &lhs, auto &rhs){
        return get<2>(lhs) < get<2>(rhs);
    });
    for(auto &method : sortedTable) {
        str << WORD;
        emit_method_ref(get<1>(method), get<0>(method), str);
        str << endl;
    }
}

void CgenClassTable::code_protObjs() {
    for (auto l = nds; l; l = l->tl()) {
        auto classNode = l->hd();

        str << WORD << "-1" << endl;
        emit_protobj_ref(classNode->get_name(), str);
        str << LABEL                                      // label
            << WORD << classNode->get_classTag() << endl; // tag
        int attr_count = classNode->get_attr_count();
        str << WORD << (DEFAULT_OBJFIELDS + attr_count) << endl // size
            << WORD;
        emit_disptable_ref(classNode->get_name(), str);
        str << endl; // dispatch table

        code_attributes(classNode);
    }
}

void CgenClassTable::code_attributes(CgenNodeP classNode) {
    if(classNode->get_parentnd()) {
        code_attributes(classNode->get_parentnd());
    }
    auto features = classNode->get_features();
    for (int i = features->first(); features->more(i);
            i = features->next(i)) {
        if (auto attrNode = dynamic_cast<attr_class *>(features->nth(i))) {
            if (attrNode->get_type_decl() == Int) {
                str << WORD;
                intDefault->code_ref(str);
                str << endl;
            } else if (attrNode->get_type_decl() == Bool) {
                str << WORD;
                falsebool.code_ref(str);
                str << endl;
            } else if (attrNode->get_type_decl() == Str) {
                str << WORD;
                stringDefault->code_ref(str);
                str << endl;
            } else {
                str << WORD;
                str << 0;
                str << endl;
            }
        }
    }
        
}

void CgenClassTable::code_initializer() {
    for (auto l = nds; l; l = l->tl()) {
        auto classNode = l->hd();
        emit_init_ref(classNode->get_name(), str);
        str << LABEL;
        code_class_initializer(classNode);
    }
}

void CgenClassTable::code_class_initializer(CgenNode *classNode) {
    NameScope classScope(this);
    bindObjectName(self, SELF);
    auto features = classNode->get_features();
    int attrOffset = 0;
    bindClassAttrs(classNode, attrOffset);

    code_method_head(3);
    StackSizeTracker sizeTracker(curSpFpSize);

    ExpressionContext context = {this, classNode};
    emit_move(SELF, ACC, str);
    if (classNode->get_name() != Object)
        str << JAL << classNode->get_parent() << CLASSINIT_SUFFIX << endl;

    // todo delete me
    if(classNode->get_name() == idtable.add_string("Foo"))
        int breakpoint;
    int fieldOffset = DEFAULT_OBJFIELDS;
    if(classNode->get_parentnd()) {
        fieldOffset += classNode->get_parentnd()->get_attr_count();
    }
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        if (auto attrNode = dynamic_cast<attr_class *>(features->nth(i))) {
            auto initNode = attrNode->get_init();
            if (initNode->get_type()) {
                initNode->code(str, context);
                emit_store(ACC, fieldOffset, SELF, str);
            } else {
                // inited by prototype object
            }
            fieldOffset += 1;
        }
    }

    emit_move(ACC, SELF, str);
    code_method_tail(3, 0);
}
void CgenClassTable::code_methods() {
    for (auto l = nds; l; l = l->tl()) {
        auto classNode = l->hd();
        if (!classNode->basic())
            code_class_methods(classNode);
    }
}
void CgenClassTable::code_class_methods(CgenNode *classNode) {
    NameScope classScope(this);
    bindObjectName(self, SELF);
    auto features = classNode->get_features();

    int attrOffset = 0;
    bindClassAttrs(classNode, attrOffset);

    for (int i = features->first(); features->more(i); i = features->next(i)) {
        if (auto methodNode = dynamic_cast<method_class *>(features->nth(i))) {
            code_method(classNode, methodNode);
        }
    }
}

void CgenClassTable::code_method(CgenNode *classNode,
                                 method_class *methodNode) {
    emit_method_ref(classNode->get_name(), methodNode->get_name(), str);
    str << LABEL;
    code_method_head(3);

    /**

    --------
    param n
    --------
    fp
    -------- <- last sp
    s0
    --------
    ra
    -------- <- new fp

    -------- <- new sp

     */

    NameScope methodScope(this);
    auto formals = methodNode->formals;
    int formalOffset = 0;
    int formalLen = formals->len();
    int formalPos = formalLen + 2;
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        auto formal = formals->nth(i);
        bindObjectName(dynamic_cast<formal_class *>(formal)->name, FP,
                       formalPos--);
    }

    StackSizeTracker sizeTracker(curSpFpSize);
    emit_move(SELF, ACC, str);
    ExpressionContext context = {this, classNode};
    methodNode->get_expr()->code(str, context);
    code_method_tail(3, formalLen);
}

void CgenClassTable::code_method_head(int frameSizeWord) {
    int frameSizeByte = frameSizeWord * WORD_SIZE;
    // save registers
    emit_push(FP, str);
    emit_push(SELF, str);
    emit_push(RA, str);
    // update frame pointer
    emit_addiu(FP, SP, 4, str);
}
void CgenClassTable::code_method_tail(int frameSizeWord,
                                      int extraParam) {
    int frameSizeByte = frameSizeWord * WORD_SIZE;
    // load registers
    emit_pop(RA, str);
    emit_pop(SELF, str);
    emit_pop(FP, str);
    emit_untracked_pop(str, extraParam);
    emit_return(str);
}

void CgenClassTable::bindObjectName(Symbol name, char *reg, int offset) {
    names.addid(name, new MemoryName(reg, offset));
}
void CgenClassTable::bindObjectName(Symbol name, char *reg) {
    names.addid(name, new RegisterName(reg));
}
void CgenClassTable::bindClassAttrs(CgenNodeP classNode, int &attrOffset) {
    if(classNode->get_parentnd()) {
        bindClassAttrs(classNode->get_parentnd(), attrOffset);
    }
    auto features = classNode->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        if (auto attrNode = dynamic_cast<attr_class *>(features->nth(i))) {
            bindObjectName(attrNode->get_name(), SELF,
                           DEFAULT_OBJFIELDS + attrOffset++);
        }
    }
}
void CgenClassTable::getNameAddress(Symbol name, char *reg, ostream &s) {
    if (auto symbol = names.lookup(name)) {
        symbol->getAddress(reg, s);
    } else {
        cerr << "no name " << name << endl;
        assert(false);
    }
}
void CgenClassTable::setNameAddress(Symbol name, char *reg, ostream &s) {
    if (auto symbol = names.lookup(name)) {
        symbol->setAddress(reg, s);
    } else {
        cerr << "no name " << name << endl;
        assert(false);
    }
}
CgenNodeP CgenClassTable::getClassNode(Symbol className) {
    for (List<CgenNode> *l = nds; l; l = l->tl()) {
        if(l->hd()->get_name() == className) {
            return l->hd();
        }
    }
    return nullptr;
}


CgenClassTable::CgenClassTable(Classes classes, ostream &s)
    : nds(NULL), str(s) {
    stringclasstag =
        STRING_CLASS_TAG /* Change to your String class tag here */;
    intclasstag = INT_CLASS_TAG /* Change to your Int class tag here */;
    boolclasstag = BOOL_CLASS_TAG /* Change to your Bool class tag here */;

    enterscope();
    if (cgen_debug)
        cout << "Building CgenClassTable" << endl;
    install_basic_classes();
    install_classes(classes);
    build_inheritance_tree();

    code();
    exitscope();
}

void CgenClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    //
    // A few special class names are installed in the lookup table but not
    // the class list.  Thus, these classes exist, but are not part of the
    // inheritance hierarchy.
    // No_class serves as the parent of Object and the other special classes.
    // SELF_TYPE is the self class; it cannot be redefined or inherited.
    // prim_slot is a class known to the code generator.
    //
    addid(No_class,
          new CgenNode(class_(No_class, No_class, nil_Features(), filename),
                       Basic, this, NO_CLASS_TAG));
    addid(SELF_TYPE,
          new CgenNode(class_(SELF_TYPE, No_class, nil_Features(), filename),
                       Basic, this, NO_CLASS_TAG));
    addid(prim_slot,
          new CgenNode(class_(prim_slot, No_class, nil_Features(), filename),
                       Basic, this, NO_CLASS_TAG));

    //
    // The Object class has no parent class. Its methods are
    //        cool_abort() : Object    aborts the program
    //        type_name() : Str        returns a string representation of class
    //        name copy() : SELF_TYPE       returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.
    //
    install_class(new CgenNode(
        class_(Object, No_class,
               append_Features(
                   append_Features(
                       single_Features(method(cool_abort, nil_Formals(), Object,
                                              no_expr())),
                       single_Features(
                           method(type_name, nil_Formals(), Str, no_expr()))),
                   single_Features(
                       method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
               filename),
        Basic, this, OBJECT_CLASS_TAG));

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE          writes a string to the output
    //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
    //        in_string() : Str                    reads a string from the input
    //        in_int() : Int                         "   an int     "  "     "
    //
    install_class(new CgenNode(
        class_(
            IO, Object,
            append_Features(
                append_Features(
                    append_Features(
                        single_Features(method(out_string,
                                               single_Formals(formal(arg, Str)),
                                               SELF_TYPE, no_expr())),
                        single_Features(method(out_int,
                                               single_Formals(formal(arg, Int)),
                                               SELF_TYPE, no_expr()))),
                    single_Features(
                        method(in_string, nil_Formals(), Str, no_expr()))),
                single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
            filename),
        Basic, this, IO_CLASS_TAG));

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    install_class(new CgenNode(
        class_(Int, Object, single_Features(attr(val, prim_slot, no_expr())),
               filename),
        Basic, this, INT_CLASS_TAG));

    //
    // Bool also has only the "val" slot.
    //
    install_class(new CgenNode(
        class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),
               filename),
        Basic, this, BOOL_CLASS_TAG));

    //
    // The class Str has a number of slots and operations:
    //       val                                  ???
    //       str_field                            the string itself
    //       length() : Int                       length of the string
    //       concat(arg: Str) : Str               string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring
    //
    install_class(new CgenNode(
        class_(Str, Object,
               append_Features(
                   append_Features(
                       append_Features(
                           append_Features(
                               single_Features(attr(val, Int, no_expr())),
                               single_Features(
                                   attr(str_field, prim_slot, no_expr()))),
                           single_Features(
                               method(length, nil_Formals(), Int, no_expr()))),
                       single_Features(method(concat,
                                              single_Formals(formal(arg, Str)),
                                              Str, no_expr()))),
                   single_Features(
                       method(substr,
                              append_Formals(single_Formals(formal(arg, Int)),
                                             single_Formals(formal(arg2, Int))),
                              Str, no_expr()))),
               filename),
        Basic, this, STRING_CLASS_TAG));
}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd) {
    Symbol name = nd->get_name();

    if (probe(name)) {
        return;
    }

    // The class name is legal, so add it to the list of classes
    // and the symbol table.
    nds = new List<CgenNode>(nd, nds);
    addid(name, nd);
}

void CgenClassTable::install_classes(Classes cs) {
    int classTag = USER_CLASS_TAG_OFFSET;
    for (int i = cs->first(); cs->more(i); i = cs->next(i))
        install_class(new CgenNode(cs->nth(i), NotBasic, this, classTag++));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree() {
    for (List<CgenNode> *l = nds; l; l = l->tl())
        set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd) {
    CgenNode *parent_node = probe(nd->get_parent());
    nd->set_parentnd(parent_node);
    parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n) {
    children = new List<CgenNode>(n, children);
}

void CgenNode::set_parentnd(CgenNodeP p) {
    assert(parentnd == NULL);
    assert(p != NULL);
    parentnd = p;
}

void CgenClassTable::code() {
    if (cgen_debug)
        cout << "coding global data" << endl;
    code_global_data();

    if (cgen_debug)
        cout << "choosing gc" << endl;
    code_select_gc();

    if (cgen_debug)
        cout << "coding constants" << endl;
    code_constants();

    //                 Add your code to emit
    //                   - prototype objects
    //                   - class_nameTab
    //                   - dispatch tables
    //
    code_class_nameTab();
    code_class_objTab();
    code_dispTab();
    code_protObjs();

    if (cgen_debug)
        cout << "coding global text" << endl;
    code_global_text();

    //                 Add your code to emit
    //                   - object initializer
    //                   - the class methods
    //                   - etc...

    code_initializer();
    code_methods();
}

CgenNodeP CgenClassTable::root() { return probe(Object); }

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct,
                   int classTag)
    : class__class((const class__class &)*nd), parentnd(NULL), children(NULL),
      basic_status(bstatus), classTag(classTag) {
    stringtable.add_string(
        name->get_string()); // Add class name to string table
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s, ExpressionContext &context) {
    expr->code(s, context);
    context.classTable->setNameAddress(name, ACC, s);
}

void static_dispatch_class::code(ostream &s, ExpressionContext &context) {
    auto symbolTable = context.classTable->get_names();
    StackUsage stack(s);
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        auto actualNode = actual->nth(i);
        actualNode->code(s, context);
        stack.singlePush(ACC);
    }
    expr->code(s, context);
    auto staticClass = context.classTable->getClassNode(type_name);
    auto methodClass = staticClass->get_override_method_class(name);
    emit_method_call(methodClass, name, s);
    curSpFpSize -= actual->len();
}

void dispatch_class::code(ostream &s, ExpressionContext &context) {
    auto symbolTable = context.classTable->get_names();
    StackUsage stack(s);
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        auto actualNode = actual->nth(i);
        actualNode->code(s, context);
        stack.singlePush(ACC);
    }
    expr->code(s, context);
    int notVoidLabel = curLabel++;
    emit_bne(ACC, ZERO, notVoidLabel, s);

    // dispatch on void
    auto fileName = stringtable.add_string(context.classNode->get_filename()->get_string());
    emit_load_imm(T1, expr->get_line_number(), s);  // line number
    emit_load_string(ACC, fileName, s);
    emit_jalr("_dispatch_abort", s);

    emit_label_def(notVoidLabel, s);
    emit_load(T1, DISPTABLE_OFFSET, ACC, s);

    auto exprType = expr->get_type();
    if (exprType == SELF_TYPE) {
        exprType = context.classNode->get_name();
    }
    int methodIndex =
        context.classTable->lookup(exprType)->find_method_index(name);
    if (context.classNode->get_name() == Main) {
        int breakpoint = 0;
    }
    emit_load(T1, methodIndex, T1, s);
    emit_jalr(T1, s);
    curSpFpSize -= actual->len();
}

void cond_class::code(ostream &s, ExpressionContext &context) {
    auto symbolTable = context.classTable->get_names();
    pred->code(s, context);
    int falseLabel = curLabel++;
    int endLabel = curLabel++;
    emit_load(ACC, DEFAULT_OBJFIELDS, ACC, s);
    emit_beqz(ACC, falseLabel, s);
    then_exp->code(s, context);
    emit_branch(endLabel, s);
    emit_label_def(falseLabel, s);
    else_exp->code(s, context);
    emit_label_def(endLabel, s);
}

void loop_class::code(ostream &s, ExpressionContext &context) {
    auto symbolTable = context.classTable->get_names();
    int endLabel = curLabel++;
    int startLabel = curLabel++;
    // loop
    emit_label_def(startLabel, s);
    pred->code(s, context);
    emit_load(ACC, DEFAULT_OBJFIELDS, ACC, s);
    emit_beqz(ACC, endLabel, s);
    body->code(s, context);
    emit_branch(startLabel, s);
    // pool
    emit_label_def(endLabel, s);
    emit_move(ACC, ZERO, s);
}

void typcase_class::code(ostream &s, ExpressionContext &context) {
    auto symbolTable = context.classTable->get_names();
    StackUsage stack(s);
    expr->code(s, context);
    int endLabel = curLabel++;
    int noMatchCaseLabel = curLabel++;
    int caseVoidLabel = curLabel++;
    stack.push(ACC);

    emit_beqz(ACC, caseVoidLabel, s);

    emit_load(T1, TAG_OFFSET, ACC, s);

    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        int nextLabel = curLabel++;
        auto branch = dynamic_cast<branch_class *>(cases->nth(i));
        int curTag =
            context.classTable->lookup(branch->type_decl)->get_classTag();
        emit_load_imm(T2, curTag, s);
        emit_bne(T1, T2, nextLabel, s);

        NameScope branchScope(context.classTable);
        // hit
        context.classTable->bindObjectName(branch->name, FP, -curSpFpSize);
        branch->expr->code(s, context);
        emit_branch(endLabel, s);

        // fallback
        emit_label_def(nextLabel, s);
    }

    auto fileName = stringtable.add_string(context.classNode->get_filename()->get_string());

    // if no matched case
    emit_label_def(noMatchCaseLabel, s);
    emit_jal("_case_abort", s);
    emit_branch(endLabel, s);
    // if case expr is void
    emit_label_def(caseVoidLabel, s);
    emit_load_imm(T1, expr->get_line_number(), s);  // line number
    emit_load_string(ACC, fileName, s);
    emit_jal("_case_abort2", s);

    emit_label_def(endLabel, s);
}

void block_class::code(ostream &s, ExpressionContext &context) {
    auto symbolTable = context.classTable->get_names();
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        body->nth(i)->code(s, context);
    }
}

void let_class::code(ostream &s, ExpressionContext &context) {
    auto symbolTable = context.classTable->get_names();
    StackUsage stack(s);
    if(!init->get_type() && (type_decl == Int || type_decl == Bool || type_decl == Str)) {
        if(type_decl == Int) {
            emit_load_int(ACC, intDefault, s);
        } else if(type_decl == Bool) {
            emit_load_bool(ACC, falsebool, s);
        } else {
            emit_load_string(ACC, stringDefault, s);
        }
    } else {
        init->code(s, context);
    }
    NameScope letScope(context.classTable);
    stack.push(ACC);
    context.classTable->bindObjectName(identifier, FP, -curSpFpSize);
    body->code(s, context);
}

void plus_class::code(ostream &s, ExpressionContext &context) {
    auto symbolTable = context.classTable->get_names();
    StackUsage stack(s);
    e1->code(s, context);
    emit_load_prim1(ACC, s);
    stack.push(ACC);
    e2->code(s, context);
    emit_load_prim1(ACC, s);
    emit_load(T1, 1, SP, s);
    emit_add(ACC, T1, ACC, s);
    stack.push(ACC);
    emit_static_new(Int, s);
    emit_load(T1, 1, SP, s);
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

void sub_class::code(ostream &s, ExpressionContext &context) {
    auto symbolTable = context.classTable->get_names();
    StackUsage stack(s);
    e1->code(s, context);
    emit_load_prim1(ACC, s);
    stack.push(ACC);
    e2->code(s, context);
    emit_load_prim1(ACC, s);
    emit_load(T1, 1, SP, s);
    emit_sub(ACC, T1, ACC, s);
    stack.push(ACC);
    emit_static_new(Int, s);
    emit_load(T1, 1, SP, s);
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

void mul_class::code(ostream &s, ExpressionContext &context) {
    auto symbolTable = context.classTable->get_names();
    StackUsage stack(s);
    e1->code(s, context);
    emit_load_prim1(ACC, s);
    stack.push(ACC);
    e2->code(s, context);
    emit_load_prim1(ACC, s);
    emit_load(T1, 1, SP, s);
    emit_mul(ACC, T1, ACC, s);
    stack.push(ACC);
    emit_static_new(Int, s);
    emit_load(T1, 1, SP, s);
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

void divide_class::code(ostream &s, ExpressionContext &context) {
    auto symbolTable = context.classTable->get_names();
    StackUsage stack(s);
    e1->code(s, context);
    emit_load_prim1(ACC, s);
    stack.push(ACC);
    e2->code(s, context);
    emit_load_prim1(ACC, s);
    emit_load(T1, 1, SP, s);
    emit_div(ACC, T1, ACC, s);
    stack.push(ACC);
    emit_static_new(Int, s);
    emit_load(T1, 1, SP, s);
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

void neg_class::code(ostream &s, ExpressionContext &context) {
    auto symbolTable = context.classTable->get_names();
    StackUsage stack(s);
    e1->code(s, context);
    emit_method_call(Object, ::copy, s);
    emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
    emit_neg(T1, T1, s);
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

void lt_class::code(ostream &s, ExpressionContext &context) {
    auto symbolTable = context.classTable->get_names();
    StackUsage stack(s);
    e1->code(s, context);
    emit_load_prim1(ACC, s);
    stack.push(ACC);
    e2->code(s, context);
    emit_load_prim1(ACC, s);
    emit_load(T1, 1, SP, s);
    emit_load_bool(T2, truebool, s);
    int ltlabel = curLabel++;
    emit_blt(T1, ACC, ltlabel, s);
    emit_load_bool(T2, falsebool, s);
    emit_label_def(ltlabel, s);
    emit_move(ACC, T2, s);
}

void eq_class::code(ostream &s, ExpressionContext &context) {
    auto symbolTable = context.classTable->get_names();
    if(e1->get_type() == Int || e1->get_type() == Bool) {
        StackUsage stack(s);
        e1->code(s, context);
        emit_load_prim1(ACC, s);
        stack.push(ACC);
        e2->code(s, context);
        emit_load_prim1(ACC, s);
        emit_load(T1, 1, SP, s);
        emit_load_bool(T2, truebool, s);
        int endlabel = curLabel++;
        emit_beq(ACC, T1, endlabel, s);
        emit_load_bool(T2, falsebool, s);
        emit_label_def(endlabel, s);
        emit_move(ACC, T2, s);
    } else if(e1->get_type() == Str) {
        StackUsage stack(s);
        e1->code(s, context);
        stack.push(ACC);
        e2->code(s, context);
        emit_load(T1, 1, SP, s);
        emit_load_bool(T2, falsebool, s);

        int loopCompLabel = curLabel++;
        int trueLabel = curLabel++;
        int endlabel = curLabel++;

        emit_load(T4, DEFAULT_OBJFIELDS, ACC, s);
        emit_load(T5, DEFAULT_OBJFIELDS, T1, s);
        emit_load(T4, DEFAULT_OBJFIELDS, T4, s);
        emit_load(T5, DEFAULT_OBJFIELDS, T5, s);
        emit_bne(T4, T5, endlabel, s);

        emit_load(T3, SIZE_OFFSET, ACC, s);
        emit_addiu(T3, T3, -DEFAULT_OBJFIELDS-1, s);
        
        emit_addiu(ACC, ACC, (DEFAULT_OBJFIELDS+1) * WORD_SIZE, s);
        emit_addiu(T1, T1, (DEFAULT_OBJFIELDS+1) * WORD_SIZE, s);

        // loop:
        emit_label_def(loopCompLabel, s);
        emit_load(T4, 0, ACC, s);
        emit_load(T5, 0, T1, s);
        emit_bne(T4, T5, endlabel, s);

        emit_addiu(ACC, ACC, 4, s);
        emit_addiu(T1, T1, 4, s);
        emit_addiu(T3, T3, -1, s);
        emit_bne(T3, ZERO, loopCompLabel, s);

        emit_label_def(trueLabel, s);
        emit_load_bool(T2, truebool, s);
        emit_label_def(endlabel, s);
        emit_move(ACC, T2, s);
    } else {
        StackUsage stack(s);
        e1->code(s, context);
        stack.push(ACC);
        e2->code(s, context);
        emit_load(T1, 1, SP, s);
        emit_load_bool(T2, truebool, s);
        int endlabel = curLabel++;
        emit_beq(ACC, T1, endlabel, s);
        emit_load_bool(T2, falsebool, s);
        emit_label_def(endlabel, s);
        emit_move(ACC, T2, s);
    }
}

void leq_class::code(ostream &s, ExpressionContext &context) {
    auto symbolTable = context.classTable->get_names();
    StackUsage stack(s);
    e1->code(s, context);
    emit_load_prim1(ACC, s);
    stack.push(ACC);
    e2->code(s, context);
    emit_load_prim1(ACC, s);
    emit_load(T1, 1, SP, s);
    emit_load_bool(T2, truebool, s);
    int leqlabel = curLabel++;
    emit_bleq(T1, ACC, leqlabel, s);
    emit_load_bool(T2, falsebool, s);
    emit_label_def(leqlabel, s);
    emit_move(ACC, T2, s);
}

void comp_class::code(ostream &s, ExpressionContext &context) {
    auto symbolTable = context.classTable->get_names();
    StackUsage stack(s);
    e1->code(s, context);
    emit_method_call(Object, ::copy, s);
    emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
    emit_load_imm(T2, 1, s);
    emit_xor(T1, T1, T2, s);
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

void int_const_class::code(ostream &s, ExpressionContext &context) {
    //
    // Need to be sure we have an IntEntry *, not an arbitrary Symbol
    //
    emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
}

void string_const_class::code(ostream &s, ExpressionContext &context) {
    emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
}

void bool_const_class::code(ostream &s, ExpressionContext &context) {
    emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s, ExpressionContext &context) {
    if(type_name == SELF_TYPE) {
        emit_load(T2, TAG_OFFSET, SELF, s);
        emit_dynamic_new(T2, s);
    } else {
        emit_static_new(type_name, s);
    }
}

void isvoid_class::code(ostream &s, ExpressionContext &context) {
    auto symbolTable = context.classTable->get_names();
    e1->code(s, context);
    emit_move(T1, ACC, s);
    emit_load_bool(ACC, truebool, s);
    int endLabel = curLabel++;
    emit_beqz(T1, endLabel, s);
    emit_load_bool(ACC, falsebool, s);
    emit_label_def(endLabel, s);
}

void no_expr_class::code(ostream &s, ExpressionContext &context) {
    auto symbolTable = context.classTable->get_names();
    emit_move(ACC, ZERO, s);
}

void object_class::code(ostream &s, ExpressionContext &context) {
    context.classTable->getNameAddress(name, ACC, s);
}

StackUsage::~StackUsage() {
    if(allocatedWord > 0)
        emit_addiu(SP, SP, allocatedWord * WORD_SIZE, s);
    curSpFpSize -= allocatedWord;
}
void StackUsage::push(char *reg) {
    emit_push(reg, s);
    ++allocatedWord;
}
void StackUsage::singlePush(char *reg) { emit_push(reg, s); }

void RegisterName::getAddress(char *destreg, ostream &s) {
    emit_move(destreg, reg, s);
}
void RegisterName::setAddress(char *sourcereg, ostream &s) {
    emit_move(reg, sourcereg, s);
}

void MemoryName::getAddress(char *destreg, ostream &s) {
    emit_load(destreg, offset, reg, s);
}
void MemoryName::setAddress(char *sourcereg, ostream &s) {
    emit_store(sourcereg, offset, reg, s);
}