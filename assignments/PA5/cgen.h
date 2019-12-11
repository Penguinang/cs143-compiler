#include <algorithm>
using std::sort;
using std::swap;
using std::transform;
#include <map>
using std::get;
using std::map;
using std::pair;
using std::tuple;
#include <vector>
using std::vector;
#include <iterator>
using std::back_inserter;

#include "cool-tree.h"
#include "emit.h"
#include "symtab.h"
#include <assert.h>
#include <stdio.h>

enum Basicness { Basic, NotBasic };
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class NameNode;
using NameNodeP = NameNode *;

class CgenClassTable : public SymbolTable<Symbol, CgenNode> {
  private:
    List<CgenNode> *nds;
    ostream &str;
    int stringclasstag;
    int intclasstag;
    int boolclasstag;

    // 对象符号表，保存地址
    // 1. formal         sp + offset
    // 2. attribute      self + offset
    // 3. let            sp + offset
    // 4. case           sp + offset
    SymbolTable<Symbol, NameNode> names;
    friend class NameScope;

    // The following methods emit code for
    // constants and global declarations.

    void code_global_data();
    void code_global_text();
    void code_bools(int);
    void code_select_gc();
    void code_constants();

    void code_class_nameTab();
    void code_class_objTab();
    void code_dispTab();
    /**
     * @return: pair of <methodName, <className, index>>
     */
    map<Symbol, pair<Symbol, size_t>> get_class_dispTab(CgenNode *classNode);
    void code_class_dispTab(CgenNode *classNode);
    void code_protObjs();
    void code_attributes(CgenNodeP classNode);
    void code_initializer();
    void code_class_initializer(CgenNode *classNode);
    void code_methods();
    void code_class_methods(CgenNode *classNode);
    void code_method(CgenNode *classNode, method_class *methodNode);
    void code_method_head(int frameSizeWord);
    void code_method_tail(int frameSizeWord, int localStackSizeWord);

    // The following creates an inheritance graph from
    // a list of classes.  The graph is implemented as
    // a tree of `CgenNode', and class names are placed
    // in the base class symbol table.

    void install_basic_classes();
    void install_class(CgenNodeP nd);
    void install_classes(Classes cs);
    void build_inheritance_tree();
    void set_relations(CgenNodeP nd);

  public:
    CgenClassTable(Classes, ostream &str);
    void code();
    CgenNodeP root();

    auto &get_names() { return names; }

    void bindObjectName(Symbol name, char *reg, int offset);
    void bindObjectName(Symbol name, char *reg);
    void bindClassAttrs(CgenNodeP classNode, int &attrOffset);
    void unbindObjectName(Symbol name);
    void getNameAddress(Symbol name, char *reg, ostream &s);
    void setNameAddress(Symbol name, char *reg, ostream &s);
    CgenNodeP getClassNode(Symbol className);
};

int constexpr NO_CLASS_TAG = -1;
int constexpr OBJECT_CLASS_TAG = 0;
int constexpr IO_CLASS_TAG = 1;
int constexpr INT_CLASS_TAG = 2;
int constexpr BOOL_CLASS_TAG = 3;
int constexpr STRING_CLASS_TAG = 4;
int constexpr USER_CLASS_TAG_OFFSET = 5;

class CgenNode : public class__class {
  private:
    CgenNodeP parentnd;       // Parent of class
    List<CgenNode> *children; // Children of class
    Basicness basic_status;   // `Basic' if class is basic
                              // `NotBasic' otherwise
    int classTag;

    bool internal_find_method_index(Symbol name, int &start) {
        if (get_parentnd()) {
            if (get_parentnd()->internal_find_method_index(name, start)) {
                return true;
            }
        }

        for (int i = features->first(); features->more(i);
             i = features->next(i)) {
            if (auto methodNode =
                    dynamic_cast<method_class *>(features->nth(i))) {
                if (methodNode->get_name() == name) {
                    return true;
                } else {
                    ++start;
                }
            }
        }

        return false;
    }

  public:
    CgenNode(Class_ c, Basicness bstatus, CgenClassTableP class_table,
             int classTag);

    void add_child(CgenNodeP child);
    List<CgenNode> *get_children() { return children; }
    void set_parentnd(CgenNodeP p);
    CgenNodeP get_parentnd() { return parentnd; }
    int basic() { return (basic_status == Basic); }

    int find_method_index(Symbol name) {
        int start = 0;
        if (!internal_find_method_index(name, start)) {
            cerr << "no method " << name;
            assert(false);
        }
        return start;
    }

    int get_attr_count() {
        int attr_count = 0;
        if (get_parentnd()) {
            attr_count = get_parentnd()->get_attr_count();
        }
        auto features = get_features();
        for (int i = features->first(); features->more(i);
             i = features->next(i)) {
            if (dynamic_cast<attr_class *>(features->nth(i))) {
                ++attr_count;
            }
        }
        return attr_count;
    }

    /**
     * @return: className of class which finally override method `methodName`
     */
    Symbol get_override_method_class(Symbol methodName) {
        auto features = get_features();
        for (int i = features->first(); features->more(i);
             i = features->next(i)) {
            if (auto methodNode =
                    dynamic_cast<method_class *>(features->nth(i))) {
                if (methodNode->get_name() == methodName) {
                    return get_name();
                }
            }
        }

        if (get_parentnd())
            return get_parentnd()->get_override_method_class(methodName);
        else
            return nullptr;
    }

    GETTER(classTag)
};

class BoolConst {
  private:
    int val;

  public:
    BoolConst(int);
    void code_def(ostream &, int boolclasstag);
    void code_ref(ostream &) const;
};

class NameNode {
  public:
    virtual void getAddress(char *reg, ostream &s) = 0;
    virtual void setAddress(char *reg, ostream &s) = 0;
};
// address stored in register
class RegisterName : public NameNode {
    char *reg;

  public:
    RegisterName(char *reg) : reg(reg) {}
    virtual void getAddress(char *reg, ostream &s);
    virtual void setAddress(char *reg, ostream &s);
};
// address stored in memory
class MemoryName : public NameNode {
    char *reg;
    int offset;

  public:
    MemoryName(char const *reg, int offset)
        : reg(const_cast<char *>(reg)), offset(offset) {}
    virtual void getAddress(char *reg, ostream &s);
    virtual void setAddress(char *reg, ostream &s);
};

class StackUsage {
    ostream &s;
    int allocatedWord = 0;
    int pseudoPopWord = 0;

  public:
    StackUsage(ostream &s) : s(s) {}
    ~StackUsage();
    // pop on destroy
    void push(char *reg);
    // not pop on destroy, for function parameters poped by callee
    void singlePush(char *reg);
};

class StackSizeTracker {
    int lastSize;
    int &curSize;

  public:
    StackSizeTracker(int &curSize) : lastSize(0), curSize(curSize) {
        swap(lastSize, curSize);
    }
    ~StackSizeTracker() { swap(lastSize, curSize); }
};
class NameScope {
    CgenClassTable *classTable;

  public:
    NameScope(CgenClassTable *classTable) : classTable(classTable) {
        classTable->names.enterscope();
    }
    ~NameScope() { classTable->names.exitscope(); }
};