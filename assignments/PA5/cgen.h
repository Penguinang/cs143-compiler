#include <algorithm>
using std::sort;
using std::swap;
using std::transform;
using std::for_each;
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

class Reference;
using ReferenceP = Reference *;

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
    SymbolTable<Symbol, Reference> names;
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
    void code_class_dispTab(CgenNode *classNode);
    void code_protObjs();
    void code_attributes(CgenNodeP classNode);
    void code_methods();
    void code_class_methods(CgenNode *classNode);
    void code_class_method(CgenNode *classNode, method_class *methodNode);
    void code_initializers();
    void code_class_initializer(CgenNode *classNode);

    void code_method_head();
    void code_method_tail(int localStackSizeWord = 0);

    // The following creates an inheritance graph from
    // a list of classes.  The graph is implemented as
    // a tree of `CgenNode', and class names are placed
    // in the base class symbol table.

    void install_basic_classes();
    void install_class(CgenNodeP nd);
    void install_classes(Classes cs);
    void build_inheritance_tree();
    void set_relations(CgenNodeP nd);
    void calculateClassTag();

  public:
    CgenClassTable(Classes, ostream &str);
    void code();
    CgenNodeP root();

    auto &get_names() { return names; }

    /**
     * @param name 标识符
     * @param reg @param offset 标识符对应的引用的位置，可能是一个寄存器或内存位置
     */
    void bindReferenceName(Symbol name, char *reg, int offset);
    void bindReferenceName(Symbol name, char *reg);
    /**
     * @param startOffset 第一个attribute 开始的地方
     * @return 下一个attribute 开始的地方
     */
    int bindClassAttrNames(CgenNodeP classNode, int startOffset);
    /**
     * @param name 标识符
     * @param reg 寄存器名称
     * @brief 加载标识符所引用对象的地址到reg中
     */
    void loadReference(Symbol name, char *reg, ostream &s);
    /**
     * @brief 将引用重新绑定到reg所指的地址上
     */
    void referenceRebind(Symbol name, char *reg, ostream &s);

    bool isAncestor(Symbol child, Symbol ancestor);

    // classes traversal helper
    template<typename VISTOR>
    void preorderTraverse(VISTOR const &visitor) {
        return preorderTraverse(root(), visitor);
    }
    template<typename VISTOR>
    void preorderTraverse(CgenNodeP start, VISTOR const &visitor);
    template<typename VISTOR>
    void listLinearOrder(VISTOR const &visitor) {
        for (auto l = nds; l; l = l->tl()) {
            visitor(l->hd());
        }
    }
};

int constexpr NO_CLASS_TAG = -1;
int constexpr CLASS_TAG_PLACEHOLDER = -1;

class CgenNode : public class__class {
  private:
    CgenNodeP parentnd;       // Parent of class
    List<CgenNode> *children; // Children of class
    Basicness basic_status;   // `Basic' if class is basic
                              // `NotBasic' otherwise
    int classTag;

    map<Symbol, pair<Symbol, size_t>> methodTable;
    bool parentDirtyBit = true;

    void recalculateMethodTable();

  public:
    CgenNode(Class_ c, Basicness bstatus, CgenClassTableP class_table,
             int classTag);

    void add_child(CgenNodeP child);
    List<CgenNode> *get_children() { return children; }
    void set_parentnd(CgenNodeP p);
    CgenNodeP get_parentnd() { return parentnd; }
    int basic() { return (basic_status == Basic); }

    int find_method_index(Symbol name);
    int get_attr_count() {
        int count = 0;
        for_each_attr([&](attr_class*){
            ++count;
        });
        if(parentnd)
            count += parentnd->get_attr_count();
        return count;
    }

    /**
     * @return className of class which finally override method `methodName`
     */
    Symbol get_override_method_class(Symbol methodName) {
        auto mTable = getMethodTable();
        auto methodIt = mTable.find(methodName);
        return methodIt == mTable.end() ? nullptr : methodIt->second.first;
    }

    bool isAncestor(Symbol type) {
        if(parentnd) {
            return this->parent == type || parentnd->isAncestor(type);
        } else {
            return false;
        }
    }

    /**
     * @brief 返回子类个数
     */
    int offspringCount() {
        int ret = 0;
        for(auto l = children; l; l = l->tl()) {
            ++ ret;
            ret += l->hd()->offspringCount();
        }
        return ret;
    }

    map<Symbol, pair<Symbol, size_t>> const& getMethodTable();

    GETTER(classTag)
    void set_classTag(int &classTag) {
        this->classTag = classTag;
    }

    template<typename VISITOR>
    void for_each_attr(VISITOR const &visitor) {
        for (int i = features->first(); features->more(i);
                i = features->next(i)) {
            if (auto attrNode = dynamic_cast<attr_class *>(features->nth(i))) {
                visitor(attrNode);
            }
        }
    }

    template<typename VISITOR>
    void for_each_method(VISITOR const &visitor) {
        for (int i = features->first(); features->more(i);
                i = features->next(i)) {
            if (auto attrNode = dynamic_cast<method_class *>(features->nth(i))) {
                visitor(attrNode);
            }
        }
    }    
};

class BoolConst {
  private:
    int val;

  public:
    BoolConst(int);
    void code_def(ostream &, int boolclasstag);
    void code_ref(ostream &) const;
};

/**
 * @brief 引用对象
 * 
 * 引用的右值是另一个对象的地址
 */
class Reference {
  public:
    /**
     * @brief 加载引用的右值到reg中，即是加载所引用对象的地址
     */
    virtual void getAddress(char *reg, ostream &s) = 0;
    /**
     * @brief 设置引用的右值为reg中的值，相当于将引用绑定到新的对象
     */
    virtual void setAddress(char *reg, ostream &s) = 0;
};
// address stored in register
class RegisterReference : public Reference {
    char *reg;

  public:
    RegisterReference(char *reg) : reg(reg) {}
    virtual void getAddress(char *reg, ostream &s);
    virtual void setAddress(char *reg, ostream &s);
};
// address stored in memory
class MemoryReference : public Reference {
    char *reg;
    int offset;

  public:
    MemoryReference(char const *reg, int offset)
        : reg(const_cast<char *>(reg)), offset(offset) {}
    virtual void getAddress(char *reg, ostream &s);
    virtual void setAddress(char *reg, ostream &s);
};

/**
 * @brief 运行时栈的抽象，帮助自动退栈，以及维护栈长度信息
 */
class StackUsage {
    ostream &s;
    int allocatedWord = 0;

  public:
    StackUsage(ostream &s) : s(s) {}
    ~StackUsage();
    /**
     * @brief 每个push的字最终会在栈析构时弹出
     */
    void push(char *reg);
    /**
     * @brief 析构时不会有对应的弹栈
     */
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

template<typename VISTOR>
void CgenClassTable::preorderTraverse(CgenNodeP start, VISTOR const &visitor) {
    stack<CgenNodeP> toTraverse;
    toTraverse.push(start);
    while(!toTraverse.empty()) {
        auto cur = toTraverse.top();
        toTraverse.pop();
        visitor(cur);
        for(auto l = cur->get_children(); l; l = l->tl()) {
            toTraverse.push(l->hd());
        }
    }
}

template<typename Elem, typename VISTOR>
inline void for_each(list_node<Elem> *listHead, VISTOR const &visitor) {
    for (int i = listHead->first(); listHead->more(i); i = listHead->next(i)) {
        visitor(listHead->nth(i));
    }    
}

// TODO register 包装
/**
 * @example 
 * Register s1 = "$s1";
 * s1 = 1; // move s1 1
 * *s1 = 1; // load s1 1
 * Register s2 = "$s2";
 * s2 = s1; // move s2 s1
 */
class Register{
  public:
};