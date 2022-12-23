#include <assert.h>
#include <stdio.h>
#include <vector>
#include <map>
#include <list>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0
#define ATTR_BASE_OFFSET 3
#define DISPATCH_OFFSET 2

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
public:
    typedef std::vector<attr_class*> attrList;
    typedef std::vector<method_class*> methodList;
    typedef std::map<Symbol, int> attroffsetList;
    typedef std::map<Symbol, std::map<Symbol, int>> methoffsetList;
private:
   List<CgenNode> *nds;     // 维护的整个程序中的所有class
   std::map<Symbol, int> class_tag_map_;
   std::map<Symbol, attrList> class_attr_map_;  // 不包含parent中的attr
   std::map<Symbol, methodList> class_method_map_;
   std::map<Symbol, attroffsetList> attr_offset_map_;
   std::map<Symbol, methoffsetList> meth_offset_map_;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;
   int labelid_;
   CgenNodeP curr_cgenclass_;
// The following methods emit code for
// constants and global declarations.

   void code_global_data();         // 数据段
   void code_global_text();         // 代码段
   void code_bools(int);
   void code_select_gc();
   void code_constants();
   // 待实现
   // 第一阶段实现
   void code_class_nametabs();
   void code_class_objtabs();
   void code_object_disptabs();
   // 第二阶段实现
   void code_protobjs();
   void code_object_inits();
   // 第三阶段实现
   void code_methods();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void install_classtags(int len);
   void install_attrs_and_methods();
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
public:
   CgenClassTable(Classes, ostream& str);
   int get_labelid() const { return labelid_; }
   void add_labelid() { labelid_++; }
   CgenNodeP get_curr_class() const { return curr_cgenclass_; }
   bool get_attr_offset(Symbol cls, Symbol attr, int *offset);
   bool get_meth_offset(Symbol cls1, Symbol cls2, Symbol meth, int *offset);
   bool get_meth_offset(Symbol cls, Symbol meth, int *offset);

   ostream& codege_str() {
       return str;
   }
   void code();
   CgenNodeP root();
};

class EnvTable {
public:
    typedef std::list<std::pair<Symbol, int>> symbol2offsetList;
private:
    std::list<symbol2offsetList> envlist_;
    int formal_fp_offset_;  // 用来记录当前formal对于fp的偏移
    int local_fp_offset_; // 用来记录当前local变量对于fp的偏移

    void init_fpoffset();
public:
    EnvTable() = default;
    ~EnvTable() = default;

    void enterframe();
    void exitframe();
    void enterscope();
    void exitscope();

    void add_formal_id(Symbol name);
    void add_local_id(Symbol name);
    bool lookup(Symbol name, int *offset); // 通过参数返回
};

void EnvTable::init_fpoffset() {
    formal_fp_offset_ = DEFAULT_OBJFIELDS;
    local_fp_offset_ = -1; // 避免和其他运算中的冲突
}

void EnvTable::enterframe() {
    init_fpoffset();
    enterscope();
}

void EnvTable::exitframe() {
    init_fpoffset();
    exitscope();
}

void EnvTable::enterscope() {
    //std::cout << "#enter scope\n";
    envlist_.push_back({});
}

void EnvTable::exitscope() {
    //std::cout << "#pop scope\n";
    envlist_.pop_back();
}

void EnvTable::add_formal_id(Symbol name) {
    //std::cout << "#push name " << name << " and " << offset << endl;
    envlist_.back().push_back({name, formal_fp_offset_++});
}

void EnvTable::add_local_id(Symbol name) {
    envlist_.back().push_back({name, local_fp_offset_--});
}

bool EnvTable::lookup(Symbol name, int *offset) {
    //std::cout <<"# find name " << name << " " << envlist_.size() << endl;
    for (auto rit = envlist_.rbegin(); rit != envlist_.rend(); ++rit) {
        const symbol2offsetList &sym2off_list = *rit;
        for (auto rlit = sym2off_list.rbegin(); rlit != sym2off_list.rend(); ++rlit) {
            if (rlit->first == name) {
                *offset = rlit->second;
                return true;
            }
        }
    }
    return false;
}


class CgenNode : public class__class { // 每一个都对应一个class
private:
   int class_tag_;
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   void set_classtag(int tag) { class_tag_ = tag; }
   int get_classtag() const { return class_tag_; }
   CgenNodeP get_parentnd() { return parentnd; }
   std::vector<CgenNodeP> get_parents_list();

   int basic() { return (basic_status == Basic); }
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

