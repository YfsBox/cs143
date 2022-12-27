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
    typedef std::map<Symbol, int> methoffsetList;
    typedef std::vector<std::pair<Symbol, Symbol>> dispatchtab;
    typedef std::vector<CgenNodeP> chain;
private:
   List<CgenNode> *nds;     // 维护的整个程序中的所有class
   std::map<int, Symbol> class_tag_map_;
   std::map<Symbol, CgenNodeP> name_to_cgen_map_;
   std::map<Symbol, attrList> class_attr_map_;  // 不包含parent中的attr
   std::map<Symbol, methodList> class_method_map_; // 每个class本层method
   std::map<Symbol, attroffsetList> attr_offset_map_;
   std::map<Symbol, methoffsetList> meth_offset_map_;
   std::map<Symbol, dispatchtab> dispatch_tab_map_;
   std::map<Symbol, chain> parent_chain_map_;

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
   void install_classtags();
   void install_attrs_and_methods();
   void install_name_to_cgen();
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);

public:
   CgenClassTable(Classes, ostream& str);
   int get_labelid_and_add() { return labelid_++; }

   CgenNodeP get_cgennode(Symbol name) { return name_to_cgen_map_[name]; }
   CgenNodeP get_curr_class() const { return curr_cgenclass_; }

   bool get_attr_offset(Symbol cls, Symbol attr, int *offset);
   bool get_meth_offset(Symbol cls1, Symbol cls2, Symbol meth, int *offset);
   bool get_meth_offset(Symbol cls, Symbol meth, int *offset);
   // 一些用来测试的函数

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
    int last_local_fp_offset_;

    void init_formal_fpoffset() {
        formal_fp_offset_ = DEFAULT_OBJFIELDS;
    }
    void init_local_fpoffset() {
        local_fp_offset_ = -1;
        last_local_fp_offset_ = -1;
    }
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

void EnvTable::enterframe() {
    init_formal_fpoffset();
    init_local_fpoffset();
    enterscope();
}

void EnvTable::exitframe() {
    exitscope();
    init_formal_fpoffset();
    init_local_fpoffset();
}

void EnvTable::enterscope() {
    last_local_fp_offset_ = local_fp_offset_;
    envlist_.push_back({});
}

void EnvTable::exitscope() { // 退出作用域的时候需要恢复local中的frame
    envlist_.pop_back();
    local_fp_offset_ = last_local_fp_offset_;
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
   int chain_depth_;
   int class_tag_;
   int descendants_cnt_; // dfs树中子孙的数量, 在typcase中有用
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
   void set_chain_depth(int depth) { chain_depth_ = depth; }
   void set_descendants_cnt(int descnt) { descendants_cnt_ = descnt; }
   int get_classtag() const { return class_tag_; }
   int get_chain_depth() const { return chain_depth_; }
   int get_descendants_cnt() const { return  descendants_cnt_; }
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

