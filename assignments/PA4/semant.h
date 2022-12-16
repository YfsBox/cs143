#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <map>
#include <vector>
#include <list>
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

// 看来还需要为所添加的类建立一个数据结构，用来判断继承图中的环，以及未定义类的问题。

class ClassTable {
private:
  int semant_errors;
  Class_ curr_class_;
  Classes classes_;
  void install_basic_classes();
  ostream& error_stream;
  std::map<Symbol, std::list<Symbol>> class_graph_;
  std::map<Symbol, Class_> class_name_map_;
  std::map<Symbol, std::list<method_class*>> methods_table_;
  std::map<Symbol, std::list<attr_class*>> attrs_table_;

  bool check_loop();
  void InitInClass(class__class *cls) {
      class_graph_[cls->get_name()] = {};
      class_name_map_[cls->get_name()] = cls;
  }
  bool NameTypeValid(Symbol name);
  bool check_method_name(Symbol cls, method_class *feature);
  bool check_attr_name(Symbol cls, attr_class *feature);
  void install_methods_and_attrs();

public:
  ClassTable(Classes);
  void show_chains() ;
  void check_and_install();
  Class_ get_class_byname(Symbol name) const;
  method_class *get_method(Class_ cls, Symbol name);
  Class_ get_curr_class() const;
  Symbol find_lastcommon_root(Class_ cls1, Class_ cls2);
  std::list<Class_> get_class_chain(Class_ cls);
  int errors() { return semant_errors; }
  // some test function
  void test_find_lc_root();

  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

  ostream& semant_debug();
  ostream& semant_debug(Class_ c);
  ostream& semant_debug(Symbol filename, tree_node *t);
};

class Debug {
private:
    std::string msg_;
public:
    Debug(const std::string &msg);
    ~Debug();
};

#endif

