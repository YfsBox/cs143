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
  void install_basic_classes();
  ostream& error_stream;
  std::map<Symbol, std::list<Symbol>> class_graph_;
  std::map<Symbol, Class_> class_name_map_;
  std::map<Symbol, std::list<method_class>> methods_table_;

  bool check_loop();
  void InitInClass(class__class *cls) {
      class_graph_[cls->get_name()] = {};
      class_name_map_[cls->get_name()] = cls;
  }
  bool NameTypeValid(Symbol name);
  void install_information();
  std::vector<Class_> get_class_chain(Class_ cls);

public:
  ClassTable(Classes);
  void show_chains() ;
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};


#endif

