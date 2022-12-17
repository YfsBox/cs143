

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <memory>
#include <vector>
#include <stack>
#include <set>
#include <symtab.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

typedef SymbolTable<Symbol, Symbol> ObjectEnvTable; // object表
ObjectEnvTable objectEnv;

ClassTable* classtable = nullptr;

Debug::Debug(const std::string &msg):msg_(msg) {
    classtable->semant_debug(classtable->get_curr_class()) << msg_ << " enter\n";
}
Debug::~Debug() {
    classtable->semant_debug(classtable->get_curr_class()) << msg_ << " leave\n";
}
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

static bool type_less_or_equal(Symbol class1, Symbol class2) {
    if (class1 == class2) {
        return true;
    }
    class__class *class_class1 = dynamic_cast<class__class*> (classtable->get_class_byname(class1));
    class__class *class_class2 = dynamic_cast<class__class*> (classtable->get_class_byname(class2));
    if (class_class1 == nullptr || class_class2 == nullptr) {
        return false;
    }
    Symbol type1 = class_class1->get_name();
    Symbol type2 = class_class2->get_name();
    auto chain = classtable->get_class_chain(classtable->get_class_byname(class1));
    for (auto chain_class : chain) {
        class__class *chain__class_class = dynamic_cast<class__class*> (chain_class);
        if (chain__class_class->get_name() == type2) {
            return true;
        }
    }
    return false;
}

bool ClassTable::check_loop() {
    std::stack<Symbol> st;
    std::map<Symbol, int> in_map;
    uint32_t cnt = 0;
    // 首先计算入度
    for (auto class_pair : class_graph_) {
        auto class_list = class_pair.second;
        for (auto symbol : class_list) {
            in_map[symbol]++;
        }
    }
    for (auto class_pair : class_graph_) {
        auto class_symbol = class_pair.first;
        if (in_map[class_symbol] == 0) {
            st.push(class_symbol);
        }
    }
    while (!st.empty()) {
        auto symbol = st.top();
        st.pop();
        cnt += 1;
        auto class_list = class_graph_[symbol];
        for (auto symbol : class_list) {
            in_map[symbol]--;
            if (in_map[symbol] == 0) {
                st.push(symbol);
            }
        }
    }
    // printf("the cnt is %d, the class name map size is %d\n", cnt, class_name_map_.size());
    return cnt != class_name_map_.size(); // 如果是true就是有环的
}

std::list<Class_> ClassTable::get_class_chain(Class_ cls) {
    std::list<Class_> chain;
    Symbol curr_symbol;
    Symbol parent_symbol;
    Class_ curr_class = cls;
    while (true) {
        chain.push_front(curr_class);  // 从开头加入
        class__class *curr_class_class = dynamic_cast<class__class*> (curr_class);
        curr_symbol = curr_class_class->get_name();
        if (curr_symbol == Object) { // 检查是否到达继承的终点
            break;
        }
        parent_symbol = curr_class_class->get_parent();
        curr_class = class_name_map_[parent_symbol];
    }
    /*printf("chain:\n");
    for (auto cls : chain) {
        dump_Symbol(error_stream, 1, dynamic_cast<class__class*>(cls)->get_name());
    }*/
    return chain;
}

Class_ ClassTable::get_class_byname(Symbol name) const {
    auto findit = class_name_map_.find(name);
    if (findit != class_name_map_.end()) {
        return findit->second;
    }
    return nullptr;
}

bool ClassTable::check_method_name(Symbol cls, method_class *feature) {
    auto findit = methods_table_.find(cls);
    for (auto method : findit->second) {
        if (method->get_name() == feature->get_name()) {  // 出现重定义的name
            return false;
        }
    }
    return true;
}

bool ClassTable::check_attr_name(Symbol cls, attr_class *feature) {
    auto findit = attrs_table_.find(cls);
    for (auto attr : findit->second) {
        if (attr->get_name() == feature->get_name()) {
            return false;
        }
    }
    return true;
}

Symbol ClassTable::find_lastcommon_root(Class_ cls1, Class_ cls2) {
    auto chain_list1 = get_class_chain(cls1);
    auto chain_list2 = get_class_chain(cls2);

    uint min_size = std::min(chain_list1.size(), chain_list2.size());
    auto chain_list1_it = chain_list1.begin();
    auto chain_list2_it = chain_list2.begin();
    for (uint i = 0; i < min_size; i++) {
        Symbol symbol1 = dynamic_cast<class__class*> (*chain_list1_it)->get_name();
        Symbol symbol2 = dynamic_cast<class__class*> (*chain_list2_it)->get_name();
        if (symbol1 != symbol2) {
            break;
        }
        chain_list1_it++;
        chain_list2_it++;
    }
    chain_list1_it--;
    return dynamic_cast<class__class*> (*chain_list1_it)->get_name();
}

void ClassTable::install_methods_and_attrs() {
    class__class *curr_class;
    Features curr_features;
    for (auto cls_pair : class_name_map_) {
        curr_class = dynamic_cast<class__class*> (cls_pair.second);
        curr_features = curr_class->get_features();
        Feature curr_feature;
        methods_table_[curr_class->get_name()] = {};
        attrs_table_[curr_class->get_name()] = {};
        for (int i = curr_features->first(); curr_features->more(i) == TRUE; i = curr_features->next(i)) {
            curr_feature = curr_features->nth(i);
            // 首先检查名称合法性
            if (!curr_feature->is_attr()) { // 属于method类型
                method_class *curr_method = dynamic_cast<method_class *> (curr_feature);
                if (!check_method_name(curr_class->get_name(), curr_method)) { // 该method重复name应该忽略
                    semant_error(curr_class) << curr_class->get_name() << " the method " << curr_method->get_name()
                                             << " redifinetion.\n";
                    continue;
                }
                methods_table_[curr_class->get_name()].push_back(curr_method); // 加入到method_table中
            } else {
                attr_class *curr_attr = dynamic_cast<attr_class *> (curr_feature);
                if (!check_attr_name(curr_class->get_name(), curr_attr)) {
                    semant_error(curr_class) << curr_class->get_name() << "the attr " << curr_attr->get_name()
                                << " redefinetion.\n";
                    continue;
                }
                attrs_table_[curr_class->get_name()].push_back(curr_attr);
            }
        }
    }
    /*for (auto methods_pair : methods_table_) {
        semant_error() << methods_pair.first << ":\n";
        for (auto method : methods_pair.second) {
            semant_error() << method->get_name() << " ";
        }
        semant_error() << "\n";
    }*/
}


ClassTable::ClassTable(Classes classes) : semant_errors(0) , curr_class_(nullptr), classes_(classes),error_stream(cerr) {
    /* Fill this in */
    install_basic_classes();
    class__class *curr_elem;
    Symbol curr_name, curr_parent;
    /* 首先检查合是否出现重定义，以及不合法的类型 */
    for (int i = classes->first(); classes->more(i) == TRUE; i = classes->next(i)) {
        // curr_elem = classes->nth(i);
        curr_elem = dynamic_cast<class__class*>(classes->nth(i));  // 获取的是class对应的指针
        // class_graph_[elem] = {};    // 记录该项因此表示该项已经存在
        curr_name = curr_elem->get_name();
        curr_parent = curr_elem->get_parent();
        auto findit = class_name_map_.find(curr_name);
        if (findit != class_name_map_.end()) { // 判断是否出现重定义现象
            semant_error(curr_elem) << "class " << curr_name << " redefine\n";
            continue;
        }
        if (!NameTypeValid(curr_parent)) {
            semant_error(curr_elem) << "class " << curr_name << " parent class not valid\n";
            continue;
        }
        InitInClass(curr_elem);
    }
    auto findmain = class_name_map_.find(Main); // 没有main函数
    if (findmain == class_name_map_.end()) {
        semant_error() << "not have main object\n";
    }
    /* 检查是否有未定义的继承，并且构造继承图 */
    for (int i = classes->first(); classes->more(i) == TRUE; i = classes->next(i)) {
        curr_elem = dynamic_cast<class__class*>(classes->nth(i));
        curr_name = curr_elem->get_name();
        curr_parent = curr_elem->get_parent();
        auto findit = class_name_map_.find(curr_parent);
        if (findit == class_name_map_.end()) { // 表示出现了未定义的继承
            semant_error(curr_elem) << "class " << curr_name << " undefined parent class\n";
            continue;
        }
        class_graph_[curr_parent].push_back(curr_name);
    }
    // 检查继承图有没有出现环，通过拓扑排序进行检查即可
    bool have_loop = check_loop();
    if (have_loop) {
        semant_error() << "ring appears in an inheritance relationship\n";
    }
    install_methods_and_attrs();
}

bool ClassTable::NameTypeValid(Symbol name) {
    return name != Int && name != Bool && name != Str && name != SELF_TYPE;
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
    InitInClass(dynamic_cast<class__class*> (Object_class));
    InitInClass(dynamic_cast<class__class*> (IO_class));
    InitInClass(dynamic_cast<class__class*> (Int_class));
    InitInClass(dynamic_cast<class__class*> (Bool_class));
    InitInClass(dynamic_cast<class__class*> (Str_class));
    class_graph_[Object].push_back(IO);
    class_graph_[Object].push_back(Int);
    class_graph_[Object].push_back(Bool);
    class_graph_[Object].push_back(Str);
}

Class_ ClassTable::get_curr_class() const {
    return curr_class_;
}

void ClassTable::check_and_install() {
    Symbol curr_class_name;
    class__class *curr_class;
    Features curr_features;
    for (int i = classes_->first(); classes_->more(i) == TRUE; i = classes_->next(i)) {
        curr_class_ = classes_->nth(i);
        curr_class = dynamic_cast<class__class*> (curr_class_);
        curr_class_name = curr_class->get_name();
        curr_features = curr_class->get_features();

        auto chain = get_class_chain(curr_class_); // 获取继承链
        // 根据继承链,来将所有的attr加入到object_env中,其中需要注意作用域
        Symbol chain_symbol;
        for (auto chain_node : chain) { // 从Object一直到该类自身,这一步仅仅是处理attr的声明,先加入符号表之中
            class__class *chain_class = dynamic_cast<class__class*> (chain_node);
            chain_symbol = chain_class->get_name();
            const std::list<attr_class*>& chain_attrs = attrs_table_[chain_symbol];
            objectEnv.enterscope();
            for (auto attr : chain_attrs) {
                objectEnv.addid(attr->get_name(), new Symbol(attr->get_type()));
            }
        }
        // 正式地检查method和attr
        const std::list<attr_class*>& curr_attrs = attrs_table_[curr_class_name];
        const std::list<method_class*>& curr_methods = methods_table_[curr_class_name];
        // 检查attrs的类型，主要在于检查init对应的表达式是否和已经在符号表中记录的一致
        attr_class *curr_attr;
        for (auto attr : curr_attrs) {
            curr_attr = attr;
            Symbol curr_attr_type = curr_attr->get_type();
            Expression curr_attr_init = curr_attr->get_expr();
            Symbol init_type = curr_attr_init->check_type();
            if (init_type == No_type) {
                continue;
            }
            if (curr_attr_type == SELF_TYPE) {
                curr_attr_type = curr_class->get_name(); // 如果attr的类型是SELF_TYPE
            }
            // 检查init的类型是否存在
            auto findit = class_name_map_.find(init_type);
            if (findit == class_name_map_.end()) { // 表示attr中init对应的表达式根本不存在
                semant_error(curr_class_) << "the attr " << curr_attr->get_name() << "'s init type is not defined\n";
            } else if (!type_less_or_equal(init_type, curr_attr_type)) { // 不是 <=的关系
                semant_error(curr_class_) << "the attr type is " << curr_attr_type << " the init type is " <<
                    init_type << " not satisfiy <=\n";
            }
        }
        // 检查method的类型
        Formals curr_formals;
        for (auto method : curr_methods) {
            // 进入该method的scope之中
            objectEnv.enterscope();
            curr_formals = method->get_formals();
            // 对其中的formal进行检查
            Formal curr_formal;
            for (int i = curr_formals->first(); curr_formals->more(i) == TRUE; i = curr_formals->next(i)) {
                curr_formal = curr_formals->nth(i);
                Symbol formal_type = curr_formal->get_type();
                if (formal_type == SELF_TYPE) { // 是否为self类型
                    formal_type = curr_class->get_name();
                } else if (class_name_map_.find(formal_type) == class_name_map_.end()) {  // 检查是否存在这个type,这个分支就是不存在的情况
                    // 不存在这类型则需要报错
                    semant_error(curr_class_) << "the method formal " << curr_formal->get_type() << " in method "
                        << method->get_name() << " is not defined\n";
                }
                // 将这个identifier加入到scope中
                objectEnv.addid(curr_formal->get_name(), new Symbol(formal_type));
            }
            Symbol return_type = method->get_returntype();
            Expression method_expr = method->get_expr();
            Symbol expr_type = method_expr->check_type();
            if (return_type == SELF_TYPE) {
                expr_type = curr_class->get_name();
            } else if (class_name_map_.find(return_type) == class_name_map_.end()) { // 找不到这个type
                semant_error(curr_class_) << "the method return_type "<< return_type
                <<"is not defined\n";
            } else if (!type_less_or_equal(expr_type, return_type)) {
                semant_error(curr_class_) << "the expr type is not <= return type\n";
            }
            objectEnv.exitscope();
        }
        // 离开作用域，弹出
        auto chain_depth = chain.size();
        for (uint i = 0; i < chain_depth; i++) {
            objectEnv.exitscope();
        }
    }
}

method_class *ClassTable::get_method(Class_ cls, Symbol name) {
    Symbol class_name = dynamic_cast<class__class*> (cls)->get_name();
    auto findit = methods_table_.find(class_name);
    if (findit == methods_table_.end()) {
        return nullptr;
    }
    const std::list<method_class*> &methods_list = findit->second;
    // 从list中遍历,遍历到之后就返回
    Symbol method_name;
    for (auto method : methods_list) {
        method_name = method->get_name();
        if (method_name == name) {
            return method;
        }
    }
    return nullptr;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
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

ostream& ClassTable::semant_error(Expression expr) {
    return semant_error(curr_class_->get_filename(), expr);
}

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error() {
    semant_errors++;                            
    return error_stream;
}

ostream& ClassTable::semant_debug(Class_ c) {
    return semant_debug(c->get_filename(),c);
}

ostream& ClassTable::semant_debug(Symbol filename, tree_node *t) {
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_debug();
}

ostream& ClassTable::semant_debug() {
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
void ClassTable::show_chains() {
    class__class* curr_class;
    for (auto cls : class_name_map_) {
        curr_class = dynamic_cast<class__class*> (cls.second);
        auto chain = get_class_chain(cls.second);
        semant_error() << curr_class->get_name() << '\n';
        for (auto chain_class : chain) {
            class__class *chain_class_class = dynamic_cast<class__class*> (chain_class);
            semant_error() << chain_class_class->get_name() << ' ';
        }
        semant_error() << '\n';
    }
}

Symbol branch_class::check_type() {
    objectEnv.enterscope();
    Symbol decl_type = type_decl;
    Symbol var_name = name;
    objectEnv.addid(var_name, new Symbol(decl_type));
    Symbol type = expr->check_type();
    objectEnv.exitscope();
    return type;
}

Symbol static_dispatch_class::check_type() {
    Expressions exprs = actual;
    Expression expr0 = exprs->nth(0);
    // 检查T和expr0的关系
    Symbol expr0_type = expr0->check_type();
    Class_ curr_class = classtable->get_curr_class();
    // 然后检查type_name是否存在
    if (classtable->get_class_byname(type_name) == nullptr) {
        classtable->semant_error() << "the type_name "<< type_name <<" in static dispatch is not exsit\n";
    } else if (!type_less_or_equal(expr0_type, type_name)) { // 判断是否符合T0 <= T的
        classtable->semant_error() << "the expr0 "<< expr0_type <<" is not <= type_name in static_dispatch_class\n";
    }
    // 或者这个method的相关信息
    method_class *method = classtable->get_method(curr_class, name);
    if (method == nullptr) { // 如果都没有这个method，下面的验证就没法进行了,因此需要返回
        classtable->semant_error() << "the method"<< name <<"is not exsit in static_dispatch_class\n";
        type = Object;
        return type;
    }

    Formals method_formals = method->get_formals();
    formal_class *curr_formal;
    for (int i = exprs->first() + 1; exprs->more(i) == TRUE; i = exprs->next(i)) {
        // 对每个formal进行检查,并结合classtable中所记录的method进行比对
        Expression expr = exprs->nth(i);
        Symbol expr_type = expr->check_type();
        curr_formal = dynamic_cast<formal_class*> (method_formals->nth(i - 1));
        Symbol formal_type = curr_formal->get_type();
        if (!type_less_or_equal(expr_type, formal_type)) {
            classtable->semant_error() << "the formal is " << formal_type <<" but the type " << expr_type << "is error in method"
                    << method->get_name() << '\n';
        }
        if (exprs->more(i + 1) == FALSE) { // 如果是最后一项
            type = (formal_type == SELF_TYPE) ? expr0_type : formal_type;
        }
    }
    return type;
}

Symbol dispatch_class::check_type() {
    Class_ curr_class = classtable->get_curr_class();
    Symbol expr_type = expr->check_type();
    if (expr_type == SELF_TYPE) {
        expr_type = dynamic_cast<class__class*>(classtable->get_curr_class())->get_name();
    } else if (classtable->get_class_byname(expr_type) == nullptr) { // 根本找不到这个type
        classtable->semant_error(this) << "the type of expr " << expr_type << " is not defined\n";
        type = Object;
        return type;
    }
    // 判断method是否存在
    method_class *method = classtable->get_method(curr_class, name);
    if (method == nullptr) { // 如果这个method不存在的话
        classtable->semant_error(this) << "the method " << method->get_name() << " is not exsit\n";
        type = Object;
        return type;
    }

    Formals method_formals = method->get_formals();
    formal_class *curr_formal;

    for (int i = actual->first(); actual->more(i) == TRUE; i = actual->next(i)) {
        Expression expr = actual->nth(i);
        Symbol curr_expr_type = expr->check_type();
        curr_formal = dynamic_cast<formal_class*> (method_formals->nth(i));
        Symbol formal_type = curr_formal->get_type();

        if (!type_less_or_equal(curr_expr_type, formal_type)) {
            classtable->semant_error(this) << "the formal is " << formal_type <<" but the type " << expr_type << "is error in method"
                                       << method->get_name() << '\n';
        }
        if (actual->more(i + 1) == FALSE) { // 如果是最后一项
            type = (formal_type == SELF_TYPE) ? curr_expr_type : formal_type;
        }
    }
    return Object;
}

Symbol cond_class::check_type() {
    Symbol pred_type = pred->check_type();
    if (pred_type != Bool) {
        classtable->semant_error(this) << "The type of pred is not Bool\n";
    }

    Symbol then_type = then_exp->check_type();
    Symbol else_type = else_exp->check_type();
    Class_ then_class = classtable->get_class_byname(then_type);
    Class_ else_class = classtable->get_class_byname(else_type);

    type = classtable->find_lastcommon_root(then_class, else_class);
    return type;
}

Symbol loop_class::check_type() {
    Symbol e1_type = pred->check_type();
    if (e1_type != Bool) {
        classtable->semant_error(this) << "the pred type is not Bool in loop_class\n";
    }
    Symbol e2_type = body->check_type();
    type = Object;
    return type;
}

Symbol typcase_class::check_type() {
    Symbol type0 = expr->check_type();
    Case case_class;
    Symbol case_symbol, last_case_symbol;
    std::set<Symbol> symbol_sets;
    for (int i = cases->first(); cases->more(i) == TRUE; i = cases->next(i)) {
        case_class = cases->nth(i);
        case_symbol = case_class->check_type();
        auto findit = symbol_sets.find(case_symbol);
        if (findit != symbol_sets.end()) {
            classtable->semant_error(this) << "The brach type is repeat\n";
        } else {
            symbol_sets.insert(case_symbol);
        }
        if (i > 0) {
            last_case_symbol = classtable->find_lastcommon_root(
                    classtable->get_class_byname(case_symbol),
                    classtable->get_class_byname(last_case_symbol));
        } else {
            last_case_symbol = case_symbol;
        }
    }
    type = last_case_symbol;
    return type;
}

Symbol block_class::check_type() {
    Expression expr;
    Symbol expr_type;
    {
        for (int i = body->first(); body->more(i) == TRUE; i = body->next(i)) {
            expr = body->nth(i);
            expr_type = expr->check_type();
        }
        type = expr_type;
    }
    return type;
}

Symbol let_class::check_type() {
    Symbol e1_type = init->check_type();
    Symbol decl_type = type_decl;
    if (classtable->get_class_byname(type_decl) == nullptr) { // 不存在这个类型的type_decl
        classtable->semant_error(this) << "the decl type " << type_decl << " is not defined\n";
        type = Object;
        return type;
    }
    if (e1_type != No_type) { // 如果有init的
        if (type_decl == SELF_TYPE) {
            decl_type = dynamic_cast<class__class *>(classtable->get_curr_class())->get_name();
        }
        if (!type_less_or_equal(e1_type, decl_type)) {
            classtable->semant_error(this) << "the init type not <= decl type\n";
            type = Object;
            return type;
        }
    }
    objectEnv.enterscope();
    objectEnv.addid(identifier, new Symbol(type_decl));
    Symbol e2_type = body->check_type();
    objectEnv.exitscope();
    type = e2_type;
    return type;
}

Symbol plus_class::check_type() {
    Symbol e1_type = e1->check_type();
    Symbol e2_type = e2->check_type();
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(this) << "the e1 or e2 is not Int type in a plus_class\n";
    }
    type = Int;
    return type;
}

Symbol sub_class::check_type() {
    Symbol e1_type = e1->check_type();
    Symbol e2_type = e2->check_type();
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(this) << "the e1 or e2 is not Int type in a sub_class\n";
    }
    type = Int;
    return type;
}

Symbol mul_class::check_type() {
    Symbol e1_type = e1->check_type();
    Symbol e2_type = e2->check_type();
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(this) << "the e1 or e2 is not Int type in a mul_class\n";
    }
    type = Int;
    return type;
}

Symbol divide_class::check_type() {
    Symbol e1_type = e1->check_type();
    Symbol e2_type = e2->check_type();
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(this) << "the e1 or e2 is not Int type in a divide_class\n";
    }
    type = Int;
    return type;
}

Symbol neg_class::check_type() {
    Symbol expr_type = e1->check_type();
    if (expr_type != Int) {
        classtable->semant_error(this) << "The neg_class is not int\n";
    }
    type = Int;
    return type;
}

Symbol lt_class::check_type() {
    Symbol e1_type = e1->check_type();
    Symbol e2_type = e2->check_type();
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(this) << "The e1 or e2 is not Int type in lt class\n";
    }
    type = Bool;
    return type;
}

Symbol eq_class::check_type() {
    Symbol e1_type = e1->check_type();
    Symbol e2_type = e2->check_type();

    if (e1_type != Int && e1_type != Bool && e1_type != Str) {
        classtable->semant_error(this) << "The e1 type error(not Int, Bool, Str) in eq_class\n";
    }
    if (e2_type != Int && e2_type != Bool && e2_type != Str) {
        classtable->semant_error(this) << "The e2 type error(not Int, Bool, Str) in eq_class\n";
    }
    if (e2_type != e1_type) {
        classtable->semant_error(this) << "The e1 type != e2 type\n";
    }
    type = Bool;
    return Object;
}

Symbol leq_class::check_type() {
    Symbol e1_type = e1->check_type();
    Symbol e2_type = e2->check_type();
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(this) << "The e1 or e2 is not Int type in leq class\n";
    }
    type = Bool;
    return type;
}

Symbol comp_class::check_type() {
    Symbol e1_type = e1->check_type();
    if (e1_type != Bool) {
        classtable->semant_error(this) << "the e1 is not Bool in comp_class\n";
    }
    type = Bool;
    return type;
}

Symbol int_const_class::check_type() {
    type = Int;
    return type;
}

Symbol bool_const_class::check_type() {
    type = Bool;
    return type;
}

Symbol string_const_class::check_type() {
    type = Str;
    return type;
}

Symbol new__class::check_type() {
    Symbol tp_name = type_name;
    if (tp_name == SELF_TYPE) {
        classtable->semant_error(this) << "Can't new a SELF_TYPE object\n";
        type = Object;
    } else if (classtable->get_class_byname(tp_name) == nullptr) {
        classtable->semant_error(this) << "The Object type not declare\n";
        type = Object;
    } else {
        type = tp_name;
    }
    return type;
}

Symbol isvoid_class::check_type() {
    Symbol e1_type = e1->check_type();
    type = Bool;
    return type;
}

Symbol no_expr_class::check_type() {
    type = No_type;
    return type;
}

Symbol object_class::check_type() {
    if (name == self) {
        type = dynamic_cast<class__class*>(classtable->get_curr_class())->get_name();
        return type;
    }
    Symbol *object_type = objectEnv.lookup(name);
    if (object_type == nullptr) {
        classtable->semant_error(this) << "the object type "<< name <<" is not declare\n";
        type = Object;
        return type;
    }
    type = *object_type;
    return type;
}

Symbol assign_class::check_type() {
    Symbol *id_type = objectEnv.lookup(name); // 这个变量是之前已经声明过的变量
    Symbol expr_type = expr->check_type();
    // classtable->semant_debug() << "the id type : " << *id_type << " the expr type is " << expr_type << '\n';
    if (id_type == nullptr) {
        classtable->semant_error(this)
                << name <<" the name of object is not defined\n";
        type = expr_type;
        return type;
    }
    if (!type_less_or_equal(expr_type, *id_type)) {
        classtable->semant_error(this)
                << "the type of expr: " << expr_type << "not <= the type of id: " << *id_type << "\n";
        type = Object;
        return type;
    }
    type = expr_type;
    return type;
}

void ClassTable::test_find_lc_root() {
    for (auto name_i : class_name_map_) {
        for (auto name_j : class_name_map_) {
            semant_error() << name_i.first << " and " << name_j.first
            << " lc root is " << find_lastcommon_root(name_i.second, name_j.second) << "\n";
        }
    }
}

void program_class::semant()
{
    initialize_constants();
    /* ClassTable constructor may do some semantic analysis */
    classtable = new ClassTable(classes);  // 根据classes的list可以得到一个ClassTable
    /* some semantic analysis code may go here */
    // classtable->show_chains();
    // classtable->test_find_lc_root();
    classtable->check_and_install();
    if (classtable->errors()) {
	    cerr << "Compilation halted due to static semantic errors." << endl;
	    exit(1);
    }
}


