

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <memory>
#include <stack>
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
    /*for (auto in_pair : in_map) {
        printf("%d ", in_pair.second);
    }
    printf("\n");*/
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


ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
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
}

bool ClassTable::NameTypeValid(Symbol name) {
    return name != IO && name != Int && name != Bool && name != Str && name != SELF_TYPE;
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
    ClassTable *classtable = new ClassTable(classes);  // 根据classes的list可以得到一个ClassTable
    /* some semantic analysis code may go here */

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}


