//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <iostream>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"
#define yylineno curr_lineno;
extern int yylineno;

inline Boolean copy_Boolean(Boolean b) {return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream& stream, int padding, Boolean b)
	{ stream << pad(padding) << (int) b << "\n"; }

void dump_Symbol(ostream& stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class Program_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

#define Program_EXTRAS                          \
virtual void semant() = 0;			\
virtual void dump_with_types(ostream&, int) = 0; 



#define program_EXTRAS                          \
void semant();     				\
void dump_with_types(ostream&, int);            

#define Class__EXTRAS                   \
virtual Symbol get_filename() = 0;      \
virtual void dump_with_types(ostream&,int) = 0; 


#define class__EXTRAS                                 \
Symbol get_filename() { return filename; }             \
void dump_with_types(ostream&,int);                   \
Symbol get_name() const { return name; }                \
Symbol get_parent() const { return parent; }          \
Features get_features() const { return features; }

#define Feature_EXTRAS                                        \
virtual void dump_with_types(ostream&,int) = 0;               \
virtual bool is_attr() = 0;                                   \
virtual Symbol get_name() = 0;

#define Feature_SHARED_EXTRAS                                       \
void dump_with_types(ostream&,int);    





#define Formal_EXTRAS                              \
virtual void dump_with_types(ostream&,int) = 0;    \
virtual Symbol get_type() = 0;                     \
virtual Symbol get_name() = 0;

#define formal_EXTRAS                           \
void dump_with_types(ostream&,int);             \
Symbol get_type() { return type_decl; }         \
Symbol get_name() { return name; }

#define Case_EXTRAS                             \
virtual void dump_with_types(ostream& ,int) = 0;\
virtual Symbol check_type() = 0;

#define branch_EXTRAS                                   \
void dump_with_types(ostream& ,int);            \
Symbol check_type();


#define Expression_EXTRAS                    \
Symbol type;                                 \
Symbol get_type() { return type; }           \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void dump_with_types(ostream&,int) = 0;  \
void dump_type(ostream&, int);               \
virtual Symbol check_type() = 0;                \
Expression_class() { type = (Symbol) NULL; } \


#define Expression_SHARED_EXTRAS           \
void dump_with_types(ostream&,int);

#define method_EXTRAS \
bool is_attr()  { return false; }   \
Symbol get_name() { return name; }  \
Formals get_formals() { return formals; } \
Expression get_expr() { return expr; }    \
Symbol get_returntype() { return return_type; }


#define attr_EXTRAS \
bool is_attr() { return true; } \
Symbol get_name() { return name;} \
Symbol get_type() { return type_decl; } \
Expression get_expr() { return init; }  \

#define assign_EXTRAS \
Symbol check_type();

#define static_dispatch_EXTRAS \
Symbol check_type();

#define dispatch_EXTRAS \
Symbol check_type();

#define cond_EXTRAS \
Symbol check_type();

#define loop_EXTRAS \
Symbol check_type();

#define typcase_EXTRAS \
Symbol check_type();

#define block_EXTRAS \
Symbol check_type();

#define let_EXTRAS \
Symbol check_type();

#define plus_EXTRAS \
Symbol check_type();

#define sub_EXTRAS \
Symbol check_type();

#define mul_EXTRAS \
Symbol check_type();

#define divide_EXTRAS \
Symbol check_type();

#define neg_EXTRAS \
Symbol check_type();

#define lt_EXTRAS \
Symbol check_type();

#define eq_EXTRAS \
Symbol check_type();

#define leq_EXTRAS \
Symbol check_type();

#define comp_EXTRAS \
Symbol check_type();

#define int_const_EXTRAS \
Symbol check_type();

#define bool_const_EXTRAS \
Symbol check_type();

#define string_const_EXTRAS \
Symbol check_type();

#define new__EXTRAS \
Symbol check_type();

#define isvoid_EXTRAS   \
Symbol check_type();

#define no_expr_EXTRAS      \
Symbol check_type();

#define object_EXTRAS \
Symbol check_type();


#endif

