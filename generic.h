#ifndef GENERIC_H
#define GENERIC_H

#include "tree.h"


tree build_string_const(const char* string, int isConst);


tree build_print_int(location_t loc, tree int_expr, bool nl = true);


tree build_print_string(location_t loc, tree string);


tree build_exit();


tree build_scan_int(location_t loc, tree var_expr);


tree build_global_var(const string & name);


tree build_string_wrapper(const char * name, bool nl = true);


tree build_for(tree iter, tree endVal, bool to, tree statement);


tree build_while(tree condition, tree statement);


tree build_main2(tree stmts);


tree build_main1();


tree build_function(const string & name, tree params, tree param_decl, tree resdecl, tree vars, tree stmts);


tree build_function_dec(const string & name, tree params, tree param_decl, bool isFunc);


tree bind_block_to_func(tree fndecl, tree resdecl, tree vars, tree stmts);


tree build_dec(tree n);


tree build_inc(tree n);


#endif // GENERIC_H