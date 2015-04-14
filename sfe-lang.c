#include "sfe-lang.h"

#include "tree-iterator.h"

#include "print-tree.h"
#include "opts.h"

#include "stringpool.h"

tree build_string_constant(const char* string, int isConst) {
  int length = strlen(string);
  // the string is just array of const chars so index_type and const_char_type are obviously needed, size_int calls build_int_cst with type stk_sizetype (normal representation of size in bytes) and the size is the size of the string
  tree index_type = build_index_type(size_int(length));
  tree const_char_type = isConst ? build_qualified_type(unsigned_char_type_node, TYPE_QUAL_CONST) : unsigned_char_type_node;
  // then the string type is the array of const chars with known size
  tree string_type = build_array_type(const_char_type, index_type);
  // for c/c++ not important but some languages may distinguis strings and arrays of chars viz tree.def
  TYPE_STRING_FLAG(string_type) = 1;
  // actual building of string
  tree res = build_string(length+1, string);
  //and the builded tree is of string type
  TREE_TYPE(res) = string_type;
  
  return res;
}

tree build_call_dummy_function(location_t loc, tree fndecl) {
  tree * args_vec = XNEWVEC( tree, 1 );
  args_vec[0] = build_int_cst(integer_type_node, 100);

  tree call = build_call_expr_loc_array( loc, fndecl, 1, args_vec );
  SET_EXPR_LOCATION(call, loc);
  TREE_USED(call) = true;
  
  return call;
}

tree build_print_integer_expr (location_t loc, tree int_expr) {
  
  tree string = build_string_constant( (const char*) "%d\n", true );
  
  tree * args_vec = XNEWVEC( tree, 2 );
  args_vec[0] = build1(ADDR_EXPR, build_pointer_type(TREE_TYPE(string)), string);
  args_vec[1] = int_expr;

  tree params = NULL_TREE;
  chainon( params, tree_cons (NULL_TREE, TREE_TYPE(args_vec[0]), NULL_TREE) );
  chainon( params, tree_cons (NULL_TREE, TREE_TYPE(args_vec[1]), NULL_TREE) );

  // function parameters
  tree param_decl = NULL_TREE;

  tree resdecl = build_decl (BUILTINS_LOCATION, RESULT_DECL, NULL_TREE, integer_type_node);
  DECL_ARTIFICIAL(resdecl) = true;
  DECL_IGNORED_P(resdecl) = true;
  
  tree fntype = build_function_type( TREE_TYPE(resdecl), params );
  tree fndecl = build_decl( UNKNOWN_LOCATION, FUNCTION_DECL, get_identifier("printf"), fntype );
  DECL_ARGUMENTS(fndecl) = param_decl;
  
  DECL_RESULT( fndecl ) = resdecl;
  
  DECL_ARTIFICIAL(resdecl) = true;
  DECL_IGNORED_P(resdecl) = true;
  DECL_EXTERNAL( fndecl ) = true;

  tree call = build_call_expr_loc_array( loc, fndecl, 2, args_vec );
  SET_EXPR_LOCATION(call, loc);
  TREE_USED(call) = true;
  
  return call;
}

tree build_scan_integer (location_t loc, tree var_expr) {
  tree string = build_string_constant( (const char*) "%d", true );
  
  tree * args_vec = XNEWVEC( tree, 2 );
  args_vec[0] = build1(ADDR_EXPR, build_pointer_type(TREE_TYPE(string)), string);
  args_vec[1] = build1(ADDR_EXPR, build_pointer_type(TREE_TYPE(var_expr)), var_expr);

  tree params = NULL_TREE;
  chainon( params, tree_cons (NULL_TREE, TREE_TYPE(args_vec[0]), NULL_TREE) );
  chainon( params, tree_cons (NULL_TREE, TREE_TYPE(args_vec[1]), NULL_TREE) );

  // function parameters
  tree param_decl = NULL_TREE;

  tree resdecl = build_decl (BUILTINS_LOCATION, RESULT_DECL, NULL_TREE, integer_type_node);
  DECL_ARTIFICIAL(resdecl) = true;
  DECL_IGNORED_P(resdecl) = true;
  
  tree fntype = build_function_type( TREE_TYPE(resdecl), params );
  tree fndecl = build_decl( UNKNOWN_LOCATION, FUNCTION_DECL, get_identifier("scanf"), fntype );
  DECL_ARGUMENTS(fndecl) = param_decl;
  
  DECL_RESULT( fndecl ) = resdecl;
  
  DECL_ARTIFICIAL(resdecl) = true;
  DECL_IGNORED_P(resdecl) = true;
  DECL_EXTERNAL( fndecl ) = true;

  tree call = build_call_expr_loc_array( loc, fndecl, 2, args_vec );
  SET_EXPR_LOCATION(call, loc);
  TREE_USED(call) = true;
  
  return call;
}

tree build_print_call_factorial(location_t loc, tree factorial_function_decl, tree parameter) {
  tree * args_vec = XNEWVEC( tree, 1 );
  args_vec[0] = parameter;

  tree call = build_call_expr_loc_array( loc, factorial_function_decl, 1, args_vec );
  SET_EXPR_LOCATION(call, loc);
  TREE_USED(call) = true;
  
  return build_print_integer_expr(loc, call);
}

tree dummy_then_block() {
  tree then_decls = NULL_TREE;
  
  tree then_stmts = alloc_stmt_list ();
  
  tree print_stmt = build_print_integer_expr(UNKNOWN_LOCATION, build_int_cst(integer_type_node, 40));
  append_to_statement_list(print_stmt, &then_stmts);
  
  // bind main block with statements
  tree then_bind = build3( BIND_EXPR, void_type_node, then_decls, NULL_TREE, NULL_TREE );
  BIND_EXPR_BODY(then_bind) = then_stmts;
  TREE_SIDE_EFFECTS(then_bind) = true;
  
  return then_bind;
}

tree dummy_else_block_2() {

  tree else_decls = NULL_TREE;
  
  tree variable_k = build_decl (UNKNOWN_LOCATION, VAR_DECL, get_identifier("k"), integer_type_node);
  TREE_ADDRESSABLE(variable_k) = true;
  TREE_USED(variable_k) = true;
  
  TREE_CHAIN( variable_k ) = else_decls;
  else_decls = variable_k;
  
  DECL_INITIAL(variable_k) = build_int_cst(integer_type_node, 30);
  
  tree else_stmts = alloc_stmt_list ();
  
  append_to_statement_list(build1(DECL_EXPR, void_type_node, variable_k), &else_stmts);
  
  tree print_stmt = build_print_integer_expr(UNKNOWN_LOCATION, variable_k);
  append_to_statement_list(print_stmt, &else_stmts);
  
  // bind main block with statements
  tree else_bind = build3( BIND_EXPR, void_type_node, else_decls, NULL_TREE, NULL_TREE );
  BIND_EXPR_BODY(else_bind) = else_stmts;
  TREE_SIDE_EFFECTS(else_bind) = true;
  
  return else_bind;
}

tree dummy_else_block() {
  
  tree else_decls = NULL_TREE;
  
  tree else_stmts = alloc_stmt_list ();
  
  tree else2_block = dummy_else_block_2();
  append_to_statement_list(else2_block, &else_stmts);
  
  tree print_stmt = build_print_integer_expr(UNKNOWN_LOCATION, build_int_cst(integer_type_node, 30));
  append_to_statement_list(print_stmt, &else_stmts);
  
  // bind main block with statements
  tree else_bind = build3( BIND_EXPR, void_type_node, else_decls, NULL_TREE, NULL_TREE );
  BIND_EXPR_BODY(else_bind) = else_stmts;
  TREE_SIDE_EFFECTS(else_bind) = true;
  
  return else_bind;
}

tree build_dummy_if_stmt () {
  
  tree cond = build2(EQ_EXPR, integer_type_node, build_int_cst(integer_type_node, 1), build_int_cst(integer_type_node, 2));
  
  tree then_block = dummy_then_block();
  tree else_block = dummy_else_block();
  
  tree if_stmt = build3(COND_EXPR, void_type_node, cond, then_block, else_block);
  
  return if_stmt;
}

tree build_print_dec_exit_on_zero(tree variable) {
  
  tree decls = NULL_TREE;
  
  tree stmts = alloc_stmt_list ();
 
  tree print_stmt = build_print_integer_expr(UNKNOWN_LOCATION, variable);
  append_to_statement_list(print_stmt, &stmts);
  
  tree dec_stmt = build2(PREDECREMENT_EXPR, TREE_TYPE(variable), variable, build_int_cst(integer_type_node, 1));
  append_to_statement_list(dec_stmt, &stmts);
  
  tree if_stmt = build_dummy_if_stmt();
  append_to_statement_list(if_stmt, &stmts);
  
  tree exit_stmt = build1(EXIT_EXPR, void_type_node, build2(EQ_EXPR, integer_type_node, build_int_cst(integer_type_node, 0), variable));
  append_to_statement_list(exit_stmt, &stmts);
  
  // bind main block with statements
  tree bind = build3( BIND_EXPR, void_type_node, decls, NULL_TREE, NULL_TREE );
  BIND_EXPR_BODY(bind) = stmts;
  TREE_SIDE_EFFECTS(bind) = true;
  
  return bind;
}

tree build_dummy_loop_stmt (tree controlVariable) {
  tree body = build_print_dec_exit_on_zero(controlVariable);
  
  tree loop_stmt = build1(LOOP_EXPR, void_type_node, body);
  
  return loop_stmt;
}

tree build_main(tree variable_global_variable_decl, tree dummy_function_decl, tree factorial_function_decl) {
  /*
  int main(void)
  {
    const char * array;
    int * pk, *pj, *pi;
    int k,j,i;
    array = "abcd efgh";
    pi = &i;
  }
  */
  // function parameters
  tree param_decl = NULL_TREE;

  // parameter types
  tree params = NULL_TREE;
  
  tree resdecl = build_decl (BUILTINS_LOCATION, RESULT_DECL, NULL_TREE, integer_type_node);

  // then the type of the function and its declaration -- external - the code for it is somewhere else and 
  tree fntype = build_function_type( TREE_TYPE(resdecl), params );
  tree fndecl = build_decl( UNKNOWN_LOCATION, FUNCTION_DECL, get_identifier("main"), fntype );
  DECL_ARGUMENTS(fndecl) = param_decl;
  
  DECL_RESULT( fndecl ) = resdecl;
  
  TREE_STATIC(fndecl) = true;
  TREE_PUBLIC( fndecl ) = true;

  tree decls = NULL_TREE;
  
  // Declare a variables
  tree intArray = build_decl (UNKNOWN_LOCATION, VAR_DECL, get_identifier("array"), build_array_type(unsigned_char_type_node, build_index_type(size_int(9))));
  TREE_ADDRESSABLE(intArray) = true;
  TREE_USED(intArray) = true;
  
  TREE_CHAIN( intArray ) = decls;
  decls = intArray;
  
  tree variable_pk = build_decl (UNKNOWN_LOCATION, VAR_DECL, get_identifier("pk"), build_pointer_type(integer_type_node));
  TREE_ADDRESSABLE(variable_pk) = true;
  TREE_USED(variable_pk) = true;
  
  TREE_CHAIN( variable_pk ) = decls;
  decls = variable_pk;

  tree variable_pj = build_decl (UNKNOWN_LOCATION, VAR_DECL, get_identifier("pj"), build_pointer_type(integer_type_node));
  TREE_ADDRESSABLE(variable_pj) = true;
  TREE_USED(variable_pj) = true;
  
  TREE_CHAIN( variable_pj ) = decls;
  decls = variable_pj;
  
  tree variable_pi = build_decl (UNKNOWN_LOCATION, VAR_DECL, get_identifier("pi"), build_pointer_type(integer_type_node));
  TREE_ADDRESSABLE(variable_pi) = true;
  TREE_USED(variable_pi) = true;
  
  TREE_CHAIN( variable_pi ) = decls;
  decls = variable_pi;
  
  tree variable_k = build_decl (UNKNOWN_LOCATION, VAR_DECL, get_identifier("k"), integer_type_node);
  TREE_ADDRESSABLE(variable_k) = true;
  TREE_USED(variable_k) = true;
  
  TREE_CHAIN( variable_k ) = decls;
  decls = variable_k;
  
  tree variable_j = build_decl (UNKNOWN_LOCATION, VAR_DECL, get_identifier("j"), integer_type_node);
  TREE_ADDRESSABLE(variable_j) = true;
  TREE_USED(variable_j) = true;
  
  TREE_CHAIN( variable_j ) = decls;
  decls = variable_j;
  
  tree variable_i = build_decl (UNKNOWN_LOCATION, VAR_DECL, get_identifier("i"), integer_type_node);
  TREE_ADDRESSABLE(variable_i) = true;
  TREE_USED(variable_i) = true;
  
  TREE_CHAIN( variable_i ) = decls;
  decls = variable_i;
  
//  DECL_INITIAL(intArray) = build_string_constant((const char*) "abcd efgh", false);
 // DECL_INITIAL(variable_pi) = build1(ADDR_EXPR, build_pointer_type(TREE_TYPE(variable_i)), variable_i);
  
  // Create main block
  tree block = build_block(decls, NULL_TREE, NULL_TREE, NULL_TREE);
  TREE_USED(block) = true;
  
  // allocate the statement list
  tree stmts = alloc_stmt_list ();
/*
  append_to_statement_list(build1(DECL_EXPR, void_type_node, variable_i), &stmts);
  
  append_to_statement_list(build1(DECL_EXPR, void_type_node, variable_j), &stmts);
    
  append_to_statement_list(build1(DECL_EXPR, void_type_node, variable_k), &stmts);
  
  append_to_statement_list(build1(DECL_EXPR, void_type_node, variable_pi), &stmts);
  
  append_to_statement_list(build1(DECL_EXPR, void_type_node, variable_pj), &stmts);
  
  append_to_statement_list(build1(DECL_EXPR, void_type_node, variable_pk), &stmts);
  
  append_to_statement_list(build1(DECL_EXPR, void_type_node, intArray), &stmts);
//*/  
  tree print_array_element = build_print_integer_expr(UNKNOWN_LOCATION, build4(ARRAY_REF, TREE_TYPE(TREE_TYPE(intArray)), intArray, build_int_cst(integer_type_node, 0), NULL_TREE, NULL_TREE));
  append_to_statement_list(print_array_element, &stmts);
  
  tree modify_array_element = build2(MODIFY_EXPR, TREE_TYPE(TREE_TYPE(intArray)), build4(ARRAY_REF, TREE_TYPE(TREE_TYPE(intArray)), intArray, build_int_cst(integer_type_node, 0), NULL_TREE, NULL_TREE), build_int_cst(unsigned_char_type_node, 98));
  append_to_statement_list(modify_array_element, &stmts);
  
  tree print_array_element_2 = build_print_integer_expr(UNKNOWN_LOCATION, build4(ARRAY_REF, TREE_TYPE(TREE_TYPE(intArray)), intArray, build_int_cst(integer_type_node, 0), NULL_TREE, NULL_TREE));
  append_to_statement_list(print_array_element_2, &stmts);
  
  tree print_global_variable_i = build_print_integer_expr(UNKNOWN_LOCATION, variable_global_variable_decl);
  append_to_statement_list(print_global_variable_i, &stmts);
  
  tree set_global_variable_decl = build2(INIT_EXPR, TREE_TYPE(variable_global_variable_decl), variable_global_variable_decl, build_int_cst(integer_type_node, 5));
  append_to_statement_list(set_global_variable_decl, &stmts);
  
  tree set_pk = build2(INIT_EXPR, TREE_TYPE(variable_pk), variable_pk, build1(ADDR_EXPR, build_pointer_type(TREE_TYPE(variable_k)), variable_k));
  append_to_statement_list(set_pk, &stmts);
  
  tree set_pj = build2(INIT_EXPR, TREE_TYPE(variable_pj), variable_pj, build1(ADDR_EXPR, build_pointer_type(TREE_TYPE(variable_j)), variable_j));
  append_to_statement_list(set_pj, &stmts);
  
  tree set_j = build2(INIT_EXPR, TREE_TYPE(variable_j), variable_j, build_int_cst(integer_type_node, 200));
  append_to_statement_list(set_j, &stmts);

  tree set_i = build2(MODIFY_EXPR, TREE_TYPE(variable_i), variable_i, build_scan_integer(UNKNOWN_LOCATION, variable_j));
  append_to_statement_list(set_i, &stmts);
  
  tree print_i_stmt = build_print_integer_expr(UNKNOWN_LOCATION, variable_i);
  append_to_statement_list(print_i_stmt, &stmts);
  
  tree print_j_from_pointer_stmt = build_print_integer_expr(UNKNOWN_LOCATION, build1(INDIRECT_REF, TREE_TYPE(TREE_TYPE(variable_pj)), variable_pj));
  append_to_statement_list(print_j_from_pointer_stmt, &stmts);
  
  tree call_dummy_function_stmt = build_call_dummy_function(UNKNOWN_LOCATION, dummy_function_decl);
  append_to_statement_list(call_dummy_function_stmt, &stmts);
  
  tree if_stmt = build_dummy_if_stmt();
  append_to_statement_list(if_stmt, &stmts);
  
  tree loop_stmt = build_dummy_loop_stmt(variable_j);
  append_to_statement_list(loop_stmt, &stmts);
  
  tree print_factorial = build_print_call_factorial(UNKNOWN_LOCATION, factorial_function_decl, variable_global_variable_decl);
  append_to_statement_list(print_factorial, &stmts);
  
  tree return_stmt = build1(RETURN_EXPR, void_type_node, build2(MODIFY_EXPR, TREE_TYPE(integer_type_node), resdecl, build_int_cst(integer_type_node, 10)));
  append_to_statement_list(return_stmt, &stmts);
    
  // bind main block with statements
  tree bind = build3( BIND_EXPR, void_type_node, BLOCK_VARS(block), NULL_TREE, block );
  BIND_EXPR_BODY(bind) = stmts;
  TREE_SIDE_EFFECTS(bind) = true;

  BLOCK_SUPERCONTEXT(block) = fndecl;
  DECL_INITIAL(fndecl) = block;
  DECL_SAVED_TREE(fndecl) = bind;

  return fndecl;
}

tree build_dummy_function(tree variable_global_variable_decl) {
  // function parameters
  tree param_decl = NULL_TREE;
  
  tree number = build_decl (UNKNOWN_LOCATION, PARM_DECL, get_identifier("number"), integer_type_node);
  DECL_ARG_TYPE(number) = integer_type_node;
  param_decl = chainon(param_decl, number);

  // parameter types
  tree params = NULL_TREE;
  chainon( params, tree_cons (NULL_TREE, TREE_TYPE(number), NULL_TREE) );
  
  tree resdecl = build_decl (BUILTINS_LOCATION, RESULT_DECL, NULL_TREE, void_type_node);

  // then the type of the function and its declaration -- external - the code for it is somewhere else and 
  tree fntype = build_function_type( TREE_TYPE(resdecl), params );
  tree fndecl = build_decl( UNKNOWN_LOCATION, FUNCTION_DECL, get_identifier("dummy"), fntype );
  DECL_ARGUMENTS(fndecl) = param_decl;
  
  DECL_RESULT( fndecl ) = resdecl;
  
  TREE_STATIC(fndecl) = true;
  TREE_PUBLIC( fndecl ) = true;

  tree decls = NULL_TREE;
  
  tree block = build_block(decls, NULL_TREE, NULL_TREE, NULL_TREE);
  TREE_USED(block) = true;
  
  // allocate the statement list
  tree stmts = alloc_stmt_list ();
  
  tree print_number_stmt = build_print_integer_expr(UNKNOWN_LOCATION, number);
  append_to_statement_list(print_number_stmt, &stmts);
  
  tree print_global_variable_i = build_print_integer_expr(UNKNOWN_LOCATION, variable_global_variable_decl);
  append_to_statement_list(print_global_variable_i, &stmts);
  
  tree return_stmt = build1(RETURN_EXPR, void_type_node, NULL_TREE);
  append_to_statement_list(return_stmt, &stmts);
    
  // bind main block with statements
  tree bind = build3( BIND_EXPR, void_type_node, BLOCK_VARS(block), NULL_TREE, block );
  BIND_EXPR_BODY(bind) = stmts;
  TREE_SIDE_EFFECTS(bind) = true;

  BLOCK_SUPERCONTEXT(block) = fndecl;
  DECL_INITIAL(fndecl) = block;
  DECL_SAVED_TREE(fndecl) = bind;

  return fndecl;
}

tree build_factorial_function() {
  // function parameters
  tree param_decl = NULL_TREE;
  
  tree number = build_decl (UNKNOWN_LOCATION, PARM_DECL, get_identifier("number"), integer_type_node);
  DECL_ARG_TYPE(number) = integer_type_node;
  param_decl = chainon(param_decl, number);

  // parameter types
  tree params = NULL_TREE;
  chainon( params, tree_cons (NULL_TREE, TREE_TYPE(number), NULL_TREE) );
  
  tree resdecl = build_decl (BUILTINS_LOCATION, RESULT_DECL, NULL_TREE, integer_type_node);

  // then the type of the function and its declaration -- external - the code for it is somewhere else and 
  tree fntype = build_function_type( TREE_TYPE(resdecl), params );
  tree fndecl = build_decl( UNKNOWN_LOCATION, FUNCTION_DECL, get_identifier("factorial"), fntype );
  DECL_ARGUMENTS(fndecl) = param_decl;
  
  DECL_RESULT( fndecl ) = resdecl;
  
  TREE_STATIC(fndecl) = true;
  TREE_PUBLIC( fndecl ) = true;

  tree decls = NULL_TREE;
  
  tree block = build_block(decls, NULL_TREE, NULL_TREE, NULL_TREE);
  TREE_USED(block) = true;

  // allocate the statement list
  tree stmts = alloc_stmt_list ();
  
    tree then_decls = NULL_TREE;
    
    tree then_stmts = alloc_stmt_list ();
    
    tree then_return = build1(RETURN_EXPR, void_type_node, build2(MODIFY_EXPR, TREE_TYPE(integer_type_node), resdecl, build_int_cst(integer_type_node, 1)));
    append_to_statement_list(then_return, &then_stmts);
    
    // bind main block with statements
    tree then_bind = build3( BIND_EXPR, void_type_node, then_decls, NULL_TREE, NULL_TREE );
    BIND_EXPR_BODY(then_bind) = then_stmts;
    TREE_SIDE_EFFECTS(then_bind) = true;
    
  tree cond = build2(LE_EXPR, integer_type_node, number, build_int_cst(integer_type_node, 1));
  
  tree if_stmt = build3(COND_EXPR, void_type_node, cond, then_bind, NULL_TREE);
  append_to_statement_list(if_stmt, &stmts);
  
  tree * args_vec = XNEWVEC( tree, 1 );
  args_vec[0] = build2(MINUS_EXPR, integer_type_node, number, build_int_cst(integer_type_node, 1));

  tree call = build_call_expr_loc_array( UNKNOWN_LOCATION, fndecl, 1, args_vec );
  SET_EXPR_LOCATION(call, UNKNOWN_LOCATION);
  TREE_USED(call) = true;
  
  tree main_return = build1(RETURN_EXPR, void_type_node, build2(MODIFY_EXPR, TREE_TYPE(integer_type_node), resdecl, build2(MULT_EXPR, integer_type_node, number, call)));
  append_to_statement_list(main_return, &stmts);
  
  // bind main block with statements
  tree bind = build3( BIND_EXPR, void_type_node, BLOCK_VARS(block), NULL_TREE, block );
  BIND_EXPR_BODY(bind) = stmts;
  TREE_SIDE_EFFECTS(bind) = true;

  BLOCK_SUPERCONTEXT(block) = fndecl;
  DECL_INITIAL(fndecl) = block;
  DECL_SAVED_TREE(fndecl) = bind;
  
  return fndecl;
}

tree declaration_block_sample_function() {
  // function parameters
  tree param_decl = NULL_TREE;
  
  // parameter types
  tree params = NULL_TREE;
  
  tree resdecl = build_decl (BUILTINS_LOCATION, RESULT_DECL, NULL_TREE, integer_type_node);

  // then the type of the function and its declaration -- external - the code for it is somewhere else and 
  tree fntype = build_function_type( TREE_TYPE(resdecl), params );
  tree fndecl = build_decl( UNKNOWN_LOCATION, FUNCTION_DECL, get_identifier("decl_block_sample"), fntype );
  DECL_ARGUMENTS(fndecl) = param_decl;
  
  DECL_RESULT( fndecl ) = resdecl;
  
  TREE_STATIC(fndecl) = true;
  TREE_PUBLIC( fndecl ) = true;

  tree decls1 = NULL_TREE;
  
  tree variable_i = build_decl (UNKNOWN_LOCATION, VAR_DECL, get_identifier("i"), integer_type_node);
  TREE_ADDRESSABLE(variable_i) = true;
  TREE_USED(variable_i) = true;
  
  TREE_CHAIN( variable_i ) = decls1;
  decls1 = variable_i;
  
  tree block1 = build_block(decls1, NULL_TREE, NULL_TREE, NULL_TREE);
  TREE_USED(block1) = true;
  
  // allocate the statement list
  tree stmts1 = alloc_stmt_list ();
  
  append_to_statement_list(build1(DECL_EXPR, void_type_node, variable_i), &stmts1);

    tree decls2 = NULL_TREE;
    
    tree variable_k = build_decl (UNKNOWN_LOCATION, VAR_DECL, get_identifier("k"), integer_type_node);
    TREE_ADDRESSABLE(variable_k) = true;
    TREE_USED(variable_k) = true;
    
    TREE_CHAIN( variable_k ) = decls2;
    decls2 = variable_k;
    
    tree stmts2 = alloc_stmt_list ();
    
    tree set_k = build2(MODIFY_EXPR, TREE_TYPE(variable_k), variable_k, build_int_cst(integer_type_node, 30));
    append_to_statement_list(set_k, &stmts2);
    
    tree main_return = build1(RETURN_EXPR, void_type_node, build2(MODIFY_EXPR, TREE_TYPE(integer_type_node), resdecl, variable_i));
    append_to_statement_list(main_return, &stmts2);
    
    // bind main block with statements
    tree bind2 = build3( BIND_EXPR, void_type_node, decls2, NULL_TREE, NULL_TREE );
    BIND_EXPR_BODY(bind2) = stmts2;
    TREE_SIDE_EFFECTS(bind2) = true;
    
  append_to_statement_list(bind2, &stmts1);
  
  // bind main block with statements
  tree bind1 = build3( BIND_EXPR, void_type_node, BLOCK_VARS(block1), NULL_TREE, block1 );
  BIND_EXPR_BODY(bind1) = stmts1;
  TREE_SIDE_EFFECTS(bind1) = true;

  BLOCK_SUPERCONTEXT(block1) = fndecl;
  DECL_INITIAL(fndecl) = block1;
  DECL_SAVED_TREE(fndecl) = bind1;
  
  return fndecl;
}

tree build_global_variable_i() {
  tree global_i = build_decl (UNKNOWN_LOCATION, VAR_DECL, get_identifier("global_i"), integer_type_node);
  TREE_ADDRESSABLE(global_i) = true;
  TREE_USED(global_i) = true;
  TREE_STATIC(global_i) = true;
  TREE_PUBLIC(global_i) = true;
  
  DECL_INITIAL(global_i) = build_int_cst(integer_type_node, 20);
  
  return global_i;
}
