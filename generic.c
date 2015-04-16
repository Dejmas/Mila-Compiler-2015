#include "generic.h"

#include "config.h"
#include "system.h"
#include "ansidecl.h"
#include "coretypes.h"

#include "toplev.h"

#include "diagnostic-core.h"
#include "input.h"

#include "tm.h"

#include "hash-set.h"
#include "machmode.h"
#include "vec.h"
#include "double-int.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "options.h"
#include "wide-int.h"
#include "inchash.h"

#include "tree.h"
#include "fold-const.h"
#include "tree-dump.h"
#include "tree-iterator.h"

#include "tree-ssa-operands.h"
#include "tree-pass.h"
#include "tree-ssa-alias.h"
#include "bitmap.h"
#include "symtab.h"

#include "hard-reg-set.h"
#include "function.h"
#include "langhooks-def.h"
#include "langhooks.h"
#include "stringpool.h"
#include "is-a.h"

#include "gimple-expr.h"

#include "predict.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimplify.h"
#include "ipa-ref.h"
#include "lto-streamer.h"
#include "cgraph.h"
#include "opts.h"

tree build_string_const(const char* string, int isConst) 
{
    int length = strlen(string);
    tree index_type = build_index_type(size_int(length));
    tree const_char_type = isConst ? build_qualified_type(unsigned_char_type_node, 
                                                            TYPE_QUAL_CONST) 
                                   : unsigned_char_type_node;
    tree string_type = build_array_type(const_char_type, index_type);
    TYPE_STRING_FLAG(string_type) = 1;
    tree res = build_string(length+1, string);
    TREE_TYPE(res) = string_type;
    return res;
}

tree build_print_int(location_t loc, tree int_expr, bool nl = true ) 
{
    tree string = build_string_const( nl ? (const char*) "%d\n" : (const char*) "%d", true );

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
    tree fndecl = build_decl( UNKNOWN_LOCATION, FUNCTION_DECL, 
        get_identifier("printf"), fntype );
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

tree build_print_string ( location_t loc, tree string) 
{
    tree * args_vec = XNEWVEC( tree, 1 );
    args_vec[0] = build1(ADDR_EXPR, build_pointer_type(TREE_TYPE(string)), string);

    tree params = NULL_TREE;
    chainon( params, tree_cons (NULL_TREE, TREE_TYPE(args_vec[0]), NULL_TREE) );

    // function parameters
    tree param_decl = NULL_TREE;

    tree resdecl = build_decl (BUILTINS_LOCATION, RESULT_DECL, NULL_TREE, integer_type_node);
    DECL_ARTIFICIAL(resdecl) = true;
    DECL_IGNORED_P(resdecl) = true;

    tree fntype = build_function_type( TREE_TYPE(resdecl), params );
    tree fndecl = build_decl( UNKNOWN_LOCATION, FUNCTION_DECL, 
        get_identifier("printf"), fntype );
    DECL_ARGUMENTS(fndecl) = param_decl;

    DECL_RESULT( fndecl ) = resdecl;

    DECL_ARTIFICIAL(resdecl) = true;
    DECL_IGNORED_P(resdecl) = true;
    DECL_EXTERNAL( fndecl ) = true;

    tree call = build_call_expr_loc_array( loc, fndecl, 1, args_vec );
    SET_EXPR_LOCATION(call, loc);
    TREE_USED(call) = true;

    return call;
}

tree build_exit ( ) 
{
    tree * args_vec = XNEWVEC( tree, 1 );
    args_vec[0] = build_int_cst( integer_type_node, 0 );
    
    tree params = NULL_TREE;
    chainon( params, tree_cons (NULL_TREE, TREE_TYPE(args_vec[0]), NULL_TREE) );
   
    // function parameters
    tree param_decl = NULL_TREE;

    tree resdecl = build_decl (BUILTINS_LOCATION, RESULT_DECL, NULL_TREE, integer_type_node);
    DECL_ARTIFICIAL(resdecl) = true;
    DECL_IGNORED_P(resdecl) = true;

    tree fntype = build_function_type( TREE_TYPE(resdecl), params );
    tree fndecl = build_decl( UNKNOWN_LOCATION, FUNCTION_DECL, get_identifier("exit"), fntype );
    DECL_ARGUMENTS(fndecl) = param_decl;

    DECL_RESULT( fndecl ) = resdecl;

    DECL_ARTIFICIAL(resdecl) = true;
    DECL_IGNORED_P(resdecl) = true;
    DECL_EXTERNAL( fndecl ) = true;

    tree call = build_call_expr_loc_array( UNKNOWN_LOCATION, fndecl, 1, args_vec );
    SET_EXPR_LOCATION(call, UNKNOWN_LOCATION);
    TREE_USED(call) = true;

    return call;
}

tree build_scan_int (location_t loc, tree var_expr) {
  tree string = build_string_const( (const char*) "%d", true );
  
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

tree build_global_var( const char * name ) {
    tree glob_var = build_decl (UNKNOWN_LOCATION, 
      VAR_DECL, 
      get_identifier( name ), 
      integer_type_node
    );
    TREE_ADDRESSABLE(glob_var) = true;
    TREE_USED(glob_var) = true;
    TREE_STATIC(glob_var) = true;
    TREE_PUBLIC(glob_var) = true;
    
    DECL_INITIAL(glob_var) = build_int_cst(integer_type_node, 0);
    
    return glob_var;
}

tree build_string_wrapper( const char * name, bool nl = true ) {
    size_t length = strlen(name) + 2;
    char * data = (char *)malloc(length * sizeof(char)); 
    strcpy(data, name);
    if (nl) {
        data[length-2] = '\n';
        data[length-1] = 0;
    }
    tree index_type = build_index_type(size_int(length));
    tree const_char_type = build_qualified_type(unsigned_char_type_node, TYPE_QUAL_CONST);
    tree string_type = build_array_type(const_char_type, index_type);
    TYPE_STRING_FLAG(string_type) = 1;
    tree res = build_string(length, name );
    TREE_TYPE(res) = string_type;
    free(name);
    return res;
}

/// @param to if false means downto
tree build_for( tree iter, tree endVal, bool to, tree statement ) 
{
    tree decls = NULL_TREE;
    tree stmts = alloc_stmt_list ();

    tree breakCondition = build2( to? GT_EXPR : LT_EXPR, 
        integer_type_node, 
        iter,
        endVal
    );
    tree exit_stmt = build1(EXIT_EXPR, void_type_node, breakCondition );
    append_to_statement_list(exit_stmt, &stmts);

    append_to_statement_list(statement, &stmts);

    tree dec_stmt = build2( to? PREINCREMENT_EXPR : PREDECREMENT_EXPR, 
        TREE_TYPE(iter), 
        iter, 
        build_int_cst(integer_type_node, 1)
    );
    append_to_statement_list(dec_stmt, &stmts);

    // bind main block with statements
    tree bind = build3( BIND_EXPR, void_type_node, decls, NULL_TREE, NULL_TREE );
    BIND_EXPR_BODY(bind) = stmts;
    TREE_SIDE_EFFECTS(bind) = true;

    tree loop_stmt = build1( LOOP_EXPR, void_type_node, bind );
    return loop_stmt;
}

/// @param to if false means downto
tree build_while( tree condition, tree statement ) 
{
    tree decls = NULL_TREE;
    tree stmts = alloc_stmt_list ();

    tree breakCondition = build2( EQ_EXPR, 
        integer_type_node, 
        build_int_cst( integer_type_node, 0 ),
        condition
    );
    tree exit_stmt = build1(EXIT_EXPR, void_type_node, breakCondition );
    append_to_statement_list(exit_stmt, &stmts);

    append_to_statement_list(statement, &stmts);
    // bind main block with statements
    tree bind = build3( BIND_EXPR, void_type_node, decls, NULL_TREE, NULL_TREE );
    BIND_EXPR_BODY(bind) = stmts;
    TREE_SIDE_EFFECTS(bind) = true;

    tree loop_stmt = build1( LOOP_EXPR, void_type_node, bind );
    return loop_stmt;
}

tree build_main2( tree stmts ) 
{
    tree param_decl = NULL_TREE;
    tree params = NULL_TREE;
    tree resdecl = build_decl (BUILTINS_LOCATION, RESULT_DECL, NULL_TREE, integer_type_node);

    // then the type of the function and its declaration
    tree fntype = build_function_type( TREE_TYPE(resdecl), params );
    tree fndecl = build_decl( UNKNOWN_LOCATION, FUNCTION_DECL, get_identifier("main"), fntype );
    DECL_ARGUMENTS(fndecl) = param_decl;

    DECL_RESULT( fndecl ) = resdecl;
    TREE_STATIC(fndecl) = true;
    TREE_PUBLIC( fndecl ) = true;

    tree decls = NULL_TREE;

    // Create main block
    tree block = build_block(decls, NULL_TREE, NULL_TREE, NULL_TREE);
    TREE_USED(block) = true;

    // allocate the statement list
    //tree stmts = alloc_stmt_list ();

    tree return_stmt = build1(RETURN_EXPR, void_type_node, 
    build2(MODIFY_EXPR, 
        TREE_TYPE(integer_type_node), 
        resdecl, 
        build_int_cst(integer_type_node, 0))
    );
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

tree build_main1() 
{
    tree param_decl = NULL_TREE;
    tree params = NULL_TREE;
    tree resdecl = build_decl (BUILTINS_LOCATION, RESULT_DECL, NULL_TREE, integer_type_node);

    // then the type of the function and its declaration
    tree fntype = build_function_type( TREE_TYPE(resdecl), params );
    tree fndecl = build_decl( UNKNOWN_LOCATION, FUNCTION_DECL, get_identifier("main"), fntype );
    DECL_ARGUMENTS(fndecl) = param_decl;

    DECL_RESULT( fndecl ) = resdecl;
    TREE_STATIC(fndecl) = true;
    TREE_PUBLIC( fndecl ) = true;

    tree decls = NULL_TREE;

    // Create main block
    tree block = build_block(decls, NULL_TREE, NULL_TREE, NULL_TREE);
    TREE_USED(block) = true;

    // allocate the statement list
    tree stmts = alloc_stmt_list ();

    tree return_stmt = build1(RETURN_EXPR, void_type_node, 
    build2(MODIFY_EXPR, 
        TREE_TYPE(integer_type_node), 
        resdecl, 
        build_int_cst(integer_type_node, 0))
    );
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
 /*   tree param_decl = NULL_TREE;
    tree params = NULL_TREE;
    tree resdecl = build_decl (BUILTINS_LOCATION, RESULT_DECL, NULL_TREE, integer_type_node);
    tree return_stmt = build1(RETURN_EXPR, void_type_node, 
    build2(MODIFY_EXPR, 
        TREE_TYPE(integer_type_node), 
        resdecl, 
        build_int_cst(integer_type_node, 0))
    );
    append_to_statement_list(return_stmt, &stmts);
*/
tree build_function( const char * name, tree params, tree param_decl, tree resdecl, tree vars, tree stmts ) 
{
    // then the type of the function and its declaration
    tree fntype = build_function_type( TREE_TYPE(resdecl), params );
    tree fndecl = build_decl( UNKNOWN_LOCATION, FUNCTION_DECL, get_identifier( name ), fntype );
    DECL_ARGUMENTS(fndecl) = param_decl;

    DECL_RESULT( fndecl ) = resdecl;
    TREE_STATIC( fndecl ) = true;
    TREE_PUBLIC( fndecl ) = true;

    // Create main block
    tree block = build_block(vars, NULL_TREE, NULL_TREE, NULL_TREE);
    TREE_USED(block) = true;

    // bind main block with statements
    tree bind = build3( BIND_EXPR, void_type_node, BLOCK_VARS(block), NULL_TREE, block );
    BIND_EXPR_BODY(bind) = stmts;
    TREE_SIDE_EFFECTS(bind) = true;

    BLOCK_SUPERCONTEXT(block) = fndecl;
    DECL_INITIAL(fndecl) = block;
    DECL_SAVED_TREE(fndecl) = bind;

    return fndecl;
}


// FIXME do it.
tree build_function_dec( const char * name, tree params, tree param_decl, bool isFunc )
{
    tree fntype = build_function_type( isFunc? integer_type_node 
                                             : void_type_node,
        params 
    );
    tree fndecl = build_decl( UNKNOWN_LOCATION, FUNCTION_DECL, get_identifier( name ), fntype );
    DECL_ARGUMENTS(fndecl) = param_decl;

    TREE_STATIC( fndecl ) = true;
    TREE_PUBLIC( fndecl ) = true;

    return fndecl;
}

tree bind_block_to_func( tree fndecl, tree resdecl, tree vars, tree stmts )
{
    // Create main block
    DECL_RESULT( fndecl ) = resdecl;
    tree block = build_block(vars, NULL_TREE, NULL_TREE, NULL_TREE);
    TREE_USED(block) = true;

    // bind main block with statements
    tree bind = build3( BIND_EXPR, void_type_node, BLOCK_VARS(block), NULL_TREE, block );
    BIND_EXPR_BODY(bind) = stmts;
    TREE_SIDE_EFFECTS(bind) = true;

    BLOCK_SUPERCONTEXT(block) = fndecl;
    DECL_INITIAL(fndecl) = block;
    DECL_SAVED_TREE(fndecl) = bind;

    return fndecl;
}


tree build_dec( tree n )
{
    return build2( PREDECREMENT_EXPR, 
        TREE_TYPE(n), 
        n, 
        build_int_cst(integer_type_node, 1)
    );
}
tree build_inc( tree n )
{
    return build2( PREINCREMENT_EXPR,
        TREE_TYPE(n), 
        n, 
        build_int_cst(integer_type_node, 1)
    );
}
