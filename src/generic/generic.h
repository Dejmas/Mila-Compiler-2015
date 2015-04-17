#ifndef GENERIC_H
#define GENERIC_H

#include "tree.h"

/**
 * @brief Constructs a tree representing a string. The string can be either constant or not.
 * 
 * @param string The string to be represented.
 * @param isConst A flag indicating if the string is a constant.
 * @return tree The constructed tree node representing the string constant.
 */
tree build_string_const(const char* string, int isConst);

/**
 * @brief Constructs a tree representing an integer print statement.
 * 
 * @param loc The location of the print statement.
 * @param int_expr The tree node representing the integer expression to be printed.
 * @param nl Optional parameter indicating if a newline should be printed (default is true).
 * @return tree The constructed tree node representing the print statement.
 */
tree build_print_int(location_t loc, tree int_expr, bool nl = true);

/**
 * @brief Constructs a tree representing a string print statement.
 * 
 * @param loc The location of the print statement.
 * @param string The tree node representing the string to be printed.
 * @return tree The constructed tree node representing the print statement.
 */
tree build_print_string(location_t loc, tree string);

/**
 * @brief Constructs a tree representing an exit statement.
 * 
 * @return tree The constructed tree node representing the exit statement.
 */
tree build_exit();

/**
 * @brief Constructs a tree representing an integer scan statement.
 * 
 * @param loc The location of the scan statement.
 * @param var_expr The tree node representing the variable where the scanned integer will be stored.
 * @return tree The constructed tree node representing the scan statement.
 */
tree build_scan_int(location_t loc, tree var_expr);

/**
 * @brief Constructs a tree representing a global variable declaration.
 * 
 * @param name The name of the global variable.
 * @return tree The constructed tree node representing the global variable declaration.
 */
tree build_global_var(const string & name);

/**
 * @brief Constructs a tree representing a string wrapper.
 * 
 * @param name The name of the string.
 * @param nl Optional parameter indicating if a newline should be included (default is true).
 * @return tree The constructed tree node representing the string wrapper.
 */
tree build_string_wrapper(const char * name, bool nl = true);

/**
 * @brief Constructs a tree representing a for loop.
 * 
 * @param iter The tree node representing the loop iterator.
 * @param endVal The tree node representing the end value of the loop.
 * @param to If false, the loop counts down; otherwise, it counts up.
 * @param statement The tree node representing the statement to be executed in each loop iteration.
 * @return tree The constructed tree node representing the for loop.
 */
tree build_for(tree iter, tree endVal, bool to, tree statement);

/**
 * @brief Constructs a tree representing a while loop.
 * 
 * @param condition The tree node representing the loop condition.
 * @param statement The tree node representing the statement to be executed while the condition is true.
 * @param to If false, the loop counts down; otherwise, it counts up.
 * @return tree The constructed tree node representing the while loop.
 */
tree build_while(tree condition, tree statement);

/**
 * @brief Constructs a tree representing the main function with statements.
 * 
 * @param stmts The tree node representing the statements in the main function.
 * @return tree The constructed tree node representing the main function.
 */
tree build_main2(tree stmts);

/**
 * @brief Constructs a tree representing the main function without statements.
 * 
 * @return tree The constructed tree node representing the main function.
 */
tree build_main1();

/**
 * @brief Constructs a tree representing a function definition.
 * 
 * @param name The name of the function.
 * @param params The tree node representing the function parameters.
 * @param param_decl The tree node representing the parameter declarations.
 * @param resdecl The tree node representing the result declaration.
 * @param vars The tree node representing the local variables.
 * @param stmts The tree node representing the statements in the function.
 * @return tree The constructed tree node representing the function definition.
 */
tree build_function(const string & name, tree params, tree param_decl, tree resdecl, tree vars, tree stmts);

/**
 * @brief Constructs a tree representing a function declaration.
 * 
 * @param name The name of the function.
 * @param params The tree node representing the function parameters.
 * @param param_decl The tree node representing the parameter declarations.
 * @param isFunc A flag indicating if the declaration is for a function.
 * @return tree The constructed tree node representing the function declaration.
 * 
 * @note This function is marked as a FIXME, indicating it may require additional work or revision.
 */
tree build_function_dec(const string & name, tree params, tree param_decl, bool isFunc);

/**
 * @brief Binds a block of code to a function.
 * 
 * @param fndecl The tree node representing the function declaration.
 * @param resdecl The tree node representing the result declaration.
 * @param vars The tree node representing the local variables.
 * @param stmts The tree node representing the statements in the function.
 * @return tree The constructed tree node representing the bound function.
 */
tree bind_block_to_func(tree fndecl, tree resdecl, tree vars, tree stmts);

/**
 * @brief Constructs a tree representing a decrement operation.
 * 
 * @param n The tree node representing the variable to be decremented.
 * @return tree The constructed tree node representing the decrement operation.
 */
tree build_dec(tree n);

/**
 * @brief Constructs a tree representing an increment operation.
 * 
 * @param n The tree node representing the variable to be incremented.
 * @return tree The constructed tree node representing the increment operation.
 */
tree build_inc(tree n);


#endif // GENERIC_H