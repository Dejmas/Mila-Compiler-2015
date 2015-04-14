#include "config.h"
#include "system.h"
#include "ansidecl.h"
#include "coretypes.h"

#include "diagnostic-core.h"
#include "input.h"

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

tree build_global_variable_i();
tree declaration_block_sample_function();
tree build_factorial_function();
tree build_dummy_function(tree variable_global_variable_decl);
tree build_main(tree variable_global_variable_decl, tree dummy_function_decl, tree factorial_function_decl);
