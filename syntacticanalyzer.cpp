#include "synan.h"
#include <map>
#include <string>
#include <vector>
//#include "/home/tom/mySchool/PJP/gcc/gcc-src/gcc/sfe/lex.yy.c"
#include "lex.yy.c"

struct TFunc{ 
    tree params;
    tree resdecl;
    tree fcdecl;
    map< string, tree >  localVarMap;
};

bool myMatch( const string & a, const char * b )
{
    int i = 0;
    while( i < a.size() )
    {
        if( ! *b ) return false;
        if( a[i] != *b 
         && a[i] != *b + 'a' - 'A' )
            return false;
        b++;
        i++;
    }
    return true;
}


#include <stdio.h>
#include <stdarg.h>
void handleError( const char * format, ... )
{
  char buffer[512];
  va_list args;
  va_start (args, format);
  vsprintf (buffer,format, args);
  printf (buffer);
  va_end (args);
  exit(1);
}

void print( const string & str )
{
    #ifdef DEBUG
    printf( "%s\n", str.c_str() );
    #endif
}

using namespace std;


SyntacticAnalyzer::SyntacticAnalyzer( FILE * inputFile, FILE * outputFile )
{
    yyin = inputFile; 
    yyout = outputFile;
    isFunc = false;
}
SyntacticAnalyzer::~SyntacticAnalyzer()
{
    delete gCurrentToken;
}

bool SyntacticAnalyzer::nextToken()
{
    delete gCurrentToken;
    gCurrentToken = NULL;
    return yylex();
}

void SyntacticAnalyzer::drop( TokenID tid )
{
    if( gCurrentToken->mId == tid )
        nextToken();
    else
    {
        printf( "parsing error: mismatch %s and %s.\n", 
            TokenNames[(int)tid],
            TokenNames[(int)gCurrentToken->mId]
        );
        exit(1);
    }
}
void SyntacticAnalyzer::dropStr( string & str )
{
    if( gCurrentToken->mId == STRING
        || gCurrentToken->mId == IDENTFIER )
    {
        str = gCurrentToken->mData;
        nextToken();
    }
    else
    {
        printf( "parsing error: %s is not string or indentifier.\n", 
            TokenNames[(int)gCurrentToken->mId]
        );
        exit(1);
    }   
}
void SyntacticAnalyzer::dropInt( int & num )
{
    if( gCurrentToken->mId == INTEGER )
    {
        num = gCurrentToken->mValue;
        nextToken();
    }
    else
    {
        printf( "parsing error: %s is not integer.\n", 
            TokenNames[(int)gCurrentToken->mId]
        );
        exit(1);
    }   
}

void SyntacticAnalyzer::parseFile()
{
    fileVec = NULL;
    nextToken();
    S();

    //registerFunction( build_main1() );
    printf( "\nCompleting all...\n" );
}
void SyntacticAnalyzer::S()
{
    string progName;
    drop( KW_PROGRAM );
    dropStr( progName );
    drop( KW_SEMICOLON );
    print( "progName: " );
    // clean data for current program
    varMap.clear();
    funcMap.clear();
    zb( NULL_TREE );
}
void SyntacticAnalyzer::zb( tree globals )
{
    print( "zb" );
    switch( gCurrentToken->mId )
    {
        case KW_PROCEDURE:
        case KW_FUNCTION:
        {
            funcDec();
            localVarMap.clear();
            isFunc = false;
            zb( globals ); 
            break;
        }
        case KW_CONST:
        {
            drop(KW_CONST);
            constbl( varMap ); 
            zb( globals );
            break;
        }
        case KW_VAR:
        {

            drop(KW_VAR);
            globals = varbl( globals, varMap, true );
            zb( globals );
            break;
        }
        default:
            zb2( globals );
            break; 
    }
}
void SyntacticAnalyzer::zb2( tree globals )
{
    print( "zb2" );
    switch( gCurrentToken->mId )
    {
        case KW_BEGIN:
        {
            printf("Starting MAIN...\n");
            tree mainfc = this->main( globals ); 
            printf("Registering MAIN...\n");
            registerFunction(mainfc);
            break;
        }
        default:
            handleError("You need global begin .. end in your code.\n"); 
            break; 
    }
}

void SyntacticAnalyzer::constbl( map<string, tree> & vmap )
{
    print( "constbl" );
    if( gCurrentToken->mId == IDENTFIER )
    {
        string name;
        dropStr( name );
        drop( LO_EQ );
        vmap[ name ] = aritmetic_expression();
        drop( KW_SEMICOLON );
        constbl( vmap );
    }
}

tree SyntacticAnalyzer::varbl( tree decls, map< string, tree > & vmap, bool global )
{
    if( gCurrentToken->mId != IDENTFIER ) return decls;
    print( "varbl" );
    vector<string> names;
    idlist( names );
    drop( KW_COLON );
    int from, to;
    bool isArray = type(from, to);
    drop( KW_SEMICOLON );

    for( unsigned int i = 0; i < names.size(); ++ i )
    {
        string name = names[ i ];
        tree nextVar;
        if( isArray )
        {
            idxMap[ name ] = -from;
            nextVar = build_decl (
                UNKNOWN_LOCATION, 
                VAR_DECL, 
                get_identifier( name.c_str() ), 
                build_array_type(integer_type_node, 
                    build_index_type(size_int(to - from + 1))
                )
            );
            TREE_ADDRESSABLE(nextVar) = true;
            TREE_USED(nextVar) = true;       
        }
        else
        {
            nextVar = build_decl (
                UNKNOWN_LOCATION, 
                VAR_DECL,
                get_identifier( name.c_str() ), 
                integer_type_node
            );
            TREE_ADDRESSABLE(nextVar) = true;
            TREE_USED(nextVar) = true;
            DECL_INITIAL(nextVar) = build_int_cst(integer_type_node, 0);
        }

        printf("'%s'\n", name.c_str() );
        if( global )
        {
            TREE_STATIC( nextVar ) = true;
            TREE_PUBLIC( nextVar ) = true;
            registerGlobalVarible( nextVar );
            varMap[ name ] = nextVar;
        }
        else
        {
            TREE_CHAIN( nextVar ) = decls;
            decls = nextVar;
            localVarMap[ name ] = nextVar;
        }
    }
    return varbl( decls, vmap, global );
}

void SyntacticAnalyzer::idlist( vector< string > & names )
{
    print( "idlist" );
    string name;
    dropStr( name );
    names.push_back( name );
    idlist2( names );
}
void SyntacticAnalyzer::idlist2( vector< string > & names )
{
    print( "idlist2" );
    if( gCurrentToken->mId != KW_COMMA ) return;
    drop( KW_COMMA );
    string name;
    dropStr( name );
    names.push_back( name );
    idlist2( names );
}
/// @return isArray
bool SyntacticAnalyzer::type( int & from, int & to )
{
    print( "type" );
    switch( gCurrentToken->mId )
    {
        case KW_INTEGER:
                drop(KW_INTEGER);
                return false;
        case KW_ARRAY:
                drop(KW_ARRAY);
                // from to under reconstruction
                interval(from, to);
                drop(KW_OF);
                drop(KW_INTEGER);
                return true;
        default:
            handleError("Error int func type\n");
            return false;
    }
}
void SyntacticAnalyzer::interval( int & from, int & to )
{
    drop(KW_LSQBRACKET);
    //aritmetic_expression();
    if( gCurrentToken->mId == OP_MINUS )
    {
        drop( OP_MINUS );
        dropInt( from );
        from = - from;
    }
    else
    {
        dropInt( from );
    }
    drop(KW_DOUBLEDOT);
    //aritmetic_expression();
    if( gCurrentToken->mId == OP_MINUS )
        handleError( "interval of static arry: only start can be negative." );
    dropInt( to );
    drop(KW_RSQBRACKET);
    print( "interval " );
}

void SyntacticAnalyzer::funcDec()
{
    print( "funcDec" );
    localVarMap.clear();
    string name;
    tree param_decl = NULL_TREE, 
            params     = NULL_TREE,
            resdecl    = NULL_TREE,
            var        = NULL_TREE,
            stmt       = NULL_TREE;
    bool now;
    bool declared;
    isFunc = gCurrentToken->mId == KW_FUNCTION;
    
    drop( isFunc? KW_FUNCTION : KW_PROCEDURE );
    dropStr(name);
    drop(KW_LBRACKET);
    parameters(param_decl, params);
    drop(KW_RBRACKET);
    //tree fcdecl = findFunction(name, true);
    TFunc * fc = findFunctionFw( name ); 

    if( isFunc ) 
    {
        drop(KW_COLON);
        int from, to;
        type(from, to);
    }
    drop(KW_SEMICOLON);
    if( fc == NULL )
    {   
        tree fcdecl = build_function_dec( name, params, param_decl, isFunc );
        funcMap[ name ] = fcdecl;
        fc = new TFunc();
        fc -> params = params;
        // fc -> param_decl = param_decl;
        fc -> fcdecl = fcdecl;
        resdecl = build_decl( BUILTINS_LOCATION, RESULT_DECL, NULL_TREE,
                    isFunc? integer_type_node : void_type_node );
        localVarMap[ string( "#_return_" ) ] = localVarMap[ name ] = resdecl;
        fc -> resdecl = resdecl;
        fc -> localVarMap = localVarMap;
    }

    now = funcDec2( var, stmt );
    drop(KW_SEMICOLON);
    if( now )
    {
        append_to_statement_list( build_return(), &stmt );
        fc -> fcdecl = bind_block_to_func( fc -> fcdecl, fc -> resdecl, var, stmt );
        registerFunction( fc -> fcdecl );
        localVarMap = fc -> localVarMap;
        delete fc;
    }
}

void SyntacticAnalyzer::parameters( tree & param_decl, tree & params )
{
    print( "parameters" );
    if( gCurrentToken->mId == KW_RBRACKET ) return;
    tree vardec;
    vector<string> names;
    idlist(names);
    drop(KW_COLON);
    int from, to;
    bool isArray = type(from, to);

    for( unsigned int i = 0; i < names.size(); ++ i )
    {
        string name = names[ i ];
        tree nextVar;
        if( isArray )
        {
            idxMap[ name ] = -from;
            nextVar = build_decl (
                UNKNOWN_LOCATION, 
                PARM_DECL, 
                get_identifier( name.c_str() ), 
                build_array_type(integer_type_node, 
                    build_index_type(size_int(to - from + 1))
                )
            );
        }
        else
        {
            nextVar = build_decl (
                UNKNOWN_LOCATION, 
                PARM_DECL,
                get_identifier( name.c_str() ), 
                integer_type_node
            );
            DECL_ARG_TYPE(nextVar) = integer_type_node;
        }
        
        param_decl = chainon(param_decl, nextVar);
        chainon( params, tree_cons (NULL_TREE, TREE_TYPE(nextVar), NULL_TREE) );
        localVarMap[ name ] = nextVar;
    }
    if( gCurrentToken->mId == KW_SEMICOLON )
    {
        drop(KW_SEMICOLON);
        parameters(param_decl, params);
    }
}
/// @return true if now, false if forward
bool SyntacticAnalyzer::funcDec2(tree & var, tree & stmt )
{
    print( "funcDec2" );
    if( gCurrentToken->mId == KW_FORWARD )
    {
        drop(KW_FORWARD);
        return false;
    }
    var = optional();
    tree decl, bind, blk;
    stmt = body(decl, bind, blk);
    return true;
}
tree SyntacticAnalyzer::optional()
{
    print( "optional" );
    if( gCurrentToken->mId == KW_CONST )
    {
        drop(KW_CONST);
        constbl(localVarMap);
    }
    return optional2();
}
tree SyntacticAnalyzer::optional2()
{
    print( "optional2" );
    tree decls = NULL_TREE;
    if( gCurrentToken->mId == KW_VAR )
    {
        drop(KW_VAR);
        decls = varbl(decls, localVarMap, false );
    }
    return decls;
}

tree SyntacticAnalyzer::main(tree globals )
{
    print( "main" );
    // zacatek
    tree bind = NULL_TREE, blk = NULL_TREE;
    tree stmt = body( globals, bind, blk );
    // konec
    drop(KW_DOT);
    return build_main2( stmt );
}

tree SyntacticAnalyzer::body( tree decls, tree & bind, tree & blk )
{
    print( "body" );
    //    blk = build_block(decls, NULL_TREE, NULL_TREE, NULL_TREE);
    //    TREE_USED(blk) = true;
    tree stmt = alloc_stmt_list();
    
    drop(KW_BEGIN);
    recursion( stmt );
    drop(KW_END);

    //    bind = build3( BIND_EXPR, void_type_node, BLOCK_VARS(blk), NULL_TREE, blk );
    //    BIND_EXPR_BODY(bind) = stmt;
    //    TREE_SIDE_EFFECTS(bind) = true;
    return stmt;
}
tree SyntacticAnalyzer::block()
{
    print( "block" );
    tree stmt = alloc_stmt_list();
    if( gCurrentToken->mId == KW_BEGIN )
    {
        drop(KW_BEGIN);
        recursion( stmt );
        drop(KW_END);
    }
    else
    {
        tree c = command(stmt);
        append_to_statement_list( c, &stmt );
    }
    return stmt;
}
void SyntacticAnalyzer::recursion( tree & stmt )
{
    print( "recursion" );
    TokenID id = gCurrentToken->mId;
    while( id == KW_IF 
        || id == KW_FOR 
        || id == KW_WHILE 
        || id == IDENTFIER 
        || id == KW_EXIT )
    {
        tree c = command( stmt );
        if( gCurrentToken->mId == KW_SEMICOLON )
            drop(KW_SEMICOLON);
        append_to_statement_list( c, &stmt );
        id = gCurrentToken->mId;
    }
}
// FIXME exit
tree SyntacticAnalyzer::command( tree & statementParam )
{
    print( "command" );
    switch( gCurrentToken->mId )
    {
        case KW_IF:   return ifconstruction();
        case KW_FOR:   return forconstruction( statementParam );
        case KW_WHILE:  return whileconstruction();
        case IDENTFIER: {
            string name;
            dropStr(name);
            if( gCurrentToken->mId == KW_LBRACKET )
                return call( name );
            else // ':=' or '['
                return assigment( name );
            break;
        }
        case KW_EXIT:  {
            drop(KW_EXIT);
            return build_return();
            //build_exit();
            break;
        }
        default: break;
    }
}
tree SyntacticAnalyzer::build_return()
{
    if( isFunc )
    {
        tree var = findVariable( "#_return_" );
        //tree resdecl = build_decl (BUILTINS_LOCATION, RESULT_DECL, NULL_TREE, integer_type_node);
        //build2(MODIFY_EXPR, TREE_TYPE(integer_type_node), resdecl, var));
        return build1(RETURN_EXPR, void_type_node, var );
    }
    return build1(RETURN_EXPR, void_type_node, NULL_TREE );
}
// FIXED conditions
tree SyntacticAnalyzer::ifconstruction()
{
    print( "ifconstruction" );
    tree cond, thenbl, elsebl = NULL_TREE;
    drop(KW_IF);
    cond = logical_expression();
    drop(KW_THEN);
    thenbl = block();
    if( gCurrentToken->mId == KW_ELSE )
        elsebl = elseblock();

    return build3( COND_EXPR, void_type_node, cond, thenbl, elsebl );
}
tree SyntacticAnalyzer::elseblock()
{
    print( "elseblock" );
    if( gCurrentToken->mId != KW_ELSE ) return NULL_TREE;
    drop(KW_ELSE);
    return block();
}
// FIXED loops
tree SyntacticAnalyzer::forconstruction( tree & statementParam )
{
    print( "forconstruction" );
    drop(KW_FOR);
    string name;
    dropStr( name );
    tree iter = findVariable( name );
    drop(OP_ASSIGN);
    tree from = aritmetic_expression();
    tree toAppend = build2( MODIFY_EXPR, integer_type_node, iter, from);
    bool to;
    where(to);
    tree endVal = aritmetic_expression();
    drop(KW_DO);
    tree stmt = block();
    tree for_loop = build_for( iter, endVal, to, stmt );
    append_to_statement_list( toAppend, & statementParam );
    //append_to_statement_list( for_loop, & statementParam );
    //drop(KW_SEMICOLON);
    return for_loop;
}
void SyntacticAnalyzer::where( bool & to )
{
    print( "where" );
    if( gCurrentToken->mId == KW_TO )
    {
        to = true;
        drop(KW_TO);
    }
    else
    { 
        to = false;
        drop(KW_DOWNTO);
    }
}
tree SyntacticAnalyzer::whileconstruction()
{
    print( "whileconstruction" );
    drop(KW_WHILE);
    tree cond = logical_expression();
    drop(KW_DO);
    tree stmt = block();
    return build_while( cond, stmt );
}
// CHECK integet_type_node if array
tree SyntacticAnalyzer::assigment( const string & name )
{
    print( "assigment" );
    // basic var or array
    tree left = findVariable( name );
    drop(OP_ASSIGN);
    tree right = aritmetic_expression();
    return build2( MODIFY_EXPR, 
        integer_type_node,
        left,
        right
    );
}
tree SyntacticAnalyzer::call( const string & name )
{
    print( "call" );
    drop(KW_LBRACKET);
    tree res;
    if(  name == "writeln" )
    {
        
        if( gCurrentToken-> mId == STRING )
        {
            string str;
            dropStr(str);
            res = build_print_string( UNKNOWN_LOCATION, build_string_wrapper( str, true ) );
        }
        else res = build_print_int( UNKNOWN_LOCATION, logical_expression(), true );
    }
    else if( name == "write" )
    {
        if( gCurrentToken-> mId == STRING )
        {
            string str;
            dropStr(str);
            res = build_print_string( UNKNOWN_LOCATION, build_string_wrapper( str, false ) );
        }
        else res = build_print_int( UNKNOWN_LOCATION, logical_expression(), false );
    }
    else if( myMatch( name, "READLN" ) )
    {
        string name;
        dropStr(name);
        res = build_scan_int( UNKNOWN_LOCATION, findVariable( name ) );
    }
    else if( myMatch( name, "DEC" ) )
    {
        string name;
        dropStr(name);
        res = build_dec( findVariable( name ) );
    }
    else if( myMatch( name, "INC" ) )
    {
        string name;
        dropStr(name);
        res = build_inc( findVariable( name ) );
    }
    else
    {
        vector< tree > vec;
        val_list( vec );
        tree * args_vec = XNEWVEC( tree, vec.size() );
        for( size_t i = 0; i < vec.size(); ++i )
        {
            args_vec[i] = vec[i];
        }
        tree fndecl = findFunction( name );
        tree call = build_call_expr_loc_array( UNKNOWN_LOCATION, fndecl, vec.size(), args_vec );
        SET_EXPR_LOCATION(call, UNKNOWN_LOCATION);
        TREE_USED(call) = true;

        res = call;
    }
    drop(KW_RBRACKET);
    return res;
}
void SyntacticAnalyzer::val_list( vector<tree> & vec )
{
    print( "val_list" );
    bool first = true;
    while( gCurrentToken->mId != KW_RBRACKET )
    {
        if( !first )
        {
            drop(KW_COMMA);
        }
        first = false;
        tree next;
        switch( gCurrentToken->mId )
        {
            case STRING:
            {
                string name;
                dropStr(name);
                next = build_decl (UNKNOWN_LOCATION, 
                    VAR_DECL, 
                    get_identifier(name.c_str()), 
                    integer_type_node
                );
                break;
            }
            default:
                next = aritmetic_expression();
                break;
        }
        vec.push_back(next);
    }
}
// FIXED LE
tree SyntacticAnalyzer::logical_expression()
{
    print( "logical_expression" );
    tree left = LT();
    return LE2(left);
}
tree SyntacticAnalyzer::LE2( tree left )
{
    print( "LE2" );
    while( gCurrentToken->mId == LO_OR )
    {
        drop( LO_OR );
        left = build2( TRUTH_ORIF_EXPR, 
            integer_type_node,
            left,
            LT()
        );
    }
    return left;
}
tree SyntacticAnalyzer::LT()
{
    print( "LT" );
    tree left = LF();
    return LT2( left );
}
tree SyntacticAnalyzer::LT2(tree left)
{
    print( "LT2" );
    while( gCurrentToken->mId == LO_AND )
    {
        drop(LO_AND);
        left = build2( TRUTH_ANDIF_EXPR, integer_type_node, left, LF() );
    }
    return left;
}
tree SyntacticAnalyzer::LF()
{
    print( "LF" );
    tree left = aritmetic_expression();
    return LF2( left );
}
tree SyntacticAnalyzer::LF2( tree left )
{
    print( "LF2" );
    switch( gCurrentToken->mId )
    {
        case LO_L :{
            drop( gCurrentToken->mId );
            return build2( LT_EXPR, integer_type_node, left, aritmetic_expression() );
        }
        case LO_LE:{
            drop( gCurrentToken->mId );
            return build2( LE_EXPR, integer_type_node, left, aritmetic_expression() );
        }
        case LO_EQ:{
            drop( gCurrentToken->mId );
            return build2( EQ_EXPR, integer_type_node, left, aritmetic_expression() );
        }
        case LO_GE:{
            drop( gCurrentToken->mId );
            return build2( GE_EXPR, integer_type_node, left, aritmetic_expression() );
        }
        case LO_G :{
            drop( gCurrentToken->mId );
            return build2( GT_EXPR, integer_type_node, left, aritmetic_expression() );
        }
        case LO_NE :{
            drop( gCurrentToken->mId );
            return build2( NE_EXPR, integer_type_node, left, aritmetic_expression() );
        }
        default:
            return left;
    }
}
tree SyntacticAnalyzer::aritmetic_expression()
{
    print( "aritmetic_expression" );
    tree left;
    if( gCurrentToken->mId == OP_MINUS )
    {
        drop(OP_MINUS);
        left = build1( NEGATE_EXPR, integer_type_node, AT() );
    }
    else
        left = AT();
    return AE2(left);
}
tree SyntacticAnalyzer::AE2(tree left)
{
    print( "AE2" );
    tree right;
    TokenID id = gCurrentToken->mId;
    while( id == OP_PLUS
        || id == OP_MINUS )
    {
        drop(id);
        right = AT();
        left = build2( id == OP_PLUS? PLUS_EXPR 
                                    : MINUS_EXPR,
                    integer_type_node,
                    left,
                    right
        );
        id = gCurrentToken->mId;
    }
    return left;
}
tree SyntacticAnalyzer::AT()
{
    print( "AT" );
    tree res = AF();
    return AT2( res );
}
tree SyntacticAnalyzer::AT2( tree left )
{
    print( "AT2" );
    TokenID id = gCurrentToken->mId;
    while( id == OP_MUL
        || id == OP_DIV
        || id == OP_MOD )
    {
        drop(id);
        tree right = AF();
        left = build2( id == OP_MUL? MULT_EXPR 
                                    : id == OP_DIV ? TRUNC_DIV_EXPR 
                                                    : TRUNC_MOD_EXPR, 
                    integer_type_node, 
                    left, 
                    right 
        );
        id = gCurrentToken->mId;
    }
    return left;
}
tree SyntacticAnalyzer::AF()
{
    print( "AF" );
    if( gCurrentToken->mId == KW_LBRACKET )
    {
        drop(KW_LBRACKET);
        tree le = logical_expression();
        drop(KW_RBRACKET);
        return le;
    }
    if( gCurrentToken->mId == INTEGER )
    {   
        int val;
        dropInt(val);
        return build_int_cst(integer_type_node, val);
    }
    if( gCurrentToken->mId == IDENTFIER )
    {
        string name;
        dropStr(name);
        if( gCurrentToken->mId == KW_LBRACKET )
        {
            return call( name );
        }
        else
        { 
            return findVariable( name );
        }
    }
}
tree SyntacticAnalyzer::findVariable( const string & name )
{
    if( gCurrentToken->mId == KW_LSQBRACKET )
    {
        drop(KW_LSQBRACKET);
        tree idx = aritmetic_expression();
        drop(KW_RSQBRACKET);
        tree array;
        // FIXME array on idx
        if( localVarMap.find( name ) != localVarMap.end() )
            array = localVarMap[ name ];
        else if( varMap.find( name ) != varMap.end() )
            array = varMap[ name ];
        else
        {
            handleError( "Array '%s' not faund in this scope.\n", name.c_str() );
            return NULL_TREE;
        }
        if( idxMap.find(name) == idxMap.end() )
        {
            handleError( "Array '%s' has not value in idxMap.\n", name.c_str() );
            return NULL_TREE;   
        }
        tree idxFromZero = build2( PLUS_EXPR, 
            integer_type_node, 
            build_int_cst(integer_type_node, idxMap[name]),
            idx 
        );
        return build4(ARRAY_REF, TREE_TYPE(TREE_TYPE(array)), array, idxFromZero, NULL_TREE, NULL_TREE);
    }
    else
    {
        return findSimpleVariable( name );
    }
}
tree SyntacticAnalyzer::findSimpleVariable( const string & name )
{
    if( localVarMap.find( name ) != localVarMap.end() )
        return localVarMap[ name ];
    if( varMap.find( name ) != varMap.end() )
        return varMap[ name ];
    handleError( "Variable '%s' not faund in this scope.\n", name.c_str() );
    return NULL_TREE;
}
tree SyntacticAnalyzer::findFunction( const string & name, bool onlyCheck = false )
{
    if( funcMap.find( name ) != funcMap.end() )
        return funcMap[ name ];
    if(! onlyCheck ) 
        handleError( "Function '%s' not faund in this scope.\n", name.c_str() );
    return NULL_TREE;
}

TFunc * SyntacticAnalyzer::findFunctionFw( const string & name )
{
    if( funcFwMap.find( name ) != funcFwMap.end() )
        return funcFwMap[ name ];
    return NULL;
}

void SyntacticAnalyzer::registerGlobalVarible( tree gv )
{
    vec_safe_push( fileVec, gv );
}

void SyntacticAnalyzer::registerFunction( tree fc )
{
    vec_safe_push( fileVec, fc );
    tree_dump_original( fc );
    gimplify_function_tree( fc );
    cgraph_node::finalize_function( fc, false );
}

void SyntacticAnalyzer::tree_dump_original (tree fndecl)
{
    FILE *dump_orig;
    int local_dump_flags;
    struct cgraph_node *cgn;

    // Dump the C-specific tree IR.  
    dump_orig = dump_begin (TDI_original, &local_dump_flags);
    if (dump_orig) {
        fprintf (dump_orig, "\n//;; Function %s", lang_hooks.decl_printable_name (fndecl, 2));
        fprintf (dump_orig, " (%s)\n", (!DECL_ASSEMBLER_NAME_SET_P (fndecl) ? "null" : IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fndecl))));
        fprintf (dump_orig, "//;; enabled by -%s\n", dump_flag_name (TDI_original));
        fprintf (dump_orig, "\n");

        if (local_dump_flags & TDF_RAW)
            dump_node (fndecl, TDF_SLIM | local_dump_flags, dump_orig);
        else {
            struct function fn;
            fn.decl = fndecl;
            fn.curr_properties = 0;
            fn.cfg = NULL;
            DECL_STRUCT_FUNCTION(fndecl) = &fn;
            dump_function_to_file(fndecl, dump_orig, 0);
            DECL_STRUCT_FUNCTION(fndecl) = NULL;
        }
        fprintf (dump_orig, "\n");
        dump_end (TDI_original, dump_orig);
    }

    // Dump all nested functions now.  
    cgn = cgraph_node::get_create (fndecl);
    for (cgn = cgn->nested; cgn ; cgn = cgn->next_nested) tree_dump_original (cgn->decl);
}



#endif // ___SYNAN___
