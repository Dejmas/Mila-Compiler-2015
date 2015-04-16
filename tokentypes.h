#ifndef TOKEN_TYPES_H
#define TOKEN_TYPES_H
#include <stdio.h>
#include <string>



enum TokenID
{
    KW_IF,          
    KW_THEN,        
    KW_BEGIN,       
    KW_END,         
    KW_PROCEDURE,   
    KW_FUNCTION,    
    KW_FOR,         
    KW_WHILE,       
    KW_DO,          
    KW_TO,          
    KW_DOWNTO,      
    KW_CONST,       
    KW_READLN,      
    KW_WRITELN,     
    KW_EXIT,        
    KW_COLON,       
    KW_COMMA,       
    KW_DOT,         
    KW_SEMICOLON,   
    KW_DOUBLEDOT,   
    KW_LSQBRACKET,  
    KW_RSQBRACKET,  
    KW_VAR,         
    KW_OF,          
    KW_LBRACKET,    
    KW_RBRACKET,    
    KW_PROGRAM,     
    KW_INTEGER,     
    KW_ARRAY,       
    KW_FORWARD,     
    KW_BREAK,       
    KW_CONTINUE,    
    KW_ELSE,        
    INTEGER,        
    OP_PLUS,        
    OP_MINUS,       
    OP_MUL,         
    OP_DIV,         
    OP_MOD,         
    OP_ASSIGN,      
    LO_OR,          
    LO_AND,         
    LO_NOT,         
    LO_EQ,          
    LO_NE,          
    LO_LE,          
    LO_GE,          
    LO_L,           
    LO_G,           
    STRING,         
    IDENTFIER       
};


const char * TokenNames[] = {
    "KW_IF",
    "KW_THEN",
    "KW_BEGIN",
    "KW_END",
    "KW_PROCEDURE",
    "KW_FUNCTION",
    "KW_FOR",
    "KW_WHILE",
    "KW_DO",
    "KW_TO",
    "KW_DOWNTO",
    "KW_CONST",
    "KW_READLN",
    "KW_WRITELN",
    "KW_EXIT",
    "KW_COLON",
    "KW_COMMA",
    "KW_DOT",
    "KW_SEMICOLON",
    "KW_DOUBLEDOT",
    "KW_LSQBRACKET",
    "KW_RSQBRACKET",
    "KW_VAR",
    "KW_OF",
    "KW_LBRACKET",
    "KW_RBRACKET",
    "KW_PROGRAM",
    "KW_INTEGER",
    "KW_ARRAY",
    "KW_FORWARD",
    "KW_BREAK",
    "KW_CONTINUE",
    "KW_ELSE",

    "INTEGER",
    "OP_PLUS",
    "OP_MINUS",
    "OP_MUL",
    "OP_DIV",
    "OP_MOD",
    "OP_ASSIGN",
    "LO_OR",
    "LO_AND",
    "LO_NOT",
    "LO_EQ",
    "LO_NE",
    "LO_LE",
    "LO_GE",
    "LO_L",
    "LO_G",
    "STRING",
    "IDENTFIER",
};


class Token {
public:

    Token(TokenID tid, const std::string & data = "", int value = 0);


    ~Token();


    std::string print() const;

public:
    TokenID mId;         /**< The token identifier. */
    std::string mData;   /**< The token data. */
    int mValue;          /**< The token value. */
};


extern Token * gCurrentToken;

#endif // TOKEN_TYPES_H
