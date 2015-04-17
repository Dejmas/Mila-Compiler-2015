#ifndef TOKEN_TYPES_H
#define TOKEN_TYPES_H
#include <stdio.h>
#include <string>


/**
 * @enum TokenID
 * @brief Enumeration of token identifiers for the Mila programming language.
 */
enum TokenID
{
    KW_IF,          /**< Keyword 'if' */
    KW_THEN,        /**< Keyword 'then' */
    KW_BEGIN,       /**< Keyword 'begin' */
    KW_END,         /**< Keyword 'end' */
    KW_PROCEDURE,   /**< Keyword 'procedure' */
    KW_FUNCTION,    /**< Keyword 'function' */
    KW_FOR,         /**< Keyword 'for' */
    KW_WHILE,       /**< Keyword 'while' */
    KW_DO,          /**< Keyword 'do' */
    KW_TO,          /**< Keyword 'to' */
    KW_DOWNTO,      /**< Keyword 'downto' */
    KW_CONST,       /**< Keyword 'const' */
    KW_READLN,      /**< Keyword 'readln' */
    KW_WRITELN,     /**< Keyword 'writeln' */
    KW_EXIT,        /**< Keyword 'exit' */
    KW_COLON,       /**< Symbol ':' */
    KW_COMMA,       /**< Symbol ',' */
    KW_DOT,         /**< Symbol '.' */
    KW_SEMICOLON,   /**< Symbol ';' */
    KW_DOUBLEDOT,   /**< Symbol '..' */
    KW_LSQBRACKET,  /**< Symbol '[' */
    KW_RSQBRACKET,  /**< Symbol ']' */
    KW_VAR,         /**< Keyword 'var' */
    KW_OF,          /**< Keyword 'of' */
    KW_LBRACKET,    /**< Symbol '(' */
    KW_RBRACKET,    /**< Symbol ')' */
    KW_PROGRAM,     /**< Keyword 'program' */
    KW_INTEGER,     /**< Keyword 'integer' */
    KW_ARRAY,       /**< Keyword 'array' */
    KW_FORWARD,     /**< Keyword 'forward' */
    KW_BREAK,       /**< Keyword 'break' */
    KW_CONTINUE,    /**< Keyword 'continue' */
    KW_ELSE,        /**< Keyword 'else' */

    INTEGER,        /**< Integer literal */
    OP_PLUS,        /**< Operator '+' */
    OP_MINUS,       /**< Operator '-' */
    OP_MUL,         /**< Operator '*' */
    OP_DIV,         /**< Operator '/' */
    OP_MOD,         /**< Operator '%' */
    OP_ASSIGN,      /**< Operator ':=' */
    LO_OR,          /**< Logical operator 'or' */
    LO_AND,         /**< Logical operator 'and' */
    LO_NOT,         /**< Logical operator 'not' */
    LO_EQ,          /**< Logical operator '=' */
    LO_NE,          /**< Logical operator '<>' */
    LO_LE,          /**< Logical operator '<=' */
    LO_GE,          /**< Logical operator '>=' */
    LO_L,           /**< Logical operator '<' */
    LO_G,           /**< Logical operator '>' */
    STRING,         /**< String literal */
    IDENTFIER       /**< Identifier */
};

/**
 * @var TokenNames
 * @brief Array of token identifier names as strings.
 */
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

/**
 * @class Token
 * @brief Represents a token in the Mila programming language.
 */
class Token {
public:
    /**
     * @brief Constructs a Token object.
     * 
     * @param tid The token identifier.
     * @param data The token data (default is an empty string).
     * @param value The token value (default is 0).
     */
    Token(TokenID tid, const std::string & data = "", int value = 0);

    /**
     * @brief Destroys the Token object.
     */
    ~Token();

    /**
     * @brief Prints the token information as a string.
     * 
     * @return std::string The token information.
     */
    std::string print() const;

public:
    TokenID mId;         /**< The token identifier. */
    std::string mData;   /**< The token data. */
    int mValue;          /**< The token value. */
};

/**
 * @var gCurrentToken
 * @brief Global pointer to the current token being processed.
 */
extern Token * gCurrentToken;

#endif // TOKEN_TYPES_H
