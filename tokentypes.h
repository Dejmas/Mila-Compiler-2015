#ifndef ___TOKEN_TYPES___
#define ___TOKEN_TYPES___
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
	IDENTFIER,
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

using std::string;
class Token{
public:
	Token( TokenID tid, const string & data = "", int value = 0 );
	~Token();
	
	std::string print() const;

	TokenID 		mId;
	std::string 	mData;
	int 		  	mValue;
} * gCurrentToken = NULL;

Token::Token( TokenID tid, const string & data, int value )
:mId( tid )
,mData( data ) 
,mValue( value ) 
{

}

Token::~Token()
{
}


string Token::print() const
{
	char str[256];
	if( mId == INTEGER )
		sprintf( str, "%s : %d", TokenNames[ mId ], mValue );
		//os << TokenNames[ mId ] << ": " << mValue;
	else if( mId == IDENTFIER || mId == STRING )
		sprintf( str, "%s : %s", TokenNames[ mId ], mData.c_str() );
		//os << TokenNames[ mId ] << ": " << mData;
	else
		sprintf( str, "%s", TokenNames[ mId ]);
		//os << TokenNames[ tok.mId ];
	return str;
}


#endif //___TOKEN_TYPES___