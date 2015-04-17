#include "tokentypes.h"

#include <stdio.h>
#include <string>

using std::string;

Token * gCurrentToken = NULL;

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
	if( mId == INTEGER ) {
		sprintf( str, "%s : %d", TokenNames[ mId ], mValue );
		//os << TokenNames[ mId ] << ": " << mValue;
	} else if( mId == IDENTFIER || mId == STRING ) {
		sprintf( str, "%s : %s", TokenNames[ mId ], mData.c_str() );
		//os << TokenNames[ mId ] << ": " << mData;
	} else {
		sprintf( str, "%s", TokenNames[ mId ]);
		//os << TokenNames[ tok.mId ];
	}
	return str;
}
