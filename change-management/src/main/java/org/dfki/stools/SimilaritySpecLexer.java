// $ANTLR 3.3 Nov 30, 2010 12:50:56 SimilaritySpec.g 2012-03-21 12:37:27

package org.dfki.stools;


import org.antlr.runtime.*;

public class SimilaritySpecLexer extends Lexer {
    public static final int EOF=-1;
    public static final int T__41=41;
    public static final int T__42=42;
    public static final int T__43=43;
    public static final int T__44=44;
    public static final int T__45=45;
    public static final int T__46=46;
    public static final int T__47=47;
    public static final int T__48=48;
    public static final int T__49=49;
    public static final int T__50=50;
    public static final int T__51=51;
    public static final int ALTERNATIVES=4;
    public static final int ALT=5;
    public static final int ENTRY=6;
    public static final int ALLBUT=7;
    public static final int OPT=8;
    public static final int OPTIONAL=9;
    public static final int MANDAT=10;
    public static final int MANDATORY=11;
    public static final int EQUIVSPECS=12;
    public static final int EQUIVSPEC=13;
    public static final int EXTENDS=14;
    public static final int XMLNS=15;
    public static final int XMLNSPREFIX=16;
    public static final int ELEMENT=17;
    public static final int LIMIT=18;
    public static final int ANNOTATIONS=19;
    public static final int ABSENT=20;
    public static final int CONSTITUENTS=21;
    public static final int UNORDERED=22;
    public static final int ORDERED=23;
    public static final int TEXTNODE=24;
    public static final int SPLITNODESTART=25;
    public static final int SPLITNODEEND=26;
    public static final int SPLITNODE=27;
    public static final int COMMENTNODE=28;
    public static final int IGNORE=29;
    public static final int PREFIXEDNAME=30;
    public static final int UNPREFIXEDNAME=31;
    public static final int TUPLE=32;
    public static final int EQUIVELEM=33;
    public static final int DEFAULTSPEC=34;
    public static final int NCNAME=35;
    public static final int ATTVALUE=36;
    public static final int STRINGCONTENT=37;
    public static final int NameStartChar=38;
    public static final int NameChar=39;
    public static final int WS=40;

    // delegates
    // delegators

    public SimilaritySpecLexer() {;} 
    public SimilaritySpecLexer(CharStream input) {
        this(input, new RecognizerSharedState());
    }
    public SimilaritySpecLexer(CharStream input, RecognizerSharedState state) {
        super(input,state);

    }
    public String getGrammarFileName() { return "SimilaritySpec.g"; }

    // $ANTLR start "ALTERNATIVES"
    public final void mALTERNATIVES() throws RecognitionException {
        try {
            int _type = ALTERNATIVES;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:7:14: ( 'alternatives' )
            // SimilaritySpec.g:7:16: 'alternatives'
            {
            match("alternatives"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ALTERNATIVES"

    // $ANTLR start "ENTRY"
    public final void mENTRY() throws RecognitionException {
        try {
            int _type = ENTRY;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:8:7: ( 'ENTRY' )
            // SimilaritySpec.g:8:9: 'ENTRY'
            {
            match("ENTRY"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ENTRY"

    // $ANTLR start "OPT"
    public final void mOPT() throws RecognitionException {
        try {
            int _type = OPT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:9:5: ( '?' )
            // SimilaritySpec.g:9:7: '?'
            {
            match('?'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "OPT"

    // $ANTLR start "OPTIONAL"
    public final void mOPTIONAL() throws RecognitionException {
        try {
            int _type = OPTIONAL;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:10:10: ( 'OPTIONAL' )
            // SimilaritySpec.g:10:12: 'OPTIONAL'
            {
            match("OPTIONAL"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "OPTIONAL"

    // $ANTLR start "MANDAT"
    public final void mMANDAT() throws RecognitionException {
        try {
            int _type = MANDAT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:11:8: ( '!' )
            // SimilaritySpec.g:11:10: '!'
            {
            match('!'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "MANDAT"

    // $ANTLR start "MANDATORY"
    public final void mMANDATORY() throws RecognitionException {
        try {
            int _type = MANDATORY;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:12:11: ( 'mandatory' )
            // SimilaritySpec.g:12:13: 'mandatory'
            {
            match("mandatory"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "MANDATORY"

    // $ANTLR start "EQUIVSPEC"
    public final void mEQUIVSPEC() throws RecognitionException {
        try {
            int _type = EQUIVSPEC;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:13:11: ( 'equivspec' )
            // SimilaritySpec.g:13:13: 'equivspec'
            {
            match("equivspec"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "EQUIVSPEC"

    // $ANTLR start "EXTENDS"
    public final void mEXTENDS() throws RecognitionException {
        try {
            int _type = EXTENDS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:14:9: ( 'extends' )
            // SimilaritySpec.g:14:11: 'extends'
            {
            match("extends"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "EXTENDS"

    // $ANTLR start "XMLNS"
    public final void mXMLNS() throws RecognitionException {
        try {
            int _type = XMLNS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:15:7: ( 'xmlns' )
            // SimilaritySpec.g:15:9: 'xmlns'
            {
            match("xmlns"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "XMLNS"

    // $ANTLR start "XMLNSPREFIX"
    public final void mXMLNSPREFIX() throws RecognitionException {
        try {
            int _type = XMLNSPREFIX;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:16:13: ( 'xmlns:' )
            // SimilaritySpec.g:16:15: 'xmlns:'
            {
            match("xmlns:"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "XMLNSPREFIX"

    // $ANTLR start "ELEMENT"
    public final void mELEMENT() throws RecognitionException {
        try {
            int _type = ELEMENT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:17:9: ( 'element' )
            // SimilaritySpec.g:17:11: 'element'
            {
            match("element"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ELEMENT"

    // $ANTLR start "LIMIT"
    public final void mLIMIT() throws RecognitionException {
        try {
            int _type = LIMIT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:18:7: ( 'limit' )
            // SimilaritySpec.g:18:9: 'limit'
            {
            match("limit"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "LIMIT"

    // $ANTLR start "ANNOTATIONS"
    public final void mANNOTATIONS() throws RecognitionException {
        try {
            int _type = ANNOTATIONS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:19:13: ( 'annotations' )
            // SimilaritySpec.g:19:15: 'annotations'
            {
            match("annotations"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ANNOTATIONS"

    // $ANTLR start "ABSENT"
    public final void mABSENT() throws RecognitionException {
        try {
            int _type = ABSENT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:20:8: ( 'absent' )
            // SimilaritySpec.g:20:10: 'absent'
            {
            match("absent"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ABSENT"

    // $ANTLR start "CONSTITUENTS"
    public final void mCONSTITUENTS() throws RecognitionException {
        try {
            int _type = CONSTITUENTS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:21:14: ( 'constituents' )
            // SimilaritySpec.g:21:16: 'constituents'
            {
            match("constituents"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "CONSTITUENTS"

    // $ANTLR start "UNORDERED"
    public final void mUNORDERED() throws RecognitionException {
        try {
            int _type = UNORDERED;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:22:11: ( 'unordered' )
            // SimilaritySpec.g:22:13: 'unordered'
            {
            match("unordered"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "UNORDERED"

    // $ANTLR start "ORDERED"
    public final void mORDERED() throws RecognitionException {
        try {
            int _type = ORDERED;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:23:9: ( 'ordered' )
            // SimilaritySpec.g:23:11: 'ordered'
            {
            match("ordered"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ORDERED"

    // $ANTLR start "TEXTNODE"
    public final void mTEXTNODE() throws RecognitionException {
        try {
            int _type = TEXTNODE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:24:10: ( '<TEXT>' )
            // SimilaritySpec.g:24:12: '<TEXT>'
            {
            match("<TEXT>"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "TEXTNODE"

    // $ANTLR start "SPLITNODESTART"
    public final void mSPLITNODESTART() throws RecognitionException {
        try {
            int _type = SPLITNODESTART;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:25:16: ( '<SPLIT(' )
            // SimilaritySpec.g:25:18: '<SPLIT('
            {
            match("<SPLIT("); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "SPLITNODESTART"

    // $ANTLR start "SPLITNODEEND"
    public final void mSPLITNODEEND() throws RecognitionException {
        try {
            int _type = SPLITNODEEND;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:26:14: ( ')>' )
            // SimilaritySpec.g:26:16: ')>'
            {
            match(")>"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "SPLITNODEEND"

    // $ANTLR start "SPLITNODE"
    public final void mSPLITNODE() throws RecognitionException {
        try {
            int _type = SPLITNODE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:27:11: ( 'SPLIT' )
            // SimilaritySpec.g:27:13: 'SPLIT'
            {
            match("SPLIT"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "SPLITNODE"

    // $ANTLR start "COMMENTNODE"
    public final void mCOMMENTNODE() throws RecognitionException {
        try {
            int _type = COMMENTNODE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:28:13: ( '<COMMENT>' )
            // SimilaritySpec.g:28:15: '<COMMENT>'
            {
            match("<COMMENT>"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "COMMENTNODE"

    // $ANTLR start "IGNORE"
    public final void mIGNORE() throws RecognitionException {
        try {
            int _type = IGNORE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:29:8: ( 'ignore' )
            // SimilaritySpec.g:29:10: 'ignore'
            {
            match("ignore"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "IGNORE"

    // $ANTLR start "DEFAULTSPEC"
    public final void mDEFAULTSPEC() throws RecognitionException {
        try {
            int _type = DEFAULTSPEC;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:30:13: ( ':default' )
            // SimilaritySpec.g:30:15: ':default'
            {
            match(":default"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "DEFAULTSPEC"

    // $ANTLR start "T__41"
    public final void mT__41() throws RecognitionException {
        try {
            int _type = T__41;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:31:7: ( '{' )
            // SimilaritySpec.g:31:9: '{'
            {
            match('{'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__41"

    // $ANTLR start "T__42"
    public final void mT__42() throws RecognitionException {
        try {
            int _type = T__42;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:32:7: ( '}' )
            // SimilaritySpec.g:32:9: '}'
            {
            match('}'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__42"

    // $ANTLR start "T__43"
    public final void mT__43() throws RecognitionException {
        try {
            int _type = T__43;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:33:7: ( ',' )
            // SimilaritySpec.g:33:9: ','
            {
            match(','); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__43"

    // $ANTLR start "T__44"
    public final void mT__44() throws RecognitionException {
        try {
            int _type = T__44;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:34:7: ( '=' )
            // SimilaritySpec.g:34:9: '='
            {
            match('='); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__44"

    // $ANTLR start "T__45"
    public final void mT__45() throws RecognitionException {
        try {
            int _type = T__45;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:35:7: ( '_' )
            // SimilaritySpec.g:35:9: '_'
            {
            match('_'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__45"

    // $ANTLR start "T__46"
    public final void mT__46() throws RecognitionException {
        try {
            int _type = T__46;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:36:7: ( '\\\\' )
            // SimilaritySpec.g:36:9: '\\\\'
            {
            match('\\'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__46"

    // $ANTLR start "T__47"
    public final void mT__47() throws RecognitionException {
        try {
            int _type = T__47;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:37:7: ( '(' )
            // SimilaritySpec.g:37:9: '('
            {
            match('('); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__47"

    // $ANTLR start "T__48"
    public final void mT__48() throws RecognitionException {
        try {
            int _type = T__48;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:38:7: ( ')' )
            // SimilaritySpec.g:38:9: ')'
            {
            match(')'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__48"

    // $ANTLR start "T__49"
    public final void mT__49() throws RecognitionException {
        try {
            int _type = T__49;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:39:7: ( '[' )
            // SimilaritySpec.g:39:9: '['
            {
            match('['); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__49"

    // $ANTLR start "T__50"
    public final void mT__50() throws RecognitionException {
        try {
            int _type = T__50;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:40:7: ( ']' )
            // SimilaritySpec.g:40:9: ']'
            {
            match(']'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__50"

    // $ANTLR start "T__51"
    public final void mT__51() throws RecognitionException {
        try {
            int _type = T__51;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:41:7: ( ':' )
            // SimilaritySpec.g:41:9: ':'
            {
            match(':'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__51"

    // $ANTLR start "ATTVALUE"
    public final void mATTVALUE() throws RecognitionException {
        try {
            int _type = ATTVALUE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:172:13: ( '\"' ( STRINGCONTENT | '\\'' )* '\"' | '\\'' ( STRINGCONTENT | '\"' )* '\\'' )
            int alt3=2;
            int LA3_0 = input.LA(1);

            if ( (LA3_0=='\"') ) {
                alt3=1;
            }
            else if ( (LA3_0=='\'') ) {
                alt3=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 3, 0, input);

                throw nvae;
            }
            switch (alt3) {
                case 1 :
                    // SimilaritySpec.g:172:16: '\"' ( STRINGCONTENT | '\\'' )* '\"'
                    {
                    match('\"'); 
                    // SimilaritySpec.g:172:20: ( STRINGCONTENT | '\\'' )*
                    loop1:
                    do {
                        int alt1=2;
                        int LA1_0 = input.LA(1);

                        if ( (LA1_0=='\''||LA1_0=='+'||(LA1_0>='-' && LA1_0<=':')||LA1_0=='='||LA1_0=='?'||(LA1_0>='A' && LA1_0<='Z')||LA1_0=='\\'||(LA1_0>='a' && LA1_0<='z')) ) {
                            alt1=1;
                        }


                        switch (alt1) {
                    	case 1 :
                    	    // SimilaritySpec.g:
                    	    {
                    	    if ( input.LA(1)=='\''||input.LA(1)=='+'||(input.LA(1)>='-' && input.LA(1)<=':')||input.LA(1)=='='||input.LA(1)=='?'||(input.LA(1)>='A' && input.LA(1)<='Z')||input.LA(1)=='\\'||(input.LA(1)>='a' && input.LA(1)<='z') ) {
                    	        input.consume();

                    	    }
                    	    else {
                    	        MismatchedSetException mse = new MismatchedSetException(null,input);
                    	        recover(mse);
                    	        throw mse;}


                    	    }
                    	    break;

                    	default :
                    	    break loop1;
                        }
                    } while (true);

                    match('\"'); 

                    }
                    break;
                case 2 :
                    // SimilaritySpec.g:173:10: '\\'' ( STRINGCONTENT | '\"' )* '\\''
                    {
                    match('\''); 
                    // SimilaritySpec.g:173:15: ( STRINGCONTENT | '\"' )*
                    loop2:
                    do {
                        int alt2=2;
                        int LA2_0 = input.LA(1);

                        if ( (LA2_0=='\"'||LA2_0=='+'||(LA2_0>='-' && LA2_0<=':')||LA2_0=='='||LA2_0=='?'||(LA2_0>='A' && LA2_0<='Z')||LA2_0=='\\'||(LA2_0>='a' && LA2_0<='z')) ) {
                            alt2=1;
                        }


                        switch (alt2) {
                    	case 1 :
                    	    // SimilaritySpec.g:
                    	    {
                    	    if ( input.LA(1)=='\"'||input.LA(1)=='+'||(input.LA(1)>='-' && input.LA(1)<=':')||input.LA(1)=='='||input.LA(1)=='?'||(input.LA(1)>='A' && input.LA(1)<='Z')||input.LA(1)=='\\'||(input.LA(1)>='a' && input.LA(1)<='z') ) {
                    	        input.consume();

                    	    }
                    	    else {
                    	        MismatchedSetException mse = new MismatchedSetException(null,input);
                    	        recover(mse);
                    	        throw mse;}


                    	    }
                    	    break;

                    	default :
                    	    break loop2;
                        }
                    } while (true);

                    match('\''); 

                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ATTVALUE"

    // $ANTLR start "STRINGCONTENT"
    public final void mSTRINGCONTENT() throws RecognitionException {
        try {
            // SimilaritySpec.g:177:15: ( 'a' .. 'z' | 'A' .. 'Z' | '\\\\' | '/' | '=' | '+' | '-' | '.' | '?' | ':' | '0' .. '9' )
            // SimilaritySpec.g:
            {
            if ( input.LA(1)=='+'||(input.LA(1)>='-' && input.LA(1)<=':')||input.LA(1)=='='||input.LA(1)=='?'||(input.LA(1)>='A' && input.LA(1)<='Z')||input.LA(1)=='\\'||(input.LA(1)>='a' && input.LA(1)<='z') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}


            }

        }
        finally {
        }
    }
    // $ANTLR end "STRINGCONTENT"

    // $ANTLR start "NCNAME"
    public final void mNCNAME() throws RecognitionException {
        try {
            int _type = NCNAME;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:180:9: ( NameStartChar ( NameChar )* )
            // SimilaritySpec.g:180:11: NameStartChar ( NameChar )*
            {
            mNameStartChar(); 
            // SimilaritySpec.g:180:25: ( NameChar )*
            loop4:
            do {
                int alt4=2;
                int LA4_0 = input.LA(1);

                if ( ((LA4_0>='-' && LA4_0<='.')||(LA4_0>='0' && LA4_0<='9')||(LA4_0>='A' && LA4_0<='Z')||LA4_0=='_'||(LA4_0>='a' && LA4_0<='z')) ) {
                    alt4=1;
                }


                switch (alt4) {
            	case 1 :
            	    // SimilaritySpec.g:180:26: NameChar
            	    {
            	    mNameChar(); 

            	    }
            	    break;

            	default :
            	    break loop4;
                }
            } while (true);


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "NCNAME"

    // $ANTLR start "NameStartChar"
    public final void mNameStartChar() throws RecognitionException {
        try {
            // SimilaritySpec.g:183:17: ( 'A' .. 'Z' | '_' | 'a' .. 'z' )
            // SimilaritySpec.g:
            {
            if ( (input.LA(1)>='A' && input.LA(1)<='Z')||input.LA(1)=='_'||(input.LA(1)>='a' && input.LA(1)<='z') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}


            }

        }
        finally {
        }
    }
    // $ANTLR end "NameStartChar"

    // $ANTLR start "NameChar"
    public final void mNameChar() throws RecognitionException {
        try {
            // SimilaritySpec.g:186:13: ( NameStartChar | '-' | '.' | '0' .. '9' )
            // SimilaritySpec.g:
            {
            if ( (input.LA(1)>='-' && input.LA(1)<='.')||(input.LA(1)>='0' && input.LA(1)<='9')||(input.LA(1)>='A' && input.LA(1)<='Z')||input.LA(1)=='_'||(input.LA(1)>='a' && input.LA(1)<='z') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}


            }

        }
        finally {
        }
    }
    // $ANTLR end "NameChar"

    // $ANTLR start "WS"
    public final void mWS() throws RecognitionException {
        try {
            int _type = WS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // SimilaritySpec.g:188:9: ( ( ' ' | '\\t' | '\\n' | '\\r' )+ )
            // SimilaritySpec.g:188:17: ( ' ' | '\\t' | '\\n' | '\\r' )+
            {
            // SimilaritySpec.g:188:17: ( ' ' | '\\t' | '\\n' | '\\r' )+
            int cnt5=0;
            loop5:
            do {
                int alt5=2;
                int LA5_0 = input.LA(1);

                if ( ((LA5_0>='\t' && LA5_0<='\n')||LA5_0=='\r'||LA5_0==' ') ) {
                    alt5=1;
                }


                switch (alt5) {
            	case 1 :
            	    // SimilaritySpec.g:
            	    {
            	    if ( (input.LA(1)>='\t' && input.LA(1)<='\n')||input.LA(1)=='\r'||input.LA(1)==' ' ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    if ( cnt5 >= 1 ) break loop5;
                        EarlyExitException eee =
                            new EarlyExitException(5, input);
                        throw eee;
                }
                cnt5++;
            } while (true);

             _channel = HIDDEN; 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "WS"

    public void mTokens() throws RecognitionException {
        // SimilaritySpec.g:1:8: ( ALTERNATIVES | ENTRY | OPT | OPTIONAL | MANDAT | MANDATORY | EQUIVSPEC | EXTENDS | XMLNS | XMLNSPREFIX | ELEMENT | LIMIT | ANNOTATIONS | ABSENT | CONSTITUENTS | UNORDERED | ORDERED | TEXTNODE | SPLITNODESTART | SPLITNODEEND | SPLITNODE | COMMENTNODE | IGNORE | DEFAULTSPEC | T__41 | T__42 | T__43 | T__44 | T__45 | T__46 | T__47 | T__48 | T__49 | T__50 | T__51 | ATTVALUE | NCNAME | WS )
        int alt6=38;
        alt6 = dfa6.predict(input);
        switch (alt6) {
            case 1 :
                // SimilaritySpec.g:1:10: ALTERNATIVES
                {
                mALTERNATIVES(); 

                }
                break;
            case 2 :
                // SimilaritySpec.g:1:23: ENTRY
                {
                mENTRY(); 

                }
                break;
            case 3 :
                // SimilaritySpec.g:1:29: OPT
                {
                mOPT(); 

                }
                break;
            case 4 :
                // SimilaritySpec.g:1:33: OPTIONAL
                {
                mOPTIONAL(); 

                }
                break;
            case 5 :
                // SimilaritySpec.g:1:42: MANDAT
                {
                mMANDAT(); 

                }
                break;
            case 6 :
                // SimilaritySpec.g:1:49: MANDATORY
                {
                mMANDATORY(); 

                }
                break;
            case 7 :
                // SimilaritySpec.g:1:59: EQUIVSPEC
                {
                mEQUIVSPEC(); 

                }
                break;
            case 8 :
                // SimilaritySpec.g:1:69: EXTENDS
                {
                mEXTENDS(); 

                }
                break;
            case 9 :
                // SimilaritySpec.g:1:77: XMLNS
                {
                mXMLNS(); 

                }
                break;
            case 10 :
                // SimilaritySpec.g:1:83: XMLNSPREFIX
                {
                mXMLNSPREFIX(); 

                }
                break;
            case 11 :
                // SimilaritySpec.g:1:95: ELEMENT
                {
                mELEMENT(); 

                }
                break;
            case 12 :
                // SimilaritySpec.g:1:103: LIMIT
                {
                mLIMIT(); 

                }
                break;
            case 13 :
                // SimilaritySpec.g:1:109: ANNOTATIONS
                {
                mANNOTATIONS(); 

                }
                break;
            case 14 :
                // SimilaritySpec.g:1:121: ABSENT
                {
                mABSENT(); 

                }
                break;
            case 15 :
                // SimilaritySpec.g:1:128: CONSTITUENTS
                {
                mCONSTITUENTS(); 

                }
                break;
            case 16 :
                // SimilaritySpec.g:1:141: UNORDERED
                {
                mUNORDERED(); 

                }
                break;
            case 17 :
                // SimilaritySpec.g:1:151: ORDERED
                {
                mORDERED(); 

                }
                break;
            case 18 :
                // SimilaritySpec.g:1:159: TEXTNODE
                {
                mTEXTNODE(); 

                }
                break;
            case 19 :
                // SimilaritySpec.g:1:168: SPLITNODESTART
                {
                mSPLITNODESTART(); 

                }
                break;
            case 20 :
                // SimilaritySpec.g:1:183: SPLITNODEEND
                {
                mSPLITNODEEND(); 

                }
                break;
            case 21 :
                // SimilaritySpec.g:1:196: SPLITNODE
                {
                mSPLITNODE(); 

                }
                break;
            case 22 :
                // SimilaritySpec.g:1:206: COMMENTNODE
                {
                mCOMMENTNODE(); 

                }
                break;
            case 23 :
                // SimilaritySpec.g:1:218: IGNORE
                {
                mIGNORE(); 

                }
                break;
            case 24 :
                // SimilaritySpec.g:1:225: DEFAULTSPEC
                {
                mDEFAULTSPEC(); 

                }
                break;
            case 25 :
                // SimilaritySpec.g:1:237: T__41
                {
                mT__41(); 

                }
                break;
            case 26 :
                // SimilaritySpec.g:1:243: T__42
                {
                mT__42(); 

                }
                break;
            case 27 :
                // SimilaritySpec.g:1:249: T__43
                {
                mT__43(); 

                }
                break;
            case 28 :
                // SimilaritySpec.g:1:255: T__44
                {
                mT__44(); 

                }
                break;
            case 29 :
                // SimilaritySpec.g:1:261: T__45
                {
                mT__45(); 

                }
                break;
            case 30 :
                // SimilaritySpec.g:1:267: T__46
                {
                mT__46(); 

                }
                break;
            case 31 :
                // SimilaritySpec.g:1:273: T__47
                {
                mT__47(); 

                }
                break;
            case 32 :
                // SimilaritySpec.g:1:279: T__48
                {
                mT__48(); 

                }
                break;
            case 33 :
                // SimilaritySpec.g:1:285: T__49
                {
                mT__49(); 

                }
                break;
            case 34 :
                // SimilaritySpec.g:1:291: T__50
                {
                mT__50(); 

                }
                break;
            case 35 :
                // SimilaritySpec.g:1:297: T__51
                {
                mT__51(); 

                }
                break;
            case 36 :
                // SimilaritySpec.g:1:303: ATTVALUE
                {
                mATTVALUE(); 

                }
                break;
            case 37 :
                // SimilaritySpec.g:1:312: NCNAME
                {
                mNCNAME(); 

                }
                break;
            case 38 :
                // SimilaritySpec.g:1:319: WS
                {
                mWS(); 

                }
                break;

        }

    }


    protected DFA6 dfa6 = new DFA6(this);
    static final String DFA6_eotS =
        "\1\uffff\2\34\1\uffff\1\34\1\uffff\7\34\1\uffff\1\60\2\34\1\64\4"+
        "\uffff\1\65\7\uffff\16\34\5\uffff\2\34\3\uffff\43\34\1\151\5\34"+
        "\1\160\1\161\3\34\1\165\3\34\1\171\1\uffff\5\34\3\uffff\3\34\1\uffff"+
        "\1\u0082\2\34\1\uffff\3\34\1\u0088\1\u0089\2\34\1\u008c\1\uffff"+
        "\2\34\1\u008f\2\34\2\uffff\2\34\1\uffff\2\34\1\uffff\1\u0096\1\u0097"+
        "\1\34\1\u0099\2\34\2\uffff\1\34\1\uffff\1\34\1\u009e\1\34\1\u00a0"+
        "\1\uffff\1\u00a1\2\uffff";
    static final String DFA6_eofS =
        "\u00a2\uffff";
    static final String DFA6_minS =
        "\1\11\1\142\1\116\1\uffff\1\120\1\uffff\1\141\1\154\1\155\1\151"+
        "\1\157\1\156\1\162\1\103\1\76\1\120\1\147\1\144\4\uffff\1\55\7\uffff"+
        "\1\164\1\156\1\163\2\124\1\156\1\165\1\164\1\145\1\154\1\155\1\156"+
        "\1\157\1\144\5\uffff\1\114\1\156\3\uffff\1\145\1\157\1\145\1\122"+
        "\1\111\1\144\1\151\1\145\1\155\1\156\1\151\1\163\1\162\1\145\1\111"+
        "\1\157\1\162\1\164\1\156\1\131\1\117\1\141\1\166\1\156\1\145\1\163"+
        "\2\164\1\144\1\162\1\124\1\162\1\156\1\141\1\164\1\55\1\116\1\164"+
        "\1\163\1\144\1\156\2\55\1\151\2\145\1\55\1\145\1\141\1\164\1\55"+
        "\1\uffff\1\101\1\157\1\160\1\163\1\164\3\uffff\1\164\1\162\1\144"+
        "\1\uffff\1\55\1\164\1\151\1\uffff\1\114\1\162\1\145\2\55\1\165\1"+
        "\145\1\55\1\uffff\1\151\1\157\1\55\1\171\1\143\2\uffff\1\145\1\144"+
        "\1\uffff\1\166\1\156\1\uffff\2\55\1\156\1\55\1\145\1\163\2\uffff"+
        "\1\164\1\uffff\1\163\1\55\1\163\1\55\1\uffff\1\55\2\uffff";
    static final String DFA6_maxS =
        "\1\175\1\156\1\116\1\uffff\1\120\1\uffff\1\141\1\170\1\155\1\151"+
        "\1\157\1\156\1\162\1\124\1\76\1\120\1\147\1\144\4\uffff\1\172\7"+
        "\uffff\1\164\1\156\1\163\2\124\1\156\1\165\1\164\1\145\1\154\1\155"+
        "\1\156\1\157\1\144\5\uffff\1\114\1\156\3\uffff\1\145\1\157\1\145"+
        "\1\122\1\111\1\144\1\151\1\145\1\155\1\156\1\151\1\163\1\162\1\145"+
        "\1\111\1\157\1\162\1\164\1\156\1\131\1\117\1\141\1\166\1\156\1\145"+
        "\1\163\2\164\1\144\1\162\1\124\1\162\1\156\1\141\1\164\1\172\1\116"+
        "\1\164\1\163\1\144\1\156\2\172\1\151\2\145\1\172\1\145\1\141\1\164"+
        "\1\172\1\uffff\1\101\1\157\1\160\1\163\1\164\3\uffff\1\164\1\162"+
        "\1\144\1\uffff\1\172\1\164\1\151\1\uffff\1\114\1\162\1\145\2\172"+
        "\1\165\1\145\1\172\1\uffff\1\151\1\157\1\172\1\171\1\143\2\uffff"+
        "\1\145\1\144\1\uffff\1\166\1\156\1\uffff\2\172\1\156\1\172\1\145"+
        "\1\163\2\uffff\1\164\1\uffff\1\163\1\172\1\163\1\172\1\uffff\1\172"+
        "\2\uffff";
    static final String DFA6_acceptS =
        "\3\uffff\1\3\1\uffff\1\5\14\uffff\1\31\1\32\1\33\1\34\1\uffff\1"+
        "\36\1\37\1\41\1\42\1\44\1\45\1\46\16\uffff\1\22\1\23\1\26\1\24\1"+
        "\40\2\uffff\1\30\1\43\1\35\63\uffff\1\2\5\uffff\1\12\1\11\1\14\3"+
        "\uffff\1\25\3\uffff\1\16\10\uffff\1\27\5\uffff\1\10\1\13\2\uffff"+
        "\1\21\2\uffff\1\4\6\uffff\1\6\1\7\1\uffff\1\20\4\uffff\1\15\1\uffff"+
        "\1\1\1\17";
    static final String DFA6_specialS =
        "\u00a2\uffff}>";
    static final String[] DFA6_transitionS = {
            "\2\35\2\uffff\1\35\22\uffff\1\35\1\5\1\33\4\uffff\1\33\1\30"+
            "\1\16\2\uffff\1\24\15\uffff\1\21\1\uffff\1\15\1\25\1\uffff\1"+
            "\3\1\uffff\4\34\1\2\11\34\1\4\3\34\1\17\7\34\1\31\1\27\1\32"+
            "\1\uffff\1\26\1\uffff\1\1\1\34\1\12\1\34\1\7\3\34\1\20\2\34"+
            "\1\11\1\6\1\34\1\14\5\34\1\13\2\34\1\10\2\34\1\22\1\uffff\1"+
            "\23",
            "\1\40\11\uffff\1\36\1\uffff\1\37",
            "\1\41",
            "",
            "\1\42",
            "",
            "\1\43",
            "\1\46\4\uffff\1\44\6\uffff\1\45",
            "\1\47",
            "\1\50",
            "\1\51",
            "\1\52",
            "\1\53",
            "\1\56\17\uffff\1\55\1\54",
            "\1\57",
            "\1\61",
            "\1\62",
            "\1\63",
            "",
            "",
            "",
            "",
            "\2\34\1\uffff\12\34\7\uffff\32\34\4\uffff\1\34\1\uffff\32\34",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "\1\66",
            "\1\67",
            "\1\70",
            "\1\71",
            "\1\72",
            "\1\73",
            "\1\74",
            "\1\75",
            "\1\76",
            "\1\77",
            "\1\100",
            "\1\101",
            "\1\102",
            "\1\103",
            "",
            "",
            "",
            "",
            "",
            "\1\104",
            "\1\105",
            "",
            "",
            "",
            "\1\106",
            "\1\107",
            "\1\110",
            "\1\111",
            "\1\112",
            "\1\113",
            "\1\114",
            "\1\115",
            "\1\116",
            "\1\117",
            "\1\120",
            "\1\121",
            "\1\122",
            "\1\123",
            "\1\124",
            "\1\125",
            "\1\126",
            "\1\127",
            "\1\130",
            "\1\131",
            "\1\132",
            "\1\133",
            "\1\134",
            "\1\135",
            "\1\136",
            "\1\137",
            "\1\140",
            "\1\141",
            "\1\142",
            "\1\143",
            "\1\144",
            "\1\145",
            "\1\146",
            "\1\147",
            "\1\150",
            "\2\34\1\uffff\12\34\7\uffff\32\34\4\uffff\1\34\1\uffff\32\34",
            "\1\152",
            "\1\153",
            "\1\154",
            "\1\155",
            "\1\156",
            "\2\34\1\uffff\12\34\1\157\6\uffff\32\34\4\uffff\1\34\1\uffff"+
            "\32\34",
            "\2\34\1\uffff\12\34\7\uffff\32\34\4\uffff\1\34\1\uffff\32\34",
            "\1\162",
            "\1\163",
            "\1\164",
            "\2\34\1\uffff\12\34\7\uffff\32\34\4\uffff\1\34\1\uffff\32\34",
            "\1\166",
            "\1\167",
            "\1\170",
            "\2\34\1\uffff\12\34\7\uffff\32\34\4\uffff\1\34\1\uffff\32\34",
            "",
            "\1\172",
            "\1\173",
            "\1\174",
            "\1\175",
            "\1\176",
            "",
            "",
            "",
            "\1\177",
            "\1\u0080",
            "\1\u0081",
            "",
            "\2\34\1\uffff\12\34\7\uffff\32\34\4\uffff\1\34\1\uffff\32\34",
            "\1\u0083",
            "\1\u0084",
            "",
            "\1\u0085",
            "\1\u0086",
            "\1\u0087",
            "\2\34\1\uffff\12\34\7\uffff\32\34\4\uffff\1\34\1\uffff\32\34",
            "\2\34\1\uffff\12\34\7\uffff\32\34\4\uffff\1\34\1\uffff\32\34",
            "\1\u008a",
            "\1\u008b",
            "\2\34\1\uffff\12\34\7\uffff\32\34\4\uffff\1\34\1\uffff\32\34",
            "",
            "\1\u008d",
            "\1\u008e",
            "\2\34\1\uffff\12\34\7\uffff\32\34\4\uffff\1\34\1\uffff\32\34",
            "\1\u0090",
            "\1\u0091",
            "",
            "",
            "\1\u0092",
            "\1\u0093",
            "",
            "\1\u0094",
            "\1\u0095",
            "",
            "\2\34\1\uffff\12\34\7\uffff\32\34\4\uffff\1\34\1\uffff\32\34",
            "\2\34\1\uffff\12\34\7\uffff\32\34\4\uffff\1\34\1\uffff\32\34",
            "\1\u0098",
            "\2\34\1\uffff\12\34\7\uffff\32\34\4\uffff\1\34\1\uffff\32\34",
            "\1\u009a",
            "\1\u009b",
            "",
            "",
            "\1\u009c",
            "",
            "\1\u009d",
            "\2\34\1\uffff\12\34\7\uffff\32\34\4\uffff\1\34\1\uffff\32\34",
            "\1\u009f",
            "\2\34\1\uffff\12\34\7\uffff\32\34\4\uffff\1\34\1\uffff\32\34",
            "",
            "\2\34\1\uffff\12\34\7\uffff\32\34\4\uffff\1\34\1\uffff\32\34",
            "",
            ""
    };

    static final short[] DFA6_eot = DFA.unpackEncodedString(DFA6_eotS);
    static final short[] DFA6_eof = DFA.unpackEncodedString(DFA6_eofS);
    static final char[] DFA6_min = DFA.unpackEncodedStringToUnsignedChars(DFA6_minS);
    static final char[] DFA6_max = DFA.unpackEncodedStringToUnsignedChars(DFA6_maxS);
    static final short[] DFA6_accept = DFA.unpackEncodedString(DFA6_acceptS);
    static final short[] DFA6_special = DFA.unpackEncodedString(DFA6_specialS);
    static final short[][] DFA6_transition;

    static {
        int numStates = DFA6_transitionS.length;
        DFA6_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA6_transition[i] = DFA.unpackEncodedString(DFA6_transitionS[i]);
        }
    }

    class DFA6 extends DFA {

        public DFA6(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 6;
            this.eot = DFA6_eot;
            this.eof = DFA6_eof;
            this.min = DFA6_min;
            this.max = DFA6_max;
            this.accept = DFA6_accept;
            this.special = DFA6_special;
            this.transition = DFA6_transition;
        }
        public String getDescription() {
            return "1:1: Tokens : ( ALTERNATIVES | ENTRY | OPT | OPTIONAL | MANDAT | MANDATORY | EQUIVSPEC | EXTENDS | XMLNS | XMLNSPREFIX | ELEMENT | LIMIT | ANNOTATIONS | ABSENT | CONSTITUENTS | UNORDERED | ORDERED | TEXTNODE | SPLITNODESTART | SPLITNODEEND | SPLITNODE | COMMENTNODE | IGNORE | DEFAULTSPEC | T__41 | T__42 | T__43 | T__44 | T__45 | T__46 | T__47 | T__48 | T__49 | T__50 | T__51 | ATTVALUE | NCNAME | WS );";
        }
    }
 

}