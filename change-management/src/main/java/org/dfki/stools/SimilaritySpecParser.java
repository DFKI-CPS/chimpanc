// $ANTLR 3.3 Nov 30, 2010 12:50:56 SimilaritySpec.g 2012-03-21 12:37:26

package org.dfki.stools;


import org.antlr.runtime.*;
import org.antlr.runtime.tree.*;

public class SimilaritySpecParser extends Parser {
    public static final String[] tokenNames = new String[] {
        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "ALTERNATIVES", "ALT", "ENTRY", "ALLBUT", "OPT", "OPTIONAL", "MANDAT", "MANDATORY", "EQUIVSPECS", "EQUIVSPEC", "EXTENDS", "XMLNS", "XMLNSPREFIX", "ELEMENT", "LIMIT", "ANNOTATIONS", "ABSENT", "CONSTITUENTS", "UNORDERED", "ORDERED", "TEXTNODE", "SPLITNODESTART", "SPLITNODEEND", "SPLITNODE", "COMMENTNODE", "IGNORE", "PREFIXEDNAME", "UNPREFIXEDNAME", "TUPLE", "EQUIVELEM", "DEFAULTSPEC", "NCNAME", "ATTVALUE", "STRINGCONTENT", "NameStartChar", "NameChar", "WS", "'{'", "'}'", "','", "'='", "'_'", "'\\\\'", "'('", "')'", "'['", "']'", "':'"
    };
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


        public SimilaritySpecParser(TokenStream input) {
            this(input, new RecognizerSharedState());
        }
        public SimilaritySpecParser(TokenStream input, RecognizerSharedState state) {
            super(input, state);
             
        }
        
    protected TreeAdaptor adaptor = new CommonTreeAdaptor();

    public void setTreeAdaptor(TreeAdaptor adaptor) {
        this.adaptor = adaptor;
    }
    public TreeAdaptor getTreeAdaptor() {
        return adaptor;
    }

    public String[] getTokenNames() { return SimilaritySpecParser.tokenNames; }
    public String getGrammarFileName() { return "SimilaritySpec.g"; }


    public static class equivspecs_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "equivspecs"
    // SimilaritySpec.g:51:1: equivspecs : ( equivspec )* -> ^( EQUIVSPECS ( equivspec )* ) ;
    public final SimilaritySpecParser.equivspecs_return equivspecs() throws RecognitionException {
        SimilaritySpecParser.equivspecs_return retval = new SimilaritySpecParser.equivspecs_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        SimilaritySpecParser.equivspec_return equivspec1 = null;


        RewriteRuleSubtreeStream stream_equivspec=new RewriteRuleSubtreeStream(adaptor,"rule equivspec");
        try {
            // SimilaritySpec.g:52:5: ( ( equivspec )* -> ^( EQUIVSPECS ( equivspec )* ) )
            // SimilaritySpec.g:52:10: ( equivspec )*
            {
            // SimilaritySpec.g:52:10: ( equivspec )*
            loop1:
            do {
                int alt1=2;
                int LA1_0 = input.LA(1);

                if ( (LA1_0==EQUIVSPEC) ) {
                    alt1=1;
                }


                switch (alt1) {
            	case 1 :
            	    // SimilaritySpec.g:0:0: equivspec
            	    {
            	    pushFollow(FOLLOW_equivspec_in_equivspecs233);
            	    equivspec1=equivspec();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) stream_equivspec.add(equivspec1.getTree());

            	    }
            	    break;

            	default :
            	    break loop1;
                }
            } while (true);



            // AST REWRITE
            // elements: equivspec
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 52:21: -> ^( EQUIVSPECS ( equivspec )* )
            {
                // SimilaritySpec.g:52:24: ^( EQUIVSPECS ( equivspec )* )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(EQUIVSPECS, "EQUIVSPECS"), root_1);

                // SimilaritySpec.g:52:37: ( equivspec )*
                while ( stream_equivspec.hasNext() ) {
                    adaptor.addChild(root_1, stream_equivspec.nextTree());

                }
                stream_equivspec.reset();

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "equivspecs"

    public static class equivspec_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "equivspec"
    // SimilaritySpec.g:55:1: equivspec : EQUIVSPEC NCNAME ( namespace | extension )? '{' ( xmlnsdecls )* ( element | glblmt | ignore )* ( defaultspec )? '}' ;
    public final SimilaritySpecParser.equivspec_return equivspec() throws RecognitionException {
        SimilaritySpecParser.equivspec_return retval = new SimilaritySpecParser.equivspec_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token EQUIVSPEC2=null;
        Token NCNAME3=null;
        Token char_literal6=null;
        Token char_literal12=null;
        SimilaritySpecParser.namespace_return namespace4 = null;

        SimilaritySpecParser.extension_return extension5 = null;

        SimilaritySpecParser.xmlnsdecls_return xmlnsdecls7 = null;

        SimilaritySpecParser.element_return element8 = null;

        SimilaritySpecParser.glblmt_return glblmt9 = null;

        SimilaritySpecParser.ignore_return ignore10 = null;

        SimilaritySpecParser.defaultspec_return defaultspec11 = null;


        Object EQUIVSPEC2_tree=null;
        Object NCNAME3_tree=null;
        Object char_literal6_tree=null;
        Object char_literal12_tree=null;

        try {
            // SimilaritySpec.g:56:2: ( EQUIVSPEC NCNAME ( namespace | extension )? '{' ( xmlnsdecls )* ( element | glblmt | ignore )* ( defaultspec )? '}' )
            // SimilaritySpec.g:56:5: EQUIVSPEC NCNAME ( namespace | extension )? '{' ( xmlnsdecls )* ( element | glblmt | ignore )* ( defaultspec )? '}'
            {
            root_0 = (Object)adaptor.nil();

            EQUIVSPEC2=(Token)match(input,EQUIVSPEC,FOLLOW_EQUIVSPEC_in_equivspec259); if (state.failed) return retval;
            if ( state.backtracking==0 ) {
            EQUIVSPEC2_tree = (Object)adaptor.create(EQUIVSPEC2);
            root_0 = (Object)adaptor.becomeRoot(EQUIVSPEC2_tree, root_0);
            }
            NCNAME3=(Token)match(input,NCNAME,FOLLOW_NCNAME_in_equivspec262); if (state.failed) return retval;
            if ( state.backtracking==0 ) {
            NCNAME3_tree = (Object)adaptor.create(NCNAME3);
            adaptor.addChild(root_0, NCNAME3_tree);
            }
            // SimilaritySpec.g:56:23: ( namespace | extension )?
            int alt2=3;
            int LA2_0 = input.LA(1);

            if ( (LA2_0==XMLNS) ) {
                alt2=1;
            }
            else if ( (LA2_0==EXTENDS) ) {
                alt2=2;
            }
            switch (alt2) {
                case 1 :
                    // SimilaritySpec.g:56:24: namespace
                    {
                    pushFollow(FOLLOW_namespace_in_equivspec265);
                    namespace4=namespace();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, namespace4.getTree());

                    }
                    break;
                case 2 :
                    // SimilaritySpec.g:56:36: extension
                    {
                    pushFollow(FOLLOW_extension_in_equivspec269);
                    extension5=extension();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, extension5.getTree());

                    }
                    break;

            }

            char_literal6=(Token)match(input,41,FOLLOW_41_in_equivspec273); if (state.failed) return retval;
            // SimilaritySpec.g:56:53: ( xmlnsdecls )*
            loop3:
            do {
                int alt3=2;
                int LA3_0 = input.LA(1);

                if ( (LA3_0==XMLNSPREFIX) ) {
                    alt3=1;
                }


                switch (alt3) {
            	case 1 :
            	    // SimilaritySpec.g:56:54: xmlnsdecls
            	    {
            	    pushFollow(FOLLOW_xmlnsdecls_in_equivspec277);
            	    xmlnsdecls7=xmlnsdecls();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) adaptor.addChild(root_0, xmlnsdecls7.getTree());

            	    }
            	    break;

            	default :
            	    break loop3;
                }
            } while (true);

            // SimilaritySpec.g:56:67: ( element | glblmt | ignore )*
            loop4:
            do {
                int alt4=4;
                switch ( input.LA(1) ) {
                case ELEMENT:
                    {
                    alt4=1;
                    }
                    break;
                case LIMIT:
                    {
                    alt4=2;
                    }
                    break;
                case IGNORE:
                    {
                    alt4=3;
                    }
                    break;

                }

                switch (alt4) {
            	case 1 :
            	    // SimilaritySpec.g:56:68: element
            	    {
            	    pushFollow(FOLLOW_element_in_equivspec282);
            	    element8=element();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) adaptor.addChild(root_0, element8.getTree());

            	    }
            	    break;
            	case 2 :
            	    // SimilaritySpec.g:56:78: glblmt
            	    {
            	    pushFollow(FOLLOW_glblmt_in_equivspec286);
            	    glblmt9=glblmt();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) adaptor.addChild(root_0, glblmt9.getTree());

            	    }
            	    break;
            	case 3 :
            	    // SimilaritySpec.g:56:87: ignore
            	    {
            	    pushFollow(FOLLOW_ignore_in_equivspec290);
            	    ignore10=ignore();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) adaptor.addChild(root_0, ignore10.getTree());

            	    }
            	    break;

            	default :
            	    break loop4;
                }
            } while (true);

            // SimilaritySpec.g:56:97: ( defaultspec )?
            int alt5=2;
            int LA5_0 = input.LA(1);

            if ( (LA5_0==DEFAULTSPEC) ) {
                alt5=1;
            }
            switch (alt5) {
                case 1 :
                    // SimilaritySpec.g:0:0: defaultspec
                    {
                    pushFollow(FOLLOW_defaultspec_in_equivspec295);
                    defaultspec11=defaultspec();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, defaultspec11.getTree());

                    }
                    break;

            }

            char_literal12=(Token)match(input,42,FOLLOW_42_in_equivspec298); if (state.failed) return retval;

            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "equivspec"

    public static class extension_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "extension"
    // SimilaritySpec.g:59:1: extension : EXTENDS NCNAME ( ( ',' )? NCNAME )* -> ^( EXTENDS ( NCNAME )+ ) ;
    public final SimilaritySpecParser.extension_return extension() throws RecognitionException {
        SimilaritySpecParser.extension_return retval = new SimilaritySpecParser.extension_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token EXTENDS13=null;
        Token NCNAME14=null;
        Token char_literal15=null;
        Token NCNAME16=null;

        Object EXTENDS13_tree=null;
        Object NCNAME14_tree=null;
        Object char_literal15_tree=null;
        Object NCNAME16_tree=null;
        RewriteRuleTokenStream stream_NCNAME=new RewriteRuleTokenStream(adaptor,"token NCNAME");
        RewriteRuleTokenStream stream_43=new RewriteRuleTokenStream(adaptor,"token 43");
        RewriteRuleTokenStream stream_EXTENDS=new RewriteRuleTokenStream(adaptor,"token EXTENDS");

        try {
            // SimilaritySpec.g:60:2: ( EXTENDS NCNAME ( ( ',' )? NCNAME )* -> ^( EXTENDS ( NCNAME )+ ) )
            // SimilaritySpec.g:60:5: EXTENDS NCNAME ( ( ',' )? NCNAME )*
            {
            EXTENDS13=(Token)match(input,EXTENDS,FOLLOW_EXTENDS_in_extension320); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_EXTENDS.add(EXTENDS13);

            NCNAME14=(Token)match(input,NCNAME,FOLLOW_NCNAME_in_extension322); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_NCNAME.add(NCNAME14);

            // SimilaritySpec.g:60:20: ( ( ',' )? NCNAME )*
            loop7:
            do {
                int alt7=2;
                int LA7_0 = input.LA(1);

                if ( (LA7_0==NCNAME||LA7_0==43) ) {
                    alt7=1;
                }


                switch (alt7) {
            	case 1 :
            	    // SimilaritySpec.g:60:21: ( ',' )? NCNAME
            	    {
            	    // SimilaritySpec.g:60:21: ( ',' )?
            	    int alt6=2;
            	    int LA6_0 = input.LA(1);

            	    if ( (LA6_0==43) ) {
            	        alt6=1;
            	    }
            	    switch (alt6) {
            	        case 1 :
            	            // SimilaritySpec.g:0:0: ','
            	            {
            	            char_literal15=(Token)match(input,43,FOLLOW_43_in_extension325); if (state.failed) return retval; 
            	            if ( state.backtracking==0 ) stream_43.add(char_literal15);


            	            }
            	            break;

            	    }

            	    NCNAME16=(Token)match(input,NCNAME,FOLLOW_NCNAME_in_extension328); if (state.failed) return retval; 
            	    if ( state.backtracking==0 ) stream_NCNAME.add(NCNAME16);


            	    }
            	    break;

            	default :
            	    break loop7;
                }
            } while (true);



            // AST REWRITE
            // elements: NCNAME, EXTENDS
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 60:36: -> ^( EXTENDS ( NCNAME )+ )
            {
                // SimilaritySpec.g:60:39: ^( EXTENDS ( NCNAME )+ )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot(stream_EXTENDS.nextNode(), root_1);

                if ( !(stream_NCNAME.hasNext()) ) {
                    throw new RewriteEarlyExitException();
                }
                while ( stream_NCNAME.hasNext() ) {
                    adaptor.addChild(root_1, stream_NCNAME.nextNode());

                }
                stream_NCNAME.reset();

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "extension"

    public static class xmlnsdecls_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "xmlnsdecls"
    // SimilaritySpec.g:62:1: xmlnsdecls : XMLNSPREFIX NCNAME '=' ATTVALUE ;
    public final SimilaritySpecParser.xmlnsdecls_return xmlnsdecls() throws RecognitionException {
        SimilaritySpecParser.xmlnsdecls_return retval = new SimilaritySpecParser.xmlnsdecls_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token XMLNSPREFIX17=null;
        Token NCNAME18=null;
        Token char_literal19=null;
        Token ATTVALUE20=null;

        Object XMLNSPREFIX17_tree=null;
        Object NCNAME18_tree=null;
        Object char_literal19_tree=null;
        Object ATTVALUE20_tree=null;

        try {
            // SimilaritySpec.g:63:2: ( XMLNSPREFIX NCNAME '=' ATTVALUE )
            // SimilaritySpec.g:63:5: XMLNSPREFIX NCNAME '=' ATTVALUE
            {
            root_0 = (Object)adaptor.nil();

            XMLNSPREFIX17=(Token)match(input,XMLNSPREFIX,FOLLOW_XMLNSPREFIX_in_xmlnsdecls352); if (state.failed) return retval;
            if ( state.backtracking==0 ) {
            XMLNSPREFIX17_tree = (Object)adaptor.create(XMLNSPREFIX17);
            root_0 = (Object)adaptor.becomeRoot(XMLNSPREFIX17_tree, root_0);
            }
            NCNAME18=(Token)match(input,NCNAME,FOLLOW_NCNAME_in_xmlnsdecls355); if (state.failed) return retval;
            if ( state.backtracking==0 ) {
            NCNAME18_tree = (Object)adaptor.create(NCNAME18);
            adaptor.addChild(root_0, NCNAME18_tree);
            }
            char_literal19=(Token)match(input,44,FOLLOW_44_in_xmlnsdecls357); if (state.failed) return retval;
            ATTVALUE20=(Token)match(input,ATTVALUE,FOLLOW_ATTVALUE_in_xmlnsdecls360); if (state.failed) return retval;
            if ( state.backtracking==0 ) {
            ATTVALUE20_tree = (Object)adaptor.create(ATTVALUE20);
            adaptor.addChild(root_0, ATTVALUE20_tree);
            }

            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "xmlnsdecls"

    public static class namespace_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "namespace"
    // SimilaritySpec.g:65:1: namespace : XMLNS ATTVALUE ;
    public final SimilaritySpecParser.namespace_return namespace() throws RecognitionException {
        SimilaritySpecParser.namespace_return retval = new SimilaritySpecParser.namespace_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token XMLNS21=null;
        Token ATTVALUE22=null;

        Object XMLNS21_tree=null;
        Object ATTVALUE22_tree=null;

        try {
            // SimilaritySpec.g:66:2: ( XMLNS ATTVALUE )
            // SimilaritySpec.g:66:5: XMLNS ATTVALUE
            {
            root_0 = (Object)adaptor.nil();

            XMLNS21=(Token)match(input,XMLNS,FOLLOW_XMLNS_in_namespace373); if (state.failed) return retval;
            if ( state.backtracking==0 ) {
            XMLNS21_tree = (Object)adaptor.create(XMLNS21);
            root_0 = (Object)adaptor.becomeRoot(XMLNS21_tree, root_0);
            }
            ATTVALUE22=(Token)match(input,ATTVALUE,FOLLOW_ATTVALUE_in_namespace376); if (state.failed) return retval;
            if ( state.backtracking==0 ) {
            ATTVALUE22_tree = (Object)adaptor.create(ATTVALUE22);
            adaptor.addChild(root_0, ATTVALUE22_tree);
            }

            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "namespace"

    public static class element_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "element"
    // SimilaritySpec.g:68:1: element : ELEMENT simpleelem '{' ( alternative )? '}' ;
    public final SimilaritySpecParser.element_return element() throws RecognitionException {
        SimilaritySpecParser.element_return retval = new SimilaritySpecParser.element_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token ELEMENT23=null;
        Token char_literal25=null;
        Token char_literal27=null;
        SimilaritySpecParser.simpleelem_return simpleelem24 = null;

        SimilaritySpecParser.alternative_return alternative26 = null;


        Object ELEMENT23_tree=null;
        Object char_literal25_tree=null;
        Object char_literal27_tree=null;

        try {
            // SimilaritySpec.g:69:2: ( ELEMENT simpleelem '{' ( alternative )? '}' )
            // SimilaritySpec.g:69:5: ELEMENT simpleelem '{' ( alternative )? '}'
            {
            root_0 = (Object)adaptor.nil();

            ELEMENT23=(Token)match(input,ELEMENT,FOLLOW_ELEMENT_in_element391); if (state.failed) return retval;
            if ( state.backtracking==0 ) {
            ELEMENT23_tree = (Object)adaptor.create(ELEMENT23);
            root_0 = (Object)adaptor.becomeRoot(ELEMENT23_tree, root_0);
            }
            pushFollow(FOLLOW_simpleelem_in_element394);
            simpleelem24=simpleelem();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) adaptor.addChild(root_0, simpleelem24.getTree());
            char_literal25=(Token)match(input,41,FOLLOW_41_in_element396); if (state.failed) return retval;
            // SimilaritySpec.g:69:30: ( alternative )?
            int alt8=2;
            int LA8_0 = input.LA(1);

            if ( (LA8_0==ALTERNATIVES||LA8_0==ANNOTATIONS||LA8_0==CONSTITUENTS) ) {
                alt8=1;
            }
            switch (alt8) {
                case 1 :
                    // SimilaritySpec.g:0:0: alternative
                    {
                    pushFollow(FOLLOW_alternative_in_element399);
                    alternative26=alternative();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, alternative26.getTree());

                    }
                    break;

            }

            char_literal27=(Token)match(input,42,FOLLOW_42_in_element402); if (state.failed) return retval;

            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "element"

    public static class defaultspec_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "defaultspec"
    // SimilaritySpec.g:71:1: defaultspec : DEFAULTSPEC '{' ( alternative )? '}' ;
    public final SimilaritySpecParser.defaultspec_return defaultspec() throws RecognitionException {
        SimilaritySpecParser.defaultspec_return retval = new SimilaritySpecParser.defaultspec_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token DEFAULTSPEC28=null;
        Token char_literal29=null;
        Token char_literal31=null;
        SimilaritySpecParser.alternative_return alternative30 = null;


        Object DEFAULTSPEC28_tree=null;
        Object char_literal29_tree=null;
        Object char_literal31_tree=null;

        try {
            // SimilaritySpec.g:71:14: ( DEFAULTSPEC '{' ( alternative )? '}' )
            // SimilaritySpec.g:71:17: DEFAULTSPEC '{' ( alternative )? '}'
            {
            root_0 = (Object)adaptor.nil();

            DEFAULTSPEC28=(Token)match(input,DEFAULTSPEC,FOLLOW_DEFAULTSPEC_in_defaultspec413); if (state.failed) return retval;
            if ( state.backtracking==0 ) {
            DEFAULTSPEC28_tree = (Object)adaptor.create(DEFAULTSPEC28);
            root_0 = (Object)adaptor.becomeRoot(DEFAULTSPEC28_tree, root_0);
            }
            char_literal29=(Token)match(input,41,FOLLOW_41_in_defaultspec416); if (state.failed) return retval;
            // SimilaritySpec.g:71:35: ( alternative )?
            int alt9=2;
            int LA9_0 = input.LA(1);

            if ( (LA9_0==ALTERNATIVES||LA9_0==ANNOTATIONS||LA9_0==CONSTITUENTS) ) {
                alt9=1;
            }
            switch (alt9) {
                case 1 :
                    // SimilaritySpec.g:0:0: alternative
                    {
                    pushFollow(FOLLOW_alternative_in_defaultspec419);
                    alternative30=alternative();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, alternative30.getTree());

                    }
                    break;

            }

            char_literal31=(Token)match(input,42,FOLLOW_42_in_defaultspec422); if (state.failed) return retval;

            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "defaultspec"

    public static class glblmt_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "glblmt"
    // SimilaritySpec.g:73:1: glblmt : LIMIT '{' ( simpleelem ( ( ',' )? simpleelem )* )? '}' ;
    public final SimilaritySpecParser.glblmt_return glblmt() throws RecognitionException {
        SimilaritySpecParser.glblmt_return retval = new SimilaritySpecParser.glblmt_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token LIMIT32=null;
        Token char_literal33=null;
        Token char_literal35=null;
        Token char_literal37=null;
        SimilaritySpecParser.simpleelem_return simpleelem34 = null;

        SimilaritySpecParser.simpleelem_return simpleelem36 = null;


        Object LIMIT32_tree=null;
        Object char_literal33_tree=null;
        Object char_literal35_tree=null;
        Object char_literal37_tree=null;

        try {
            // SimilaritySpec.g:74:2: ( LIMIT '{' ( simpleelem ( ( ',' )? simpleelem )* )? '}' )
            // SimilaritySpec.g:74:5: LIMIT '{' ( simpleelem ( ( ',' )? simpleelem )* )? '}'
            {
            root_0 = (Object)adaptor.nil();

            LIMIT32=(Token)match(input,LIMIT,FOLLOW_LIMIT_in_glblmt439); if (state.failed) return retval;
            if ( state.backtracking==0 ) {
            LIMIT32_tree = (Object)adaptor.create(LIMIT32);
            root_0 = (Object)adaptor.becomeRoot(LIMIT32_tree, root_0);
            }
            char_literal33=(Token)match(input,41,FOLLOW_41_in_glblmt442); if (state.failed) return retval;
            // SimilaritySpec.g:74:17: ( simpleelem ( ( ',' )? simpleelem )* )?
            int alt12=2;
            int LA12_0 = input.LA(1);

            if ( ((LA12_0>=TEXTNODE && LA12_0<=SPLITNODESTART)||LA12_0==COMMENTNODE||LA12_0==NCNAME) ) {
                alt12=1;
            }
            switch (alt12) {
                case 1 :
                    // SimilaritySpec.g:74:18: simpleelem ( ( ',' )? simpleelem )*
                    {
                    pushFollow(FOLLOW_simpleelem_in_glblmt446);
                    simpleelem34=simpleelem();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, simpleelem34.getTree());
                    // SimilaritySpec.g:74:29: ( ( ',' )? simpleelem )*
                    loop11:
                    do {
                        int alt11=2;
                        int LA11_0 = input.LA(1);

                        if ( ((LA11_0>=TEXTNODE && LA11_0<=SPLITNODESTART)||LA11_0==COMMENTNODE||LA11_0==NCNAME||LA11_0==43) ) {
                            alt11=1;
                        }


                        switch (alt11) {
                    	case 1 :
                    	    // SimilaritySpec.g:74:31: ( ',' )? simpleelem
                    	    {
                    	    // SimilaritySpec.g:74:31: ( ',' )?
                    	    int alt10=2;
                    	    int LA10_0 = input.LA(1);

                    	    if ( (LA10_0==43) ) {
                    	        alt10=1;
                    	    }
                    	    switch (alt10) {
                    	        case 1 :
                    	            // SimilaritySpec.g:74:32: ','
                    	            {
                    	            char_literal35=(Token)match(input,43,FOLLOW_43_in_glblmt451); if (state.failed) return retval;

                    	            }
                    	            break;

                    	    }

                    	    pushFollow(FOLLOW_simpleelem_in_glblmt456);
                    	    simpleelem36=simpleelem();

                    	    state._fsp--;
                    	    if (state.failed) return retval;
                    	    if ( state.backtracking==0 ) adaptor.addChild(root_0, simpleelem36.getTree());

                    	    }
                    	    break;

                    	default :
                    	    break loop11;
                        }
                    } while (true);


                    }
                    break;

            }

            char_literal37=(Token)match(input,42,FOLLOW_42_in_glblmt463); if (state.failed) return retval;

            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "glblmt"

    public static class alternative_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "alternative"
    // SimilaritySpec.g:76:1: alternative : ( ALTERNATIVES '{' ( '{' altentry '}' )+ '}' | altentry -> ^( ALTERNATIVES altentry ) );
    public final SimilaritySpecParser.alternative_return alternative() throws RecognitionException {
        SimilaritySpecParser.alternative_return retval = new SimilaritySpecParser.alternative_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token ALTERNATIVES38=null;
        Token char_literal39=null;
        Token char_literal40=null;
        Token char_literal42=null;
        Token char_literal43=null;
        SimilaritySpecParser.altentry_return altentry41 = null;

        SimilaritySpecParser.altentry_return altentry44 = null;


        Object ALTERNATIVES38_tree=null;
        Object char_literal39_tree=null;
        Object char_literal40_tree=null;
        Object char_literal42_tree=null;
        Object char_literal43_tree=null;
        RewriteRuleSubtreeStream stream_altentry=new RewriteRuleSubtreeStream(adaptor,"rule altentry");
        try {
            // SimilaritySpec.g:77:2: ( ALTERNATIVES '{' ( '{' altentry '}' )+ '}' | altentry -> ^( ALTERNATIVES altentry ) )
            int alt14=2;
            int LA14_0 = input.LA(1);

            if ( (LA14_0==ALTERNATIVES) ) {
                alt14=1;
            }
            else if ( (LA14_0==ANNOTATIONS||LA14_0==CONSTITUENTS) ) {
                alt14=2;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 14, 0, input);

                throw nvae;
            }
            switch (alt14) {
                case 1 :
                    // SimilaritySpec.g:77:5: ALTERNATIVES '{' ( '{' altentry '}' )+ '}'
                    {
                    root_0 = (Object)adaptor.nil();

                    ALTERNATIVES38=(Token)match(input,ALTERNATIVES,FOLLOW_ALTERNATIVES_in_alternative475); if (state.failed) return retval;
                    if ( state.backtracking==0 ) {
                    ALTERNATIVES38_tree = (Object)adaptor.create(ALTERNATIVES38);
                    root_0 = (Object)adaptor.becomeRoot(ALTERNATIVES38_tree, root_0);
                    }
                    char_literal39=(Token)match(input,41,FOLLOW_41_in_alternative478); if (state.failed) return retval;
                    // SimilaritySpec.g:77:24: ( '{' altentry '}' )+
                    int cnt13=0;
                    loop13:
                    do {
                        int alt13=2;
                        int LA13_0 = input.LA(1);

                        if ( (LA13_0==41) ) {
                            alt13=1;
                        }


                        switch (alt13) {
                    	case 1 :
                    	    // SimilaritySpec.g:77:26: '{' altentry '}'
                    	    {
                    	    char_literal40=(Token)match(input,41,FOLLOW_41_in_alternative483); if (state.failed) return retval;
                    	    pushFollow(FOLLOW_altentry_in_alternative486);
                    	    altentry41=altentry();

                    	    state._fsp--;
                    	    if (state.failed) return retval;
                    	    if ( state.backtracking==0 ) adaptor.addChild(root_0, altentry41.getTree());
                    	    char_literal42=(Token)match(input,42,FOLLOW_42_in_alternative488); if (state.failed) return retval;

                    	    }
                    	    break;

                    	default :
                    	    if ( cnt13 >= 1 ) break loop13;
                    	    if (state.backtracking>0) {state.failed=true; return retval;}
                                EarlyExitException eee =
                                    new EarlyExitException(13, input);
                                throw eee;
                        }
                        cnt13++;
                    } while (true);

                    char_literal43=(Token)match(input,42,FOLLOW_42_in_alternative494); if (state.failed) return retval;

                    }
                    break;
                case 2 :
                    // SimilaritySpec.g:78:18: altentry
                    {
                    pushFollow(FOLLOW_altentry_in_alternative514);
                    altentry44=altentry();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_altentry.add(altentry44.getTree());


                    // AST REWRITE
                    // elements: altentry
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 78:27: -> ^( ALTERNATIVES altentry )
                    {
                        // SimilaritySpec.g:78:30: ^( ALTERNATIVES altentry )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(ALTERNATIVES, "ALTERNATIVES"), root_1);

                        adaptor.addChild(root_1, stream_altentry.nextTree());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "alternative"

    public static class altentry_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "altentry"
    // SimilaritySpec.g:80:1: altentry : ( annotations ( constituents )? -> ^( ENTRY annotations ( constituents )? ) | constituents ( annotations )? -> ^( ENTRY constituents ( annotations )? ) );
    public final SimilaritySpecParser.altentry_return altentry() throws RecognitionException {
        SimilaritySpecParser.altentry_return retval = new SimilaritySpecParser.altentry_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        SimilaritySpecParser.annotations_return annotations45 = null;

        SimilaritySpecParser.constituents_return constituents46 = null;

        SimilaritySpecParser.constituents_return constituents47 = null;

        SimilaritySpecParser.annotations_return annotations48 = null;


        RewriteRuleSubtreeStream stream_constituents=new RewriteRuleSubtreeStream(adaptor,"rule constituents");
        RewriteRuleSubtreeStream stream_annotations=new RewriteRuleSubtreeStream(adaptor,"rule annotations");
        try {
            // SimilaritySpec.g:81:2: ( annotations ( constituents )? -> ^( ENTRY annotations ( constituents )? ) | constituents ( annotations )? -> ^( ENTRY constituents ( annotations )? ) )
            int alt17=2;
            int LA17_0 = input.LA(1);

            if ( (LA17_0==ANNOTATIONS) ) {
                alt17=1;
            }
            else if ( (LA17_0==CONSTITUENTS) ) {
                alt17=2;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 17, 0, input);

                throw nvae;
            }
            switch (alt17) {
                case 1 :
                    // SimilaritySpec.g:81:5: annotations ( constituents )?
                    {
                    pushFollow(FOLLOW_annotations_in_altentry536);
                    annotations45=annotations();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_annotations.add(annotations45.getTree());
                    // SimilaritySpec.g:81:17: ( constituents )?
                    int alt15=2;
                    int LA15_0 = input.LA(1);

                    if ( (LA15_0==CONSTITUENTS) ) {
                        alt15=1;
                    }
                    switch (alt15) {
                        case 1 :
                            // SimilaritySpec.g:0:0: constituents
                            {
                            pushFollow(FOLLOW_constituents_in_altentry538);
                            constituents46=constituents();

                            state._fsp--;
                            if (state.failed) return retval;
                            if ( state.backtracking==0 ) stream_constituents.add(constituents46.getTree());

                            }
                            break;

                    }



                    // AST REWRITE
                    // elements: annotations, constituents
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 81:31: -> ^( ENTRY annotations ( constituents )? )
                    {
                        // SimilaritySpec.g:81:34: ^( ENTRY annotations ( constituents )? )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(ENTRY, "ENTRY"), root_1);

                        adaptor.addChild(root_1, stream_annotations.nextTree());
                        // SimilaritySpec.g:81:54: ( constituents )?
                        if ( stream_constituents.hasNext() ) {
                            adaptor.addChild(root_1, stream_constituents.nextTree());

                        }
                        stream_constituents.reset();

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 2 :
                    // SimilaritySpec.g:82:18: constituents ( annotations )?
                    {
                    pushFollow(FOLLOW_constituents_in_altentry569);
                    constituents47=constituents();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_constituents.add(constituents47.getTree());
                    // SimilaritySpec.g:82:31: ( annotations )?
                    int alt16=2;
                    int LA16_0 = input.LA(1);

                    if ( (LA16_0==ANNOTATIONS) ) {
                        alt16=1;
                    }
                    switch (alt16) {
                        case 1 :
                            // SimilaritySpec.g:0:0: annotations
                            {
                            pushFollow(FOLLOW_annotations_in_altentry571);
                            annotations48=annotations();

                            state._fsp--;
                            if (state.failed) return retval;
                            if ( state.backtracking==0 ) stream_annotations.add(annotations48.getTree());

                            }
                            break;

                    }



                    // AST REWRITE
                    // elements: constituents, annotations
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 82:44: -> ^( ENTRY constituents ( annotations )? )
                    {
                        // SimilaritySpec.g:82:47: ^( ENTRY constituents ( annotations )? )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(ENTRY, "ENTRY"), root_1);

                        adaptor.addChild(root_1, stream_constituents.nextTree());
                        // SimilaritySpec.g:82:68: ( annotations )?
                        if ( stream_annotations.hasNext() ) {
                            adaptor.addChild(root_1, stream_annotations.nextTree());

                        }
                        stream_annotations.reset();

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "altentry"

    public static class annotations_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "annotations"
    // SimilaritySpec.g:85:1: annotations : ( ANNOTATIONS '{' annoallbut ( ( ',' )? annoentry )* '}' | ANNOTATIONS '{' ( annoentry )? '}' );
    public final SimilaritySpecParser.annotations_return annotations() throws RecognitionException {
        SimilaritySpecParser.annotations_return retval = new SimilaritySpecParser.annotations_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token ANNOTATIONS49=null;
        Token char_literal50=null;
        Token char_literal52=null;
        Token char_literal54=null;
        Token ANNOTATIONS55=null;
        Token char_literal56=null;
        Token char_literal58=null;
        SimilaritySpecParser.annoallbut_return annoallbut51 = null;

        SimilaritySpecParser.annoentry_return annoentry53 = null;

        SimilaritySpecParser.annoentry_return annoentry57 = null;


        Object ANNOTATIONS49_tree=null;
        Object char_literal50_tree=null;
        Object char_literal52_tree=null;
        Object char_literal54_tree=null;
        Object ANNOTATIONS55_tree=null;
        Object char_literal56_tree=null;
        Object char_literal58_tree=null;

        try {
            // SimilaritySpec.g:86:2: ( ANNOTATIONS '{' annoallbut ( ( ',' )? annoentry )* '}' | ANNOTATIONS '{' ( annoentry )? '}' )
            int alt21=2;
            int LA21_0 = input.LA(1);

            if ( (LA21_0==ANNOTATIONS) ) {
                int LA21_1 = input.LA(2);

                if ( (LA21_1==41) ) {
                    int LA21_2 = input.LA(3);

                    if ( (LA21_2==45) ) {
                        alt21=1;
                    }
                    else if ( (LA21_2==ABSENT||LA21_2==NCNAME||LA21_2==42) ) {
                        alt21=2;
                    }
                    else {
                        if (state.backtracking>0) {state.failed=true; return retval;}
                        NoViableAltException nvae =
                            new NoViableAltException("", 21, 2, input);

                        throw nvae;
                    }
                }
                else {
                    if (state.backtracking>0) {state.failed=true; return retval;}
                    NoViableAltException nvae =
                        new NoViableAltException("", 21, 1, input);

                    throw nvae;
                }
            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 21, 0, input);

                throw nvae;
            }
            switch (alt21) {
                case 1 :
                    // SimilaritySpec.g:86:5: ANNOTATIONS '{' annoallbut ( ( ',' )? annoentry )* '}'
                    {
                    root_0 = (Object)adaptor.nil();

                    ANNOTATIONS49=(Token)match(input,ANNOTATIONS,FOLLOW_ANNOTATIONS_in_annotations595); if (state.failed) return retval;
                    if ( state.backtracking==0 ) {
                    ANNOTATIONS49_tree = (Object)adaptor.create(ANNOTATIONS49);
                    root_0 = (Object)adaptor.becomeRoot(ANNOTATIONS49_tree, root_0);
                    }
                    char_literal50=(Token)match(input,41,FOLLOW_41_in_annotations598); if (state.failed) return retval;
                    pushFollow(FOLLOW_annoallbut_in_annotations601);
                    annoallbut51=annoallbut();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, annoallbut51.getTree());
                    // SimilaritySpec.g:86:34: ( ( ',' )? annoentry )*
                    loop19:
                    do {
                        int alt19=2;
                        int LA19_0 = input.LA(1);

                        if ( (LA19_0==ABSENT||LA19_0==NCNAME||LA19_0==43) ) {
                            alt19=1;
                        }


                        switch (alt19) {
                    	case 1 :
                    	    // SimilaritySpec.g:86:35: ( ',' )? annoentry
                    	    {
                    	    // SimilaritySpec.g:86:35: ( ',' )?
                    	    int alt18=2;
                    	    int LA18_0 = input.LA(1);

                    	    if ( (LA18_0==43) ) {
                    	        alt18=1;
                    	    }
                    	    switch (alt18) {
                    	        case 1 :
                    	            // SimilaritySpec.g:86:36: ','
                    	            {
                    	            char_literal52=(Token)match(input,43,FOLLOW_43_in_annotations605); if (state.failed) return retval;

                    	            }
                    	            break;

                    	    }

                    	    pushFollow(FOLLOW_annoentry_in_annotations610);
                    	    annoentry53=annoentry();

                    	    state._fsp--;
                    	    if (state.failed) return retval;
                    	    if ( state.backtracking==0 ) adaptor.addChild(root_0, annoentry53.getTree());

                    	    }
                    	    break;

                    	default :
                    	    break loop19;
                        }
                    } while (true);

                    char_literal54=(Token)match(input,42,FOLLOW_42_in_annotations614); if (state.failed) return retval;

                    }
                    break;
                case 2 :
                    // SimilaritySpec.g:87:18: ANNOTATIONS '{' ( annoentry )? '}'
                    {
                    root_0 = (Object)adaptor.nil();

                    ANNOTATIONS55=(Token)match(input,ANNOTATIONS,FOLLOW_ANNOTATIONS_in_annotations634); if (state.failed) return retval;
                    if ( state.backtracking==0 ) {
                    ANNOTATIONS55_tree = (Object)adaptor.create(ANNOTATIONS55);
                    root_0 = (Object)adaptor.becomeRoot(ANNOTATIONS55_tree, root_0);
                    }
                    char_literal56=(Token)match(input,41,FOLLOW_41_in_annotations637); if (state.failed) return retval;
                    // SimilaritySpec.g:87:36: ( annoentry )?
                    int alt20=2;
                    int LA20_0 = input.LA(1);

                    if ( (LA20_0==ABSENT||LA20_0==NCNAME) ) {
                        alt20=1;
                    }
                    switch (alt20) {
                        case 1 :
                            // SimilaritySpec.g:0:0: annoentry
                            {
                            pushFollow(FOLLOW_annoentry_in_annotations640);
                            annoentry57=annoentry();

                            state._fsp--;
                            if (state.failed) return retval;
                            if ( state.backtracking==0 ) adaptor.addChild(root_0, annoentry57.getTree());

                            }
                            break;

                    }

                    char_literal58=(Token)match(input,42,FOLLOW_42_in_annotations643); if (state.failed) return retval;

                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "annotations"

    public static class annoallbut_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "annoallbut"
    // SimilaritySpec.g:89:1: annoallbut : '_' ( '\\\\' '{' qname ( ',' qname )* '}' )? -> ^( ALLBUT[\"allbut\"] ( qname )* ) ;
    public final SimilaritySpecParser.annoallbut_return annoallbut() throws RecognitionException {
        SimilaritySpecParser.annoallbut_return retval = new SimilaritySpecParser.annoallbut_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token char_literal59=null;
        Token char_literal60=null;
        Token char_literal61=null;
        Token char_literal63=null;
        Token char_literal65=null;
        SimilaritySpecParser.qname_return qname62 = null;

        SimilaritySpecParser.qname_return qname64 = null;


        Object char_literal59_tree=null;
        Object char_literal60_tree=null;
        Object char_literal61_tree=null;
        Object char_literal63_tree=null;
        Object char_literal65_tree=null;
        RewriteRuleTokenStream stream_45=new RewriteRuleTokenStream(adaptor,"token 45");
        RewriteRuleTokenStream stream_43=new RewriteRuleTokenStream(adaptor,"token 43");
        RewriteRuleTokenStream stream_42=new RewriteRuleTokenStream(adaptor,"token 42");
        RewriteRuleTokenStream stream_41=new RewriteRuleTokenStream(adaptor,"token 41");
        RewriteRuleTokenStream stream_46=new RewriteRuleTokenStream(adaptor,"token 46");
        RewriteRuleSubtreeStream stream_qname=new RewriteRuleSubtreeStream(adaptor,"rule qname");
        try {
            // SimilaritySpec.g:90:2: ( '_' ( '\\\\' '{' qname ( ',' qname )* '}' )? -> ^( ALLBUT[\"allbut\"] ( qname )* ) )
            // SimilaritySpec.g:90:5: '_' ( '\\\\' '{' qname ( ',' qname )* '}' )?
            {
            char_literal59=(Token)match(input,45,FOLLOW_45_in_annoallbut656); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_45.add(char_literal59);

            // SimilaritySpec.g:90:9: ( '\\\\' '{' qname ( ',' qname )* '}' )?
            int alt23=2;
            int LA23_0 = input.LA(1);

            if ( (LA23_0==46) ) {
                alt23=1;
            }
            switch (alt23) {
                case 1 :
                    // SimilaritySpec.g:90:11: '\\\\' '{' qname ( ',' qname )* '}'
                    {
                    char_literal60=(Token)match(input,46,FOLLOW_46_in_annoallbut660); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_46.add(char_literal60);

                    char_literal61=(Token)match(input,41,FOLLOW_41_in_annoallbut662); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_41.add(char_literal61);

                    pushFollow(FOLLOW_qname_in_annoallbut664);
                    qname62=qname();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_qname.add(qname62.getTree());
                    // SimilaritySpec.g:90:26: ( ',' qname )*
                    loop22:
                    do {
                        int alt22=2;
                        int LA22_0 = input.LA(1);

                        if ( (LA22_0==43) ) {
                            alt22=1;
                        }


                        switch (alt22) {
                    	case 1 :
                    	    // SimilaritySpec.g:90:27: ',' qname
                    	    {
                    	    char_literal63=(Token)match(input,43,FOLLOW_43_in_annoallbut667); if (state.failed) return retval; 
                    	    if ( state.backtracking==0 ) stream_43.add(char_literal63);

                    	    pushFollow(FOLLOW_qname_in_annoallbut669);
                    	    qname64=qname();

                    	    state._fsp--;
                    	    if (state.failed) return retval;
                    	    if ( state.backtracking==0 ) stream_qname.add(qname64.getTree());

                    	    }
                    	    break;

                    	default :
                    	    break loop22;
                        }
                    } while (true);

                    char_literal65=(Token)match(input,42,FOLLOW_42_in_annoallbut673); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_42.add(char_literal65);


                    }
                    break;

            }



            // AST REWRITE
            // elements: qname
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 90:46: -> ^( ALLBUT[\"allbut\"] ( qname )* )
            {
                // SimilaritySpec.g:90:49: ^( ALLBUT[\"allbut\"] ( qname )* )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(ALLBUT, "allbut"), root_1);

                // SimilaritySpec.g:90:68: ( qname )*
                while ( stream_qname.hasNext() ) {
                    adaptor.addChild(root_1, stream_qname.nextTree());

                }
                stream_qname.reset();

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "annoallbut"

    public static class annoentry_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "annoentry"
    // SimilaritySpec.g:92:1: annoentry : ( qnameoptorrequired ( ( ',' )? annoentry )* | annoabsent ( ( ',' )? annoentry )* );
    public final SimilaritySpecParser.annoentry_return annoentry() throws RecognitionException {
        SimilaritySpecParser.annoentry_return retval = new SimilaritySpecParser.annoentry_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token char_literal67=null;
        Token char_literal70=null;
        SimilaritySpecParser.qnameoptorrequired_return qnameoptorrequired66 = null;

        SimilaritySpecParser.annoentry_return annoentry68 = null;

        SimilaritySpecParser.annoabsent_return annoabsent69 = null;

        SimilaritySpecParser.annoentry_return annoentry71 = null;


        Object char_literal67_tree=null;
        Object char_literal70_tree=null;

        try {
            // SimilaritySpec.g:93:2: ( qnameoptorrequired ( ( ',' )? annoentry )* | annoabsent ( ( ',' )? annoentry )* )
            int alt28=2;
            int LA28_0 = input.LA(1);

            if ( (LA28_0==NCNAME) ) {
                alt28=1;
            }
            else if ( (LA28_0==ABSENT) ) {
                alt28=2;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 28, 0, input);

                throw nvae;
            }
            switch (alt28) {
                case 1 :
                    // SimilaritySpec.g:93:4: qnameoptorrequired ( ( ',' )? annoentry )*
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_qnameoptorrequired_in_annoentry702);
                    qnameoptorrequired66=qnameoptorrequired();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, qnameoptorrequired66.getTree());
                    // SimilaritySpec.g:93:23: ( ( ',' )? annoentry )*
                    loop25:
                    do {
                        int alt25=2;
                        switch ( input.LA(1) ) {
                        case 43:
                            {
                            int LA25_2 = input.LA(2);

                            if ( (synpred28_SimilaritySpec()) ) {
                                alt25=1;
                            }


                            }
                            break;
                        case NCNAME:
                            {
                            int LA25_3 = input.LA(2);

                            if ( (synpred28_SimilaritySpec()) ) {
                                alt25=1;
                            }


                            }
                            break;
                        case ABSENT:
                            {
                            int LA25_4 = input.LA(2);

                            if ( (synpred28_SimilaritySpec()) ) {
                                alt25=1;
                            }


                            }
                            break;

                        }

                        switch (alt25) {
                    	case 1 :
                    	    // SimilaritySpec.g:93:24: ( ',' )? annoentry
                    	    {
                    	    // SimilaritySpec.g:93:24: ( ',' )?
                    	    int alt24=2;
                    	    int LA24_0 = input.LA(1);

                    	    if ( (LA24_0==43) ) {
                    	        alt24=1;
                    	    }
                    	    switch (alt24) {
                    	        case 1 :
                    	            // SimilaritySpec.g:93:25: ','
                    	            {
                    	            char_literal67=(Token)match(input,43,FOLLOW_43_in_annoentry706); if (state.failed) return retval;

                    	            }
                    	            break;

                    	    }

                    	    pushFollow(FOLLOW_annoentry_in_annoentry711);
                    	    annoentry68=annoentry();

                    	    state._fsp--;
                    	    if (state.failed) return retval;
                    	    if ( state.backtracking==0 ) adaptor.addChild(root_0, annoentry68.getTree());

                    	    }
                    	    break;

                    	default :
                    	    break loop25;
                        }
                    } while (true);


                    }
                    break;
                case 2 :
                    // SimilaritySpec.g:94:5: annoabsent ( ( ',' )? annoentry )*
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_annoabsent_in_annoentry719);
                    annoabsent69=annoabsent();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, annoabsent69.getTree());
                    // SimilaritySpec.g:94:16: ( ( ',' )? annoentry )*
                    loop27:
                    do {
                        int alt27=2;
                        switch ( input.LA(1) ) {
                        case 43:
                            {
                            int LA27_2 = input.LA(2);

                            if ( (synpred31_SimilaritySpec()) ) {
                                alt27=1;
                            }


                            }
                            break;
                        case NCNAME:
                            {
                            int LA27_3 = input.LA(2);

                            if ( (synpred31_SimilaritySpec()) ) {
                                alt27=1;
                            }


                            }
                            break;
                        case ABSENT:
                            {
                            int LA27_4 = input.LA(2);

                            if ( (synpred31_SimilaritySpec()) ) {
                                alt27=1;
                            }


                            }
                            break;

                        }

                        switch (alt27) {
                    	case 1 :
                    	    // SimilaritySpec.g:94:17: ( ',' )? annoentry
                    	    {
                    	    // SimilaritySpec.g:94:17: ( ',' )?
                    	    int alt26=2;
                    	    int LA26_0 = input.LA(1);

                    	    if ( (LA26_0==43) ) {
                    	        alt26=1;
                    	    }
                    	    switch (alt26) {
                    	        case 1 :
                    	            // SimilaritySpec.g:94:18: ','
                    	            {
                    	            char_literal70=(Token)match(input,43,FOLLOW_43_in_annoentry723); if (state.failed) return retval;

                    	            }
                    	            break;

                    	    }

                    	    pushFollow(FOLLOW_annoentry_in_annoentry728);
                    	    annoentry71=annoentry();

                    	    state._fsp--;
                    	    if (state.failed) return retval;
                    	    if ( state.backtracking==0 ) adaptor.addChild(root_0, annoentry71.getTree());

                    	    }
                    	    break;

                    	default :
                    	    break loop27;
                        }
                    } while (true);


                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "annoentry"

    public static class annoabsent_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "annoabsent"
    // SimilaritySpec.g:96:1: annoabsent : ABSENT '{' qname ( ( ',' )? qname )* '}' -> ^( ABSENT ( qname )* ) ;
    public final SimilaritySpecParser.annoabsent_return annoabsent() throws RecognitionException {
        SimilaritySpecParser.annoabsent_return retval = new SimilaritySpecParser.annoabsent_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token ABSENT72=null;
        Token char_literal73=null;
        Token char_literal75=null;
        Token char_literal77=null;
        SimilaritySpecParser.qname_return qname74 = null;

        SimilaritySpecParser.qname_return qname76 = null;


        Object ABSENT72_tree=null;
        Object char_literal73_tree=null;
        Object char_literal75_tree=null;
        Object char_literal77_tree=null;
        RewriteRuleTokenStream stream_43=new RewriteRuleTokenStream(adaptor,"token 43");
        RewriteRuleTokenStream stream_42=new RewriteRuleTokenStream(adaptor,"token 42");
        RewriteRuleTokenStream stream_41=new RewriteRuleTokenStream(adaptor,"token 41");
        RewriteRuleTokenStream stream_ABSENT=new RewriteRuleTokenStream(adaptor,"token ABSENT");
        RewriteRuleSubtreeStream stream_qname=new RewriteRuleSubtreeStream(adaptor,"rule qname");
        try {
            // SimilaritySpec.g:97:2: ( ABSENT '{' qname ( ( ',' )? qname )* '}' -> ^( ABSENT ( qname )* ) )
            // SimilaritySpec.g:97:5: ABSENT '{' qname ( ( ',' )? qname )* '}'
            {
            ABSENT72=(Token)match(input,ABSENT,FOLLOW_ABSENT_in_annoabsent742); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_ABSENT.add(ABSENT72);

            char_literal73=(Token)match(input,41,FOLLOW_41_in_annoabsent744); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_41.add(char_literal73);

            pushFollow(FOLLOW_qname_in_annoabsent746);
            qname74=qname();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) stream_qname.add(qname74.getTree());
            // SimilaritySpec.g:97:22: ( ( ',' )? qname )*
            loop30:
            do {
                int alt30=2;
                int LA30_0 = input.LA(1);

                if ( (LA30_0==NCNAME||LA30_0==43) ) {
                    alt30=1;
                }


                switch (alt30) {
            	case 1 :
            	    // SimilaritySpec.g:97:24: ( ',' )? qname
            	    {
            	    // SimilaritySpec.g:97:24: ( ',' )?
            	    int alt29=2;
            	    int LA29_0 = input.LA(1);

            	    if ( (LA29_0==43) ) {
            	        alt29=1;
            	    }
            	    switch (alt29) {
            	        case 1 :
            	            // SimilaritySpec.g:97:25: ','
            	            {
            	            char_literal75=(Token)match(input,43,FOLLOW_43_in_annoabsent751); if (state.failed) return retval; 
            	            if ( state.backtracking==0 ) stream_43.add(char_literal75);


            	            }
            	            break;

            	    }

            	    pushFollow(FOLLOW_qname_in_annoabsent755);
            	    qname76=qname();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) stream_qname.add(qname76.getTree());

            	    }
            	    break;

            	default :
            	    break loop30;
                }
            } while (true);

            char_literal77=(Token)match(input,42,FOLLOW_42_in_annoabsent760); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_42.add(char_literal77);



            // AST REWRITE
            // elements: ABSENT, qname
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 97:44: -> ^( ABSENT ( qname )* )
            {
                // SimilaritySpec.g:97:47: ^( ABSENT ( qname )* )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot(stream_ABSENT.nextNode(), root_1);

                // SimilaritySpec.g:97:56: ( qname )*
                while ( stream_qname.hasNext() ) {
                    adaptor.addChild(root_1, stream_qname.nextTree());

                }
                stream_qname.reset();

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "annoabsent"

    public static class constituents_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "constituents"
    // SimilaritySpec.g:99:1: constituents : CONSTITUENTS '{' ( consentry )? '}' ;
    public final SimilaritySpecParser.constituents_return constituents() throws RecognitionException {
        SimilaritySpecParser.constituents_return retval = new SimilaritySpecParser.constituents_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token CONSTITUENTS78=null;
        Token char_literal79=null;
        Token char_literal81=null;
        SimilaritySpecParser.consentry_return consentry80 = null;


        Object CONSTITUENTS78_tree=null;
        Object char_literal79_tree=null;
        Object char_literal81_tree=null;

        try {
            // SimilaritySpec.g:100:2: ( CONSTITUENTS '{' ( consentry )? '}' )
            // SimilaritySpec.g:100:5: CONSTITUENTS '{' ( consentry )? '}'
            {
            root_0 = (Object)adaptor.nil();

            CONSTITUENTS78=(Token)match(input,CONSTITUENTS,FOLLOW_CONSTITUENTS_in_constituents780); if (state.failed) return retval;
            if ( state.backtracking==0 ) {
            CONSTITUENTS78_tree = (Object)adaptor.create(CONSTITUENTS78);
            root_0 = (Object)adaptor.becomeRoot(CONSTITUENTS78_tree, root_0);
            }
            char_literal79=(Token)match(input,41,FOLLOW_41_in_constituents783); if (state.failed) return retval;
            // SimilaritySpec.g:100:24: ( consentry )?
            int alt31=2;
            int LA31_0 = input.LA(1);

            if ( (LA31_0==MANDATORY||LA31_0==LIMIT||LA31_0==ABSENT||(LA31_0>=UNORDERED && LA31_0<=ORDERED)) ) {
                alt31=1;
            }
            switch (alt31) {
                case 1 :
                    // SimilaritySpec.g:0:0: consentry
                    {
                    pushFollow(FOLLOW_consentry_in_constituents786);
                    consentry80=consentry();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, consentry80.getTree());

                    }
                    break;

            }

            char_literal81=(Token)match(input,42,FOLLOW_42_in_constituents789); if (state.failed) return retval;

            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "constituents"

    public static class consentry_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "consentry"
    // SimilaritySpec.g:102:1: consentry : ( consordered ( ( ',' )? consentry )* | consabsent ( ( ',' )? consentry )* | consmandatory ( ( ',' )? consentry )* | loclmt ( ( ',' )? consentry )* );
    public final SimilaritySpecParser.consentry_return consentry() throws RecognitionException {
        SimilaritySpecParser.consentry_return retval = new SimilaritySpecParser.consentry_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token char_literal83=null;
        Token char_literal86=null;
        Token char_literal89=null;
        Token char_literal92=null;
        SimilaritySpecParser.consordered_return consordered82 = null;

        SimilaritySpecParser.consentry_return consentry84 = null;

        SimilaritySpecParser.consabsent_return consabsent85 = null;

        SimilaritySpecParser.consentry_return consentry87 = null;

        SimilaritySpecParser.consmandatory_return consmandatory88 = null;

        SimilaritySpecParser.consentry_return consentry90 = null;

        SimilaritySpecParser.loclmt_return loclmt91 = null;

        SimilaritySpecParser.consentry_return consentry93 = null;


        Object char_literal83_tree=null;
        Object char_literal86_tree=null;
        Object char_literal89_tree=null;
        Object char_literal92_tree=null;

        try {
            // SimilaritySpec.g:103:2: ( consordered ( ( ',' )? consentry )* | consabsent ( ( ',' )? consentry )* | consmandatory ( ( ',' )? consentry )* | loclmt ( ( ',' )? consentry )* )
            int alt40=4;
            switch ( input.LA(1) ) {
            case UNORDERED:
            case ORDERED:
                {
                alt40=1;
                }
                break;
            case ABSENT:
                {
                alt40=2;
                }
                break;
            case MANDATORY:
                {
                alt40=3;
                }
                break;
            case LIMIT:
                {
                alt40=4;
                }
                break;
            default:
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 40, 0, input);

                throw nvae;
            }

            switch (alt40) {
                case 1 :
                    // SimilaritySpec.g:103:5: consordered ( ( ',' )? consentry )*
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_consordered_in_consentry803);
                    consordered82=consordered();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, consordered82.getTree());
                    // SimilaritySpec.g:103:17: ( ( ',' )? consentry )*
                    loop33:
                    do {
                        int alt33=2;
                        switch ( input.LA(1) ) {
                        case 43:
                            {
                            int LA33_2 = input.LA(2);

                            if ( (synpred36_SimilaritySpec()) ) {
                                alt33=1;
                            }


                            }
                            break;
                        case UNORDERED:
                            {
                            int LA33_3 = input.LA(2);

                            if ( (synpred36_SimilaritySpec()) ) {
                                alt33=1;
                            }


                            }
                            break;
                        case ORDERED:
                            {
                            int LA33_4 = input.LA(2);

                            if ( (synpred36_SimilaritySpec()) ) {
                                alt33=1;
                            }


                            }
                            break;
                        case ABSENT:
                            {
                            int LA33_5 = input.LA(2);

                            if ( (synpred36_SimilaritySpec()) ) {
                                alt33=1;
                            }


                            }
                            break;
                        case MANDATORY:
                            {
                            int LA33_6 = input.LA(2);

                            if ( (synpred36_SimilaritySpec()) ) {
                                alt33=1;
                            }


                            }
                            break;
                        case LIMIT:
                            {
                            int LA33_7 = input.LA(2);

                            if ( (synpred36_SimilaritySpec()) ) {
                                alt33=1;
                            }


                            }
                            break;

                        }

                        switch (alt33) {
                    	case 1 :
                    	    // SimilaritySpec.g:103:18: ( ',' )? consentry
                    	    {
                    	    // SimilaritySpec.g:103:18: ( ',' )?
                    	    int alt32=2;
                    	    int LA32_0 = input.LA(1);

                    	    if ( (LA32_0==43) ) {
                    	        alt32=1;
                    	    }
                    	    switch (alt32) {
                    	        case 1 :
                    	            // SimilaritySpec.g:103:19: ','
                    	            {
                    	            char_literal83=(Token)match(input,43,FOLLOW_43_in_consentry807); if (state.failed) return retval;

                    	            }
                    	            break;

                    	    }

                    	    pushFollow(FOLLOW_consentry_in_consentry812);
                    	    consentry84=consentry();

                    	    state._fsp--;
                    	    if (state.failed) return retval;
                    	    if ( state.backtracking==0 ) adaptor.addChild(root_0, consentry84.getTree());

                    	    }
                    	    break;

                    	default :
                    	    break loop33;
                        }
                    } while (true);


                    }
                    break;
                case 2 :
                    // SimilaritySpec.g:104:18: consabsent ( ( ',' )? consentry )*
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_consabsent_in_consentry834);
                    consabsent85=consabsent();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, consabsent85.getTree());
                    // SimilaritySpec.g:104:29: ( ( ',' )? consentry )*
                    loop35:
                    do {
                        int alt35=2;
                        switch ( input.LA(1) ) {
                        case 43:
                            {
                            int LA35_2 = input.LA(2);

                            if ( (synpred39_SimilaritySpec()) ) {
                                alt35=1;
                            }


                            }
                            break;
                        case UNORDERED:
                            {
                            int LA35_3 = input.LA(2);

                            if ( (synpred39_SimilaritySpec()) ) {
                                alt35=1;
                            }


                            }
                            break;
                        case ORDERED:
                            {
                            int LA35_4 = input.LA(2);

                            if ( (synpred39_SimilaritySpec()) ) {
                                alt35=1;
                            }


                            }
                            break;
                        case ABSENT:
                            {
                            int LA35_5 = input.LA(2);

                            if ( (synpred39_SimilaritySpec()) ) {
                                alt35=1;
                            }


                            }
                            break;
                        case MANDATORY:
                            {
                            int LA35_6 = input.LA(2);

                            if ( (synpred39_SimilaritySpec()) ) {
                                alt35=1;
                            }


                            }
                            break;
                        case LIMIT:
                            {
                            int LA35_7 = input.LA(2);

                            if ( (synpred39_SimilaritySpec()) ) {
                                alt35=1;
                            }


                            }
                            break;

                        }

                        switch (alt35) {
                    	case 1 :
                    	    // SimilaritySpec.g:104:30: ( ',' )? consentry
                    	    {
                    	    // SimilaritySpec.g:104:30: ( ',' )?
                    	    int alt34=2;
                    	    int LA34_0 = input.LA(1);

                    	    if ( (LA34_0==43) ) {
                    	        alt34=1;
                    	    }
                    	    switch (alt34) {
                    	        case 1 :
                    	            // SimilaritySpec.g:104:31: ','
                    	            {
                    	            char_literal86=(Token)match(input,43,FOLLOW_43_in_consentry838); if (state.failed) return retval;

                    	            }
                    	            break;

                    	    }

                    	    pushFollow(FOLLOW_consentry_in_consentry843);
                    	    consentry87=consentry();

                    	    state._fsp--;
                    	    if (state.failed) return retval;
                    	    if ( state.backtracking==0 ) adaptor.addChild(root_0, consentry87.getTree());

                    	    }
                    	    break;

                    	default :
                    	    break loop35;
                        }
                    } while (true);


                    }
                    break;
                case 3 :
                    // SimilaritySpec.g:105:18: consmandatory ( ( ',' )? consentry )*
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_consmandatory_in_consentry864);
                    consmandatory88=consmandatory();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, consmandatory88.getTree());
                    // SimilaritySpec.g:105:32: ( ( ',' )? consentry )*
                    loop37:
                    do {
                        int alt37=2;
                        switch ( input.LA(1) ) {
                        case 43:
                            {
                            int LA37_2 = input.LA(2);

                            if ( (synpred42_SimilaritySpec()) ) {
                                alt37=1;
                            }


                            }
                            break;
                        case UNORDERED:
                            {
                            int LA37_3 = input.LA(2);

                            if ( (synpred42_SimilaritySpec()) ) {
                                alt37=1;
                            }


                            }
                            break;
                        case ORDERED:
                            {
                            int LA37_4 = input.LA(2);

                            if ( (synpred42_SimilaritySpec()) ) {
                                alt37=1;
                            }


                            }
                            break;
                        case ABSENT:
                            {
                            int LA37_5 = input.LA(2);

                            if ( (synpred42_SimilaritySpec()) ) {
                                alt37=1;
                            }


                            }
                            break;
                        case MANDATORY:
                            {
                            int LA37_6 = input.LA(2);

                            if ( (synpred42_SimilaritySpec()) ) {
                                alt37=1;
                            }


                            }
                            break;
                        case LIMIT:
                            {
                            int LA37_7 = input.LA(2);

                            if ( (synpred42_SimilaritySpec()) ) {
                                alt37=1;
                            }


                            }
                            break;

                        }

                        switch (alt37) {
                    	case 1 :
                    	    // SimilaritySpec.g:105:33: ( ',' )? consentry
                    	    {
                    	    // SimilaritySpec.g:105:33: ( ',' )?
                    	    int alt36=2;
                    	    int LA36_0 = input.LA(1);

                    	    if ( (LA36_0==43) ) {
                    	        alt36=1;
                    	    }
                    	    switch (alt36) {
                    	        case 1 :
                    	            // SimilaritySpec.g:105:34: ','
                    	            {
                    	            char_literal89=(Token)match(input,43,FOLLOW_43_in_consentry868); if (state.failed) return retval;

                    	            }
                    	            break;

                    	    }

                    	    pushFollow(FOLLOW_consentry_in_consentry873);
                    	    consentry90=consentry();

                    	    state._fsp--;
                    	    if (state.failed) return retval;
                    	    if ( state.backtracking==0 ) adaptor.addChild(root_0, consentry90.getTree());

                    	    }
                    	    break;

                    	default :
                    	    break loop37;
                        }
                    } while (true);


                    }
                    break;
                case 4 :
                    // SimilaritySpec.g:106:18: loclmt ( ( ',' )? consentry )*
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_loclmt_in_consentry894);
                    loclmt91=loclmt();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, loclmt91.getTree());
                    // SimilaritySpec.g:106:25: ( ( ',' )? consentry )*
                    loop39:
                    do {
                        int alt39=2;
                        switch ( input.LA(1) ) {
                        case 43:
                            {
                            int LA39_2 = input.LA(2);

                            if ( (synpred45_SimilaritySpec()) ) {
                                alt39=1;
                            }


                            }
                            break;
                        case UNORDERED:
                            {
                            int LA39_3 = input.LA(2);

                            if ( (synpred45_SimilaritySpec()) ) {
                                alt39=1;
                            }


                            }
                            break;
                        case ORDERED:
                            {
                            int LA39_4 = input.LA(2);

                            if ( (synpred45_SimilaritySpec()) ) {
                                alt39=1;
                            }


                            }
                            break;
                        case ABSENT:
                            {
                            int LA39_5 = input.LA(2);

                            if ( (synpred45_SimilaritySpec()) ) {
                                alt39=1;
                            }


                            }
                            break;
                        case MANDATORY:
                            {
                            int LA39_6 = input.LA(2);

                            if ( (synpred45_SimilaritySpec()) ) {
                                alt39=1;
                            }


                            }
                            break;
                        case LIMIT:
                            {
                            int LA39_7 = input.LA(2);

                            if ( (synpred45_SimilaritySpec()) ) {
                                alt39=1;
                            }


                            }
                            break;

                        }

                        switch (alt39) {
                    	case 1 :
                    	    // SimilaritySpec.g:106:26: ( ',' )? consentry
                    	    {
                    	    // SimilaritySpec.g:106:26: ( ',' )?
                    	    int alt38=2;
                    	    int LA38_0 = input.LA(1);

                    	    if ( (LA38_0==43) ) {
                    	        alt38=1;
                    	    }
                    	    switch (alt38) {
                    	        case 1 :
                    	            // SimilaritySpec.g:106:27: ','
                    	            {
                    	            char_literal92=(Token)match(input,43,FOLLOW_43_in_consentry898); if (state.failed) return retval;

                    	            }
                    	            break;

                    	    }

                    	    pushFollow(FOLLOW_consentry_in_consentry903);
                    	    consentry93=consentry();

                    	    state._fsp--;
                    	    if (state.failed) return retval;
                    	    if ( state.backtracking==0 ) adaptor.addChild(root_0, consentry93.getTree());

                    	    }
                    	    break;

                    	default :
                    	    break loop39;
                        }
                    } while (true);


                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "consentry"

    public static class consordered_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "consordered"
    // SimilaritySpec.g:109:1: consordered : ( UNORDERED | ORDERED ) conscoll ;
    public final SimilaritySpecParser.consordered_return consordered() throws RecognitionException {
        SimilaritySpecParser.consordered_return retval = new SimilaritySpecParser.consordered_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token UNORDERED94=null;
        Token ORDERED95=null;
        SimilaritySpecParser.conscoll_return conscoll96 = null;


        Object UNORDERED94_tree=null;
        Object ORDERED95_tree=null;

        try {
            // SimilaritySpec.g:110:2: ( ( UNORDERED | ORDERED ) conscoll )
            // SimilaritySpec.g:110:4: ( UNORDERED | ORDERED ) conscoll
            {
            root_0 = (Object)adaptor.nil();

            // SimilaritySpec.g:110:4: ( UNORDERED | ORDERED )
            int alt41=2;
            int LA41_0 = input.LA(1);

            if ( (LA41_0==UNORDERED) ) {
                alt41=1;
            }
            else if ( (LA41_0==ORDERED) ) {
                alt41=2;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 41, 0, input);

                throw nvae;
            }
            switch (alt41) {
                case 1 :
                    // SimilaritySpec.g:110:6: UNORDERED
                    {
                    UNORDERED94=(Token)match(input,UNORDERED,FOLLOW_UNORDERED_in_consordered933); if (state.failed) return retval;
                    if ( state.backtracking==0 ) {
                    UNORDERED94_tree = (Object)adaptor.create(UNORDERED94);
                    root_0 = (Object)adaptor.becomeRoot(UNORDERED94_tree, root_0);
                    }

                    }
                    break;
                case 2 :
                    // SimilaritySpec.g:110:19: ORDERED
                    {
                    ORDERED95=(Token)match(input,ORDERED,FOLLOW_ORDERED_in_consordered938); if (state.failed) return retval;
                    if ( state.backtracking==0 ) {
                    ORDERED95_tree = (Object)adaptor.create(ORDERED95);
                    root_0 = (Object)adaptor.becomeRoot(ORDERED95_tree, root_0);
                    }

                    }
                    break;

            }

            pushFollow(FOLLOW_conscoll_in_consordered943);
            conscoll96=conscoll();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) adaptor.addChild(root_0, conscoll96.getTree());

            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "consordered"

    public static class consabsent_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "consabsent"
    // SimilaritySpec.g:113:1: consabsent : ABSENT '{' simpleelem ( ( ',' )? simpleelem )* '}' -> ^( ABSENT ( simpleelem )* ) ;
    public final SimilaritySpecParser.consabsent_return consabsent() throws RecognitionException {
        SimilaritySpecParser.consabsent_return retval = new SimilaritySpecParser.consabsent_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token ABSENT97=null;
        Token char_literal98=null;
        Token char_literal100=null;
        Token char_literal102=null;
        SimilaritySpecParser.simpleelem_return simpleelem99 = null;

        SimilaritySpecParser.simpleelem_return simpleelem101 = null;


        Object ABSENT97_tree=null;
        Object char_literal98_tree=null;
        Object char_literal100_tree=null;
        Object char_literal102_tree=null;
        RewriteRuleTokenStream stream_43=new RewriteRuleTokenStream(adaptor,"token 43");
        RewriteRuleTokenStream stream_42=new RewriteRuleTokenStream(adaptor,"token 42");
        RewriteRuleTokenStream stream_41=new RewriteRuleTokenStream(adaptor,"token 41");
        RewriteRuleTokenStream stream_ABSENT=new RewriteRuleTokenStream(adaptor,"token ABSENT");
        RewriteRuleSubtreeStream stream_simpleelem=new RewriteRuleSubtreeStream(adaptor,"rule simpleelem");
        try {
            // SimilaritySpec.g:114:2: ( ABSENT '{' simpleelem ( ( ',' )? simpleelem )* '}' -> ^( ABSENT ( simpleelem )* ) )
            // SimilaritySpec.g:114:5: ABSENT '{' simpleelem ( ( ',' )? simpleelem )* '}'
            {
            ABSENT97=(Token)match(input,ABSENT,FOLLOW_ABSENT_in_consabsent959); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_ABSENT.add(ABSENT97);

            char_literal98=(Token)match(input,41,FOLLOW_41_in_consabsent961); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_41.add(char_literal98);

            pushFollow(FOLLOW_simpleelem_in_consabsent963);
            simpleelem99=simpleelem();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) stream_simpleelem.add(simpleelem99.getTree());
            // SimilaritySpec.g:114:27: ( ( ',' )? simpleelem )*
            loop43:
            do {
                int alt43=2;
                int LA43_0 = input.LA(1);

                if ( ((LA43_0>=TEXTNODE && LA43_0<=SPLITNODESTART)||LA43_0==COMMENTNODE||LA43_0==NCNAME||LA43_0==43) ) {
                    alt43=1;
                }


                switch (alt43) {
            	case 1 :
            	    // SimilaritySpec.g:114:29: ( ',' )? simpleelem
            	    {
            	    // SimilaritySpec.g:114:29: ( ',' )?
            	    int alt42=2;
            	    int LA42_0 = input.LA(1);

            	    if ( (LA42_0==43) ) {
            	        alt42=1;
            	    }
            	    switch (alt42) {
            	        case 1 :
            	            // SimilaritySpec.g:114:30: ','
            	            {
            	            char_literal100=(Token)match(input,43,FOLLOW_43_in_consabsent968); if (state.failed) return retval; 
            	            if ( state.backtracking==0 ) stream_43.add(char_literal100);


            	            }
            	            break;

            	    }

            	    pushFollow(FOLLOW_simpleelem_in_consabsent972);
            	    simpleelem101=simpleelem();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) stream_simpleelem.add(simpleelem101.getTree());

            	    }
            	    break;

            	default :
            	    break loop43;
                }
            } while (true);

            char_literal102=(Token)match(input,42,FOLLOW_42_in_consabsent977); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_42.add(char_literal102);



            // AST REWRITE
            // elements: ABSENT, simpleelem
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 114:54: -> ^( ABSENT ( simpleelem )* )
            {
                // SimilaritySpec.g:114:57: ^( ABSENT ( simpleelem )* )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot(stream_ABSENT.nextNode(), root_1);

                // SimilaritySpec.g:114:66: ( simpleelem )*
                while ( stream_simpleelem.hasNext() ) {
                    adaptor.addChild(root_1, stream_simpleelem.nextTree());

                }
                stream_simpleelem.reset();

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "consabsent"

    public static class consmandatory_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "consmandatory"
    // SimilaritySpec.g:116:1: consmandatory : MANDATORY '{' simpleelem ( ( ',' )? simpleelem )* '}' -> ^( MANDATORY ( simpleelem )* ) ;
    public final SimilaritySpecParser.consmandatory_return consmandatory() throws RecognitionException {
        SimilaritySpecParser.consmandatory_return retval = new SimilaritySpecParser.consmandatory_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token MANDATORY103=null;
        Token char_literal104=null;
        Token char_literal106=null;
        Token char_literal108=null;
        SimilaritySpecParser.simpleelem_return simpleelem105 = null;

        SimilaritySpecParser.simpleelem_return simpleelem107 = null;


        Object MANDATORY103_tree=null;
        Object char_literal104_tree=null;
        Object char_literal106_tree=null;
        Object char_literal108_tree=null;
        RewriteRuleTokenStream stream_43=new RewriteRuleTokenStream(adaptor,"token 43");
        RewriteRuleTokenStream stream_42=new RewriteRuleTokenStream(adaptor,"token 42");
        RewriteRuleTokenStream stream_41=new RewriteRuleTokenStream(adaptor,"token 41");
        RewriteRuleTokenStream stream_MANDATORY=new RewriteRuleTokenStream(adaptor,"token MANDATORY");
        RewriteRuleSubtreeStream stream_simpleelem=new RewriteRuleSubtreeStream(adaptor,"rule simpleelem");
        try {
            // SimilaritySpec.g:117:2: ( MANDATORY '{' simpleelem ( ( ',' )? simpleelem )* '}' -> ^( MANDATORY ( simpleelem )* ) )
            // SimilaritySpec.g:117:5: MANDATORY '{' simpleelem ( ( ',' )? simpleelem )* '}'
            {
            MANDATORY103=(Token)match(input,MANDATORY,FOLLOW_MANDATORY_in_consmandatory998); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_MANDATORY.add(MANDATORY103);

            char_literal104=(Token)match(input,41,FOLLOW_41_in_consmandatory1000); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_41.add(char_literal104);

            pushFollow(FOLLOW_simpleelem_in_consmandatory1002);
            simpleelem105=simpleelem();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) stream_simpleelem.add(simpleelem105.getTree());
            // SimilaritySpec.g:117:30: ( ( ',' )? simpleelem )*
            loop45:
            do {
                int alt45=2;
                int LA45_0 = input.LA(1);

                if ( ((LA45_0>=TEXTNODE && LA45_0<=SPLITNODESTART)||LA45_0==COMMENTNODE||LA45_0==NCNAME||LA45_0==43) ) {
                    alt45=1;
                }


                switch (alt45) {
            	case 1 :
            	    // SimilaritySpec.g:117:32: ( ',' )? simpleelem
            	    {
            	    // SimilaritySpec.g:117:32: ( ',' )?
            	    int alt44=2;
            	    int LA44_0 = input.LA(1);

            	    if ( (LA44_0==43) ) {
            	        alt44=1;
            	    }
            	    switch (alt44) {
            	        case 1 :
            	            // SimilaritySpec.g:117:33: ','
            	            {
            	            char_literal106=(Token)match(input,43,FOLLOW_43_in_consmandatory1007); if (state.failed) return retval; 
            	            if ( state.backtracking==0 ) stream_43.add(char_literal106);


            	            }
            	            break;

            	    }

            	    pushFollow(FOLLOW_simpleelem_in_consmandatory1011);
            	    simpleelem107=simpleelem();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) stream_simpleelem.add(simpleelem107.getTree());

            	    }
            	    break;

            	default :
            	    break loop45;
                }
            } while (true);

            char_literal108=(Token)match(input,42,FOLLOW_42_in_consmandatory1016); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_42.add(char_literal108);



            // AST REWRITE
            // elements: simpleelem, MANDATORY
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 117:57: -> ^( MANDATORY ( simpleelem )* )
            {
                // SimilaritySpec.g:117:60: ^( MANDATORY ( simpleelem )* )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot(stream_MANDATORY.nextNode(), root_1);

                // SimilaritySpec.g:117:72: ( simpleelem )*
                while ( stream_simpleelem.hasNext() ) {
                    adaptor.addChild(root_1, stream_simpleelem.nextTree());

                }
                stream_simpleelem.reset();

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "consmandatory"

    public static class conscoll_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "conscoll"
    // SimilaritySpec.g:119:1: conscoll : ( '{' ( conseqelem | conselemtuple ) ( ( ',' )? ( conseqelem | conselemtuple ) )* '}' | '{' consallbut '}' );
    public final SimilaritySpecParser.conscoll_return conscoll() throws RecognitionException {
        SimilaritySpecParser.conscoll_return retval = new SimilaritySpecParser.conscoll_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token char_literal109=null;
        Token char_literal112=null;
        Token char_literal115=null;
        Token char_literal116=null;
        Token char_literal118=null;
        SimilaritySpecParser.conseqelem_return conseqelem110 = null;

        SimilaritySpecParser.conselemtuple_return conselemtuple111 = null;

        SimilaritySpecParser.conseqelem_return conseqelem113 = null;

        SimilaritySpecParser.conselemtuple_return conselemtuple114 = null;

        SimilaritySpecParser.consallbut_return consallbut117 = null;


        Object char_literal109_tree=null;
        Object char_literal112_tree=null;
        Object char_literal115_tree=null;
        Object char_literal116_tree=null;
        Object char_literal118_tree=null;

        try {
            // SimilaritySpec.g:120:2: ( '{' ( conseqelem | conselemtuple ) ( ( ',' )? ( conseqelem | conselemtuple ) )* '}' | '{' consallbut '}' )
            int alt50=2;
            int LA50_0 = input.LA(1);

            if ( (LA50_0==41) ) {
                int LA50_1 = input.LA(2);

                if ( ((LA50_1>=TEXTNODE && LA50_1<=SPLITNODESTART)||LA50_1==COMMENTNODE||LA50_1==NCNAME||LA50_1==47) ) {
                    alt50=1;
                }
                else if ( (LA50_1==45) ) {
                    alt50=2;
                }
                else {
                    if (state.backtracking>0) {state.failed=true; return retval;}
                    NoViableAltException nvae =
                        new NoViableAltException("", 50, 1, input);

                    throw nvae;
                }
            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 50, 0, input);

                throw nvae;
            }
            switch (alt50) {
                case 1 :
                    // SimilaritySpec.g:120:4: '{' ( conseqelem | conselemtuple ) ( ( ',' )? ( conseqelem | conselemtuple ) )* '}'
                    {
                    root_0 = (Object)adaptor.nil();

                    char_literal109=(Token)match(input,41,FOLLOW_41_in_conscoll1038); if (state.failed) return retval;
                    // SimilaritySpec.g:120:9: ( conseqelem | conselemtuple )
                    int alt46=2;
                    int LA46_0 = input.LA(1);

                    if ( ((LA46_0>=TEXTNODE && LA46_0<=SPLITNODESTART)||LA46_0==COMMENTNODE||LA46_0==NCNAME) ) {
                        alt46=1;
                    }
                    else if ( (LA46_0==47) ) {
                        alt46=2;
                    }
                    else {
                        if (state.backtracking>0) {state.failed=true; return retval;}
                        NoViableAltException nvae =
                            new NoViableAltException("", 46, 0, input);

                        throw nvae;
                    }
                    switch (alt46) {
                        case 1 :
                            // SimilaritySpec.g:120:10: conseqelem
                            {
                            pushFollow(FOLLOW_conseqelem_in_conscoll1042);
                            conseqelem110=conseqelem();

                            state._fsp--;
                            if (state.failed) return retval;
                            if ( state.backtracking==0 ) adaptor.addChild(root_0, conseqelem110.getTree());

                            }
                            break;
                        case 2 :
                            // SimilaritySpec.g:120:23: conselemtuple
                            {
                            pushFollow(FOLLOW_conselemtuple_in_conscoll1046);
                            conselemtuple111=conselemtuple();

                            state._fsp--;
                            if (state.failed) return retval;
                            if ( state.backtracking==0 ) adaptor.addChild(root_0, conselemtuple111.getTree());

                            }
                            break;

                    }

                    // SimilaritySpec.g:120:38: ( ( ',' )? ( conseqelem | conselemtuple ) )*
                    loop49:
                    do {
                        int alt49=2;
                        int LA49_0 = input.LA(1);

                        if ( ((LA49_0>=TEXTNODE && LA49_0<=SPLITNODESTART)||LA49_0==COMMENTNODE||LA49_0==NCNAME||LA49_0==43||LA49_0==47) ) {
                            alt49=1;
                        }


                        switch (alt49) {
                    	case 1 :
                    	    // SimilaritySpec.g:120:40: ( ',' )? ( conseqelem | conselemtuple )
                    	    {
                    	    // SimilaritySpec.g:120:40: ( ',' )?
                    	    int alt47=2;
                    	    int LA47_0 = input.LA(1);

                    	    if ( (LA47_0==43) ) {
                    	        alt47=1;
                    	    }
                    	    switch (alt47) {
                    	        case 1 :
                    	            // SimilaritySpec.g:120:41: ','
                    	            {
                    	            char_literal112=(Token)match(input,43,FOLLOW_43_in_conscoll1052); if (state.failed) return retval;

                    	            }
                    	            break;

                    	    }

                    	    // SimilaritySpec.g:120:48: ( conseqelem | conselemtuple )
                    	    int alt48=2;
                    	    int LA48_0 = input.LA(1);

                    	    if ( ((LA48_0>=TEXTNODE && LA48_0<=SPLITNODESTART)||LA48_0==COMMENTNODE||LA48_0==NCNAME) ) {
                    	        alt48=1;
                    	    }
                    	    else if ( (LA48_0==47) ) {
                    	        alt48=2;
                    	    }
                    	    else {
                    	        if (state.backtracking>0) {state.failed=true; return retval;}
                    	        NoViableAltException nvae =
                    	            new NoViableAltException("", 48, 0, input);

                    	        throw nvae;
                    	    }
                    	    switch (alt48) {
                    	        case 1 :
                    	            // SimilaritySpec.g:120:49: conseqelem
                    	            {
                    	            pushFollow(FOLLOW_conseqelem_in_conscoll1058);
                    	            conseqelem113=conseqelem();

                    	            state._fsp--;
                    	            if (state.failed) return retval;
                    	            if ( state.backtracking==0 ) adaptor.addChild(root_0, conseqelem113.getTree());

                    	            }
                    	            break;
                    	        case 2 :
                    	            // SimilaritySpec.g:120:62: conselemtuple
                    	            {
                    	            pushFollow(FOLLOW_conselemtuple_in_conscoll1062);
                    	            conselemtuple114=conselemtuple();

                    	            state._fsp--;
                    	            if (state.failed) return retval;
                    	            if ( state.backtracking==0 ) adaptor.addChild(root_0, conselemtuple114.getTree());

                    	            }
                    	            break;

                    	    }


                    	    }
                    	    break;

                    	default :
                    	    break loop49;
                        }
                    } while (true);

                    char_literal115=(Token)match(input,42,FOLLOW_42_in_conscoll1068); if (state.failed) return retval;

                    }
                    break;
                case 2 :
                    // SimilaritySpec.g:121:5: '{' consallbut '}'
                    {
                    root_0 = (Object)adaptor.nil();

                    char_literal116=(Token)match(input,41,FOLLOW_41_in_conscoll1075); if (state.failed) return retval;
                    pushFollow(FOLLOW_consallbut_in_conscoll1078);
                    consallbut117=consallbut();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, consallbut117.getTree());
                    char_literal118=(Token)match(input,42,FOLLOW_42_in_conscoll1080); if (state.failed) return retval;

                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "conscoll"

    public static class conselemtuple_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "conselemtuple"
    // SimilaritySpec.g:124:1: conselemtuple : '(' conseqelem ( ',' conseqelem )* ')' -> ^( TUPLE ( conseqelem )+ ) ;
    public final SimilaritySpecParser.conselemtuple_return conselemtuple() throws RecognitionException {
        SimilaritySpecParser.conselemtuple_return retval = new SimilaritySpecParser.conselemtuple_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token char_literal119=null;
        Token char_literal121=null;
        Token char_literal123=null;
        SimilaritySpecParser.conseqelem_return conseqelem120 = null;

        SimilaritySpecParser.conseqelem_return conseqelem122 = null;


        Object char_literal119_tree=null;
        Object char_literal121_tree=null;
        Object char_literal123_tree=null;
        RewriteRuleTokenStream stream_48=new RewriteRuleTokenStream(adaptor,"token 48");
        RewriteRuleTokenStream stream_43=new RewriteRuleTokenStream(adaptor,"token 43");
        RewriteRuleTokenStream stream_47=new RewriteRuleTokenStream(adaptor,"token 47");
        RewriteRuleSubtreeStream stream_conseqelem=new RewriteRuleSubtreeStream(adaptor,"rule conseqelem");
        try {
            // SimilaritySpec.g:125:2: ( '(' conseqelem ( ',' conseqelem )* ')' -> ^( TUPLE ( conseqelem )+ ) )
            // SimilaritySpec.g:125:4: '(' conseqelem ( ',' conseqelem )* ')'
            {
            char_literal119=(Token)match(input,47,FOLLOW_47_in_conselemtuple1092); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_47.add(char_literal119);

            pushFollow(FOLLOW_conseqelem_in_conselemtuple1094);
            conseqelem120=conseqelem();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) stream_conseqelem.add(conseqelem120.getTree());
            // SimilaritySpec.g:125:19: ( ',' conseqelem )*
            loop51:
            do {
                int alt51=2;
                int LA51_0 = input.LA(1);

                if ( (LA51_0==43) ) {
                    alt51=1;
                }


                switch (alt51) {
            	case 1 :
            	    // SimilaritySpec.g:125:20: ',' conseqelem
            	    {
            	    char_literal121=(Token)match(input,43,FOLLOW_43_in_conselemtuple1097); if (state.failed) return retval; 
            	    if ( state.backtracking==0 ) stream_43.add(char_literal121);

            	    pushFollow(FOLLOW_conseqelem_in_conselemtuple1099);
            	    conseqelem122=conseqelem();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) stream_conseqelem.add(conseqelem122.getTree());

            	    }
            	    break;

            	default :
            	    break loop51;
                }
            } while (true);

            char_literal123=(Token)match(input,48,FOLLOW_48_in_conselemtuple1103); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_48.add(char_literal123);



            // AST REWRITE
            // elements: conseqelem
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 125:41: -> ^( TUPLE ( conseqelem )+ )
            {
                // SimilaritySpec.g:125:44: ^( TUPLE ( conseqelem )+ )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(TUPLE, "TUPLE"), root_1);

                if ( !(stream_conseqelem.hasNext()) ) {
                    throw new RewriteEarlyExitException();
                }
                while ( stream_conseqelem.hasNext() ) {
                    adaptor.addChild(root_1, stream_conseqelem.nextTree());

                }
                stream_conseqelem.reset();

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "conselemtuple"

    public static class conseqelem_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "conseqelem"
    // SimilaritySpec.g:128:1: conseqelem : ( simpleelem | simpleelem equivspecdecl -> ^( EQUIVELEM simpleelem equivspecdecl ) );
    public final SimilaritySpecParser.conseqelem_return conseqelem() throws RecognitionException {
        SimilaritySpecParser.conseqelem_return retval = new SimilaritySpecParser.conseqelem_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        SimilaritySpecParser.simpleelem_return simpleelem124 = null;

        SimilaritySpecParser.simpleelem_return simpleelem125 = null;

        SimilaritySpecParser.equivspecdecl_return equivspecdecl126 = null;


        RewriteRuleSubtreeStream stream_equivspecdecl=new RewriteRuleSubtreeStream(adaptor,"rule equivspecdecl");
        RewriteRuleSubtreeStream stream_simpleelem=new RewriteRuleSubtreeStream(adaptor,"rule simpleelem");
        try {
            // SimilaritySpec.g:129:2: ( simpleelem | simpleelem equivspecdecl -> ^( EQUIVELEM simpleelem equivspecdecl ) )
            int alt52=2;
            alt52 = dfa52.predict(input);
            switch (alt52) {
                case 1 :
                    // SimilaritySpec.g:129:4: simpleelem
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_simpleelem_in_conseqelem1125);
                    simpleelem124=simpleelem();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, simpleelem124.getTree());

                    }
                    break;
                case 2 :
                    // SimilaritySpec.g:130:5: simpleelem equivspecdecl
                    {
                    pushFollow(FOLLOW_simpleelem_in_conseqelem1132);
                    simpleelem125=simpleelem();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_simpleelem.add(simpleelem125.getTree());
                    pushFollow(FOLLOW_equivspecdecl_in_conseqelem1134);
                    equivspecdecl126=equivspecdecl();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_equivspecdecl.add(equivspecdecl126.getTree());


                    // AST REWRITE
                    // elements: simpleelem, equivspecdecl
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 130:30: -> ^( EQUIVELEM simpleelem equivspecdecl )
                    {
                        // SimilaritySpec.g:130:33: ^( EQUIVELEM simpleelem equivspecdecl )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(EQUIVELEM, "EQUIVELEM"), root_1);

                        adaptor.addChild(root_1, stream_simpleelem.nextTree());
                        adaptor.addChild(root_1, stream_equivspecdecl.nextTree());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "conseqelem"

    public static class equivspecdecl_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "equivspecdecl"
    // SimilaritySpec.g:133:1: equivspecdecl : '[' NCNAME ']' ;
    public final SimilaritySpecParser.equivspecdecl_return equivspecdecl() throws RecognitionException {
        SimilaritySpecParser.equivspecdecl_return retval = new SimilaritySpecParser.equivspecdecl_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token char_literal127=null;
        Token NCNAME128=null;
        Token char_literal129=null;

        Object char_literal127_tree=null;
        Object NCNAME128_tree=null;
        Object char_literal129_tree=null;

        try {
            // SimilaritySpec.g:134:2: ( '[' NCNAME ']' )
            // SimilaritySpec.g:134:4: '[' NCNAME ']'
            {
            root_0 = (Object)adaptor.nil();

            char_literal127=(Token)match(input,49,FOLLOW_49_in_equivspecdecl1157); if (state.failed) return retval;
            NCNAME128=(Token)match(input,NCNAME,FOLLOW_NCNAME_in_equivspecdecl1160); if (state.failed) return retval;
            if ( state.backtracking==0 ) {
            NCNAME128_tree = (Object)adaptor.create(NCNAME128);
            adaptor.addChild(root_0, NCNAME128_tree);
            }
            char_literal129=(Token)match(input,50,FOLLOW_50_in_equivspecdecl1162); if (state.failed) return retval;

            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "equivspecdecl"

    public static class simpleelem_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "simpleelem"
    // SimilaritySpec.g:137:1: simpleelem : ( qname | TEXTNODE | SPLITNODESTART ATTVALUE SPLITNODEEND -> ^( SPLITNODE ATTVALUE ) | COMMENTNODE );
    public final SimilaritySpecParser.simpleelem_return simpleelem() throws RecognitionException {
        SimilaritySpecParser.simpleelem_return retval = new SimilaritySpecParser.simpleelem_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token TEXTNODE131=null;
        Token SPLITNODESTART132=null;
        Token ATTVALUE133=null;
        Token SPLITNODEEND134=null;
        Token COMMENTNODE135=null;
        SimilaritySpecParser.qname_return qname130 = null;


        Object TEXTNODE131_tree=null;
        Object SPLITNODESTART132_tree=null;
        Object ATTVALUE133_tree=null;
        Object SPLITNODEEND134_tree=null;
        Object COMMENTNODE135_tree=null;
        RewriteRuleTokenStream stream_ATTVALUE=new RewriteRuleTokenStream(adaptor,"token ATTVALUE");
        RewriteRuleTokenStream stream_SPLITNODESTART=new RewriteRuleTokenStream(adaptor,"token SPLITNODESTART");
        RewriteRuleTokenStream stream_SPLITNODEEND=new RewriteRuleTokenStream(adaptor,"token SPLITNODEEND");

        try {
            // SimilaritySpec.g:138:2: ( qname | TEXTNODE | SPLITNODESTART ATTVALUE SPLITNODEEND -> ^( SPLITNODE ATTVALUE ) | COMMENTNODE )
            int alt53=4;
            switch ( input.LA(1) ) {
            case NCNAME:
                {
                alt53=1;
                }
                break;
            case TEXTNODE:
                {
                alt53=2;
                }
                break;
            case SPLITNODESTART:
                {
                alt53=3;
                }
                break;
            case COMMENTNODE:
                {
                alt53=4;
                }
                break;
            default:
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 53, 0, input);

                throw nvae;
            }

            switch (alt53) {
                case 1 :
                    // SimilaritySpec.g:138:5: qname
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_qname_in_simpleelem1176);
                    qname130=qname();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, qname130.getTree());

                    }
                    break;
                case 2 :
                    // SimilaritySpec.g:139:6: TEXTNODE
                    {
                    root_0 = (Object)adaptor.nil();

                    TEXTNODE131=(Token)match(input,TEXTNODE,FOLLOW_TEXTNODE_in_simpleelem1183); if (state.failed) return retval;
                    if ( state.backtracking==0 ) {
                    TEXTNODE131_tree = (Object)adaptor.create(TEXTNODE131);
                    adaptor.addChild(root_0, TEXTNODE131_tree);
                    }

                    }
                    break;
                case 3 :
                    // SimilaritySpec.g:140:6: SPLITNODESTART ATTVALUE SPLITNODEEND
                    {
                    SPLITNODESTART132=(Token)match(input,SPLITNODESTART,FOLLOW_SPLITNODESTART_in_simpleelem1190); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_SPLITNODESTART.add(SPLITNODESTART132);

                    ATTVALUE133=(Token)match(input,ATTVALUE,FOLLOW_ATTVALUE_in_simpleelem1192); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_ATTVALUE.add(ATTVALUE133);

                    SPLITNODEEND134=(Token)match(input,SPLITNODEEND,FOLLOW_SPLITNODEEND_in_simpleelem1194); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_SPLITNODEEND.add(SPLITNODEEND134);



                    // AST REWRITE
                    // elements: ATTVALUE
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 140:43: -> ^( SPLITNODE ATTVALUE )
                    {
                        // SimilaritySpec.g:140:46: ^( SPLITNODE ATTVALUE )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(SPLITNODE, "SPLITNODE"), root_1);

                        adaptor.addChild(root_1, stream_ATTVALUE.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 4 :
                    // SimilaritySpec.g:141:5: COMMENTNODE
                    {
                    root_0 = (Object)adaptor.nil();

                    COMMENTNODE135=(Token)match(input,COMMENTNODE,FOLLOW_COMMENTNODE_in_simpleelem1208); if (state.failed) return retval;
                    if ( state.backtracking==0 ) {
                    COMMENTNODE135_tree = (Object)adaptor.create(COMMENTNODE135);
                    adaptor.addChild(root_0, COMMENTNODE135_tree);
                    }

                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "simpleelem"

    public static class consallbut_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "consallbut"
    // SimilaritySpec.g:144:1: consallbut : '_' ( '\\\\' '{' ( simpleelem ( ',' simpleelem )* )? '}' )? -> ^( ALLBUT[\"allbut\"] ( simpleelem )* ) ;
    public final SimilaritySpecParser.consallbut_return consallbut() throws RecognitionException {
        SimilaritySpecParser.consallbut_return retval = new SimilaritySpecParser.consallbut_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token char_literal136=null;
        Token char_literal137=null;
        Token char_literal138=null;
        Token char_literal140=null;
        Token char_literal142=null;
        SimilaritySpecParser.simpleelem_return simpleelem139 = null;

        SimilaritySpecParser.simpleelem_return simpleelem141 = null;


        Object char_literal136_tree=null;
        Object char_literal137_tree=null;
        Object char_literal138_tree=null;
        Object char_literal140_tree=null;
        Object char_literal142_tree=null;
        RewriteRuleTokenStream stream_45=new RewriteRuleTokenStream(adaptor,"token 45");
        RewriteRuleTokenStream stream_43=new RewriteRuleTokenStream(adaptor,"token 43");
        RewriteRuleTokenStream stream_42=new RewriteRuleTokenStream(adaptor,"token 42");
        RewriteRuleTokenStream stream_41=new RewriteRuleTokenStream(adaptor,"token 41");
        RewriteRuleTokenStream stream_46=new RewriteRuleTokenStream(adaptor,"token 46");
        RewriteRuleSubtreeStream stream_simpleelem=new RewriteRuleSubtreeStream(adaptor,"rule simpleelem");
        try {
            // SimilaritySpec.g:145:2: ( '_' ( '\\\\' '{' ( simpleelem ( ',' simpleelem )* )? '}' )? -> ^( ALLBUT[\"allbut\"] ( simpleelem )* ) )
            // SimilaritySpec.g:145:5: '_' ( '\\\\' '{' ( simpleelem ( ',' simpleelem )* )? '}' )?
            {
            char_literal136=(Token)match(input,45,FOLLOW_45_in_consallbut1222); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_45.add(char_literal136);

            // SimilaritySpec.g:145:9: ( '\\\\' '{' ( simpleelem ( ',' simpleelem )* )? '}' )?
            int alt56=2;
            int LA56_0 = input.LA(1);

            if ( (LA56_0==46) ) {
                alt56=1;
            }
            switch (alt56) {
                case 1 :
                    // SimilaritySpec.g:145:11: '\\\\' '{' ( simpleelem ( ',' simpleelem )* )? '}'
                    {
                    char_literal137=(Token)match(input,46,FOLLOW_46_in_consallbut1226); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_46.add(char_literal137);

                    char_literal138=(Token)match(input,41,FOLLOW_41_in_consallbut1228); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_41.add(char_literal138);

                    // SimilaritySpec.g:145:20: ( simpleelem ( ',' simpleelem )* )?
                    int alt55=2;
                    int LA55_0 = input.LA(1);

                    if ( ((LA55_0>=TEXTNODE && LA55_0<=SPLITNODESTART)||LA55_0==COMMENTNODE||LA55_0==NCNAME) ) {
                        alt55=1;
                    }
                    switch (alt55) {
                        case 1 :
                            // SimilaritySpec.g:145:21: simpleelem ( ',' simpleelem )*
                            {
                            pushFollow(FOLLOW_simpleelem_in_consallbut1231);
                            simpleelem139=simpleelem();

                            state._fsp--;
                            if (state.failed) return retval;
                            if ( state.backtracking==0 ) stream_simpleelem.add(simpleelem139.getTree());
                            // SimilaritySpec.g:145:32: ( ',' simpleelem )*
                            loop54:
                            do {
                                int alt54=2;
                                int LA54_0 = input.LA(1);

                                if ( (LA54_0==43) ) {
                                    alt54=1;
                                }


                                switch (alt54) {
                            	case 1 :
                            	    // SimilaritySpec.g:145:34: ',' simpleelem
                            	    {
                            	    char_literal140=(Token)match(input,43,FOLLOW_43_in_consallbut1235); if (state.failed) return retval; 
                            	    if ( state.backtracking==0 ) stream_43.add(char_literal140);

                            	    pushFollow(FOLLOW_simpleelem_in_consallbut1237);
                            	    simpleelem141=simpleelem();

                            	    state._fsp--;
                            	    if (state.failed) return retval;
                            	    if ( state.backtracking==0 ) stream_simpleelem.add(simpleelem141.getTree());

                            	    }
                            	    break;

                            	default :
                            	    break loop54;
                                }
                            } while (true);


                            }
                            break;

                    }

                    char_literal142=(Token)match(input,42,FOLLOW_42_in_consallbut1244); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_42.add(char_literal142);


                    }
                    break;

            }



            // AST REWRITE
            // elements: simpleelem
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 146:3: -> ^( ALLBUT[\"allbut\"] ( simpleelem )* )
            {
                // SimilaritySpec.g:146:6: ^( ALLBUT[\"allbut\"] ( simpleelem )* )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(ALLBUT, "allbut"), root_1);

                // SimilaritySpec.g:146:25: ( simpleelem )*
                while ( stream_simpleelem.hasNext() ) {
                    adaptor.addChild(root_1, stream_simpleelem.nextTree());

                }
                stream_simpleelem.reset();

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "consallbut"

    public static class loclmt_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "loclmt"
    // SimilaritySpec.g:150:1: loclmt : LIMIT '{' ( simpleelem ( ( ',' )? simpleelem )* )? '}' -> ^( LIMIT ( simpleelem )* ) ;
    public final SimilaritySpecParser.loclmt_return loclmt() throws RecognitionException {
        SimilaritySpecParser.loclmt_return retval = new SimilaritySpecParser.loclmt_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token LIMIT143=null;
        Token char_literal144=null;
        Token char_literal146=null;
        Token char_literal148=null;
        SimilaritySpecParser.simpleelem_return simpleelem145 = null;

        SimilaritySpecParser.simpleelem_return simpleelem147 = null;


        Object LIMIT143_tree=null;
        Object char_literal144_tree=null;
        Object char_literal146_tree=null;
        Object char_literal148_tree=null;
        RewriteRuleTokenStream stream_43=new RewriteRuleTokenStream(adaptor,"token 43");
        RewriteRuleTokenStream stream_42=new RewriteRuleTokenStream(adaptor,"token 42");
        RewriteRuleTokenStream stream_41=new RewriteRuleTokenStream(adaptor,"token 41");
        RewriteRuleTokenStream stream_LIMIT=new RewriteRuleTokenStream(adaptor,"token LIMIT");
        RewriteRuleSubtreeStream stream_simpleelem=new RewriteRuleSubtreeStream(adaptor,"rule simpleelem");
        try {
            // SimilaritySpec.g:151:2: ( LIMIT '{' ( simpleelem ( ( ',' )? simpleelem )* )? '}' -> ^( LIMIT ( simpleelem )* ) )
            // SimilaritySpec.g:151:5: LIMIT '{' ( simpleelem ( ( ',' )? simpleelem )* )? '}'
            {
            LIMIT143=(Token)match(input,LIMIT,FOLLOW_LIMIT_in_loclmt1279); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_LIMIT.add(LIMIT143);

            char_literal144=(Token)match(input,41,FOLLOW_41_in_loclmt1281); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_41.add(char_literal144);

            // SimilaritySpec.g:151:15: ( simpleelem ( ( ',' )? simpleelem )* )?
            int alt59=2;
            int LA59_0 = input.LA(1);

            if ( ((LA59_0>=TEXTNODE && LA59_0<=SPLITNODESTART)||LA59_0==COMMENTNODE||LA59_0==NCNAME) ) {
                alt59=1;
            }
            switch (alt59) {
                case 1 :
                    // SimilaritySpec.g:151:16: simpleelem ( ( ',' )? simpleelem )*
                    {
                    pushFollow(FOLLOW_simpleelem_in_loclmt1284);
                    simpleelem145=simpleelem();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_simpleelem.add(simpleelem145.getTree());
                    // SimilaritySpec.g:151:27: ( ( ',' )? simpleelem )*
                    loop58:
                    do {
                        int alt58=2;
                        int LA58_0 = input.LA(1);

                        if ( ((LA58_0>=TEXTNODE && LA58_0<=SPLITNODESTART)||LA58_0==COMMENTNODE||LA58_0==NCNAME||LA58_0==43) ) {
                            alt58=1;
                        }


                        switch (alt58) {
                    	case 1 :
                    	    // SimilaritySpec.g:151:29: ( ',' )? simpleelem
                    	    {
                    	    // SimilaritySpec.g:151:29: ( ',' )?
                    	    int alt57=2;
                    	    int LA57_0 = input.LA(1);

                    	    if ( (LA57_0==43) ) {
                    	        alt57=1;
                    	    }
                    	    switch (alt57) {
                    	        case 1 :
                    	            // SimilaritySpec.g:151:30: ','
                    	            {
                    	            char_literal146=(Token)match(input,43,FOLLOW_43_in_loclmt1289); if (state.failed) return retval; 
                    	            if ( state.backtracking==0 ) stream_43.add(char_literal146);


                    	            }
                    	            break;

                    	    }

                    	    pushFollow(FOLLOW_simpleelem_in_loclmt1293);
                    	    simpleelem147=simpleelem();

                    	    state._fsp--;
                    	    if (state.failed) return retval;
                    	    if ( state.backtracking==0 ) stream_simpleelem.add(simpleelem147.getTree());

                    	    }
                    	    break;

                    	default :
                    	    break loop58;
                        }
                    } while (true);


                    }
                    break;

            }

            char_literal148=(Token)match(input,42,FOLLOW_42_in_loclmt1300); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_42.add(char_literal148);



            // AST REWRITE
            // elements: simpleelem, LIMIT
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 151:56: -> ^( LIMIT ( simpleelem )* )
            {
                // SimilaritySpec.g:151:59: ^( LIMIT ( simpleelem )* )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot(stream_LIMIT.nextNode(), root_1);

                // SimilaritySpec.g:151:67: ( simpleelem )*
                while ( stream_simpleelem.hasNext() ) {
                    adaptor.addChild(root_1, stream_simpleelem.nextTree());

                }
                stream_simpleelem.reset();

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "loclmt"

    public static class ignore_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "ignore"
    // SimilaritySpec.g:153:1: ignore : IGNORE '{' ( simpleelem ( ( ',' )? simpleelem )* )? '}' -> ^( IGNORE ( simpleelem )* ) ;
    public final SimilaritySpecParser.ignore_return ignore() throws RecognitionException {
        SimilaritySpecParser.ignore_return retval = new SimilaritySpecParser.ignore_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token IGNORE149=null;
        Token char_literal150=null;
        Token char_literal152=null;
        Token char_literal154=null;
        SimilaritySpecParser.simpleelem_return simpleelem151 = null;

        SimilaritySpecParser.simpleelem_return simpleelem153 = null;


        Object IGNORE149_tree=null;
        Object char_literal150_tree=null;
        Object char_literal152_tree=null;
        Object char_literal154_tree=null;
        RewriteRuleTokenStream stream_43=new RewriteRuleTokenStream(adaptor,"token 43");
        RewriteRuleTokenStream stream_42=new RewriteRuleTokenStream(adaptor,"token 42");
        RewriteRuleTokenStream stream_41=new RewriteRuleTokenStream(adaptor,"token 41");
        RewriteRuleTokenStream stream_IGNORE=new RewriteRuleTokenStream(adaptor,"token IGNORE");
        RewriteRuleSubtreeStream stream_simpleelem=new RewriteRuleSubtreeStream(adaptor,"rule simpleelem");
        try {
            // SimilaritySpec.g:154:2: ( IGNORE '{' ( simpleelem ( ( ',' )? simpleelem )* )? '}' -> ^( IGNORE ( simpleelem )* ) )
            // SimilaritySpec.g:154:5: IGNORE '{' ( simpleelem ( ( ',' )? simpleelem )* )? '}'
            {
            IGNORE149=(Token)match(input,IGNORE,FOLLOW_IGNORE_in_ignore1325); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_IGNORE.add(IGNORE149);

            char_literal150=(Token)match(input,41,FOLLOW_41_in_ignore1327); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_41.add(char_literal150);

            // SimilaritySpec.g:154:16: ( simpleelem ( ( ',' )? simpleelem )* )?
            int alt62=2;
            int LA62_0 = input.LA(1);

            if ( ((LA62_0>=TEXTNODE && LA62_0<=SPLITNODESTART)||LA62_0==COMMENTNODE||LA62_0==NCNAME) ) {
                alt62=1;
            }
            switch (alt62) {
                case 1 :
                    // SimilaritySpec.g:154:17: simpleelem ( ( ',' )? simpleelem )*
                    {
                    pushFollow(FOLLOW_simpleelem_in_ignore1330);
                    simpleelem151=simpleelem();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_simpleelem.add(simpleelem151.getTree());
                    // SimilaritySpec.g:154:28: ( ( ',' )? simpleelem )*
                    loop61:
                    do {
                        int alt61=2;
                        int LA61_0 = input.LA(1);

                        if ( ((LA61_0>=TEXTNODE && LA61_0<=SPLITNODESTART)||LA61_0==COMMENTNODE||LA61_0==NCNAME||LA61_0==43) ) {
                            alt61=1;
                        }


                        switch (alt61) {
                    	case 1 :
                    	    // SimilaritySpec.g:154:30: ( ',' )? simpleelem
                    	    {
                    	    // SimilaritySpec.g:154:30: ( ',' )?
                    	    int alt60=2;
                    	    int LA60_0 = input.LA(1);

                    	    if ( (LA60_0==43) ) {
                    	        alt60=1;
                    	    }
                    	    switch (alt60) {
                    	        case 1 :
                    	            // SimilaritySpec.g:154:31: ','
                    	            {
                    	            char_literal152=(Token)match(input,43,FOLLOW_43_in_ignore1335); if (state.failed) return retval; 
                    	            if ( state.backtracking==0 ) stream_43.add(char_literal152);


                    	            }
                    	            break;

                    	    }

                    	    pushFollow(FOLLOW_simpleelem_in_ignore1339);
                    	    simpleelem153=simpleelem();

                    	    state._fsp--;
                    	    if (state.failed) return retval;
                    	    if ( state.backtracking==0 ) stream_simpleelem.add(simpleelem153.getTree());

                    	    }
                    	    break;

                    	default :
                    	    break loop61;
                        }
                    } while (true);


                    }
                    break;

            }

            char_literal154=(Token)match(input,42,FOLLOW_42_in_ignore1346); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_42.add(char_literal154);



            // AST REWRITE
            // elements: IGNORE, simpleelem
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 154:57: -> ^( IGNORE ( simpleelem )* )
            {
                // SimilaritySpec.g:154:60: ^( IGNORE ( simpleelem )* )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot(stream_IGNORE.nextNode(), root_1);

                // SimilaritySpec.g:154:69: ( simpleelem )*
                while ( stream_simpleelem.hasNext() ) {
                    adaptor.addChild(root_1, stream_simpleelem.nextTree());

                }
                stream_simpleelem.reset();

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "ignore"

    public static class qnameoptorrequired_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "qnameoptorrequired"
    // SimilaritySpec.g:156:1: qnameoptorrequired : ( qname | qname OPT -> ^( OPTIONAL qname ) | qname MANDAT -> ^( MANDATORY qname ) );
    public final SimilaritySpecParser.qnameoptorrequired_return qnameoptorrequired() throws RecognitionException {
        SimilaritySpecParser.qnameoptorrequired_return retval = new SimilaritySpecParser.qnameoptorrequired_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token OPT157=null;
        Token MANDAT159=null;
        SimilaritySpecParser.qname_return qname155 = null;

        SimilaritySpecParser.qname_return qname156 = null;

        SimilaritySpecParser.qname_return qname158 = null;


        Object OPT157_tree=null;
        Object MANDAT159_tree=null;
        RewriteRuleTokenStream stream_MANDAT=new RewriteRuleTokenStream(adaptor,"token MANDAT");
        RewriteRuleTokenStream stream_OPT=new RewriteRuleTokenStream(adaptor,"token OPT");
        RewriteRuleSubtreeStream stream_qname=new RewriteRuleSubtreeStream(adaptor,"rule qname");
        try {
            // SimilaritySpec.g:157:2: ( qname | qname OPT -> ^( OPTIONAL qname ) | qname MANDAT -> ^( MANDATORY qname ) )
            int alt63=3;
            int LA63_0 = input.LA(1);

            if ( (LA63_0==NCNAME) ) {
                switch ( input.LA(2) ) {
                case 51:
                    {
                    int LA63_2 = input.LA(3);

                    if ( (LA63_2==NCNAME) ) {
                        switch ( input.LA(4) ) {
                        case EOF:
                        case ABSENT:
                        case NCNAME:
                        case 42:
                        case 43:
                            {
                            alt63=1;
                            }
                            break;
                        case OPT:
                            {
                            alt63=2;
                            }
                            break;
                        case MANDAT:
                            {
                            alt63=3;
                            }
                            break;
                        default:
                            if (state.backtracking>0) {state.failed=true; return retval;}
                            NoViableAltException nvae =
                                new NoViableAltException("", 63, 6, input);

                            throw nvae;
                        }

                    }
                    else {
                        if (state.backtracking>0) {state.failed=true; return retval;}
                        NoViableAltException nvae =
                            new NoViableAltException("", 63, 2, input);

                        throw nvae;
                    }
                    }
                    break;
                case EOF:
                case ABSENT:
                case NCNAME:
                case 42:
                case 43:
                    {
                    alt63=1;
                    }
                    break;
                case OPT:
                    {
                    alt63=2;
                    }
                    break;
                case MANDAT:
                    {
                    alt63=3;
                    }
                    break;
                default:
                    if (state.backtracking>0) {state.failed=true; return retval;}
                    NoViableAltException nvae =
                        new NoViableAltException("", 63, 1, input);

                    throw nvae;
                }

            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 63, 0, input);

                throw nvae;
            }
            switch (alt63) {
                case 1 :
                    // SimilaritySpec.g:157:4: qname
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_qname_in_qnameoptorrequired1364);
                    qname155=qname();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, qname155.getTree());

                    }
                    break;
                case 2 :
                    // SimilaritySpec.g:158:5: qname OPT
                    {
                    pushFollow(FOLLOW_qname_in_qnameoptorrequired1370);
                    qname156=qname();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_qname.add(qname156.getTree());
                    OPT157=(Token)match(input,OPT,FOLLOW_OPT_in_qnameoptorrequired1372); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_OPT.add(OPT157);



                    // AST REWRITE
                    // elements: qname
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 158:15: -> ^( OPTIONAL qname )
                    {
                        // SimilaritySpec.g:158:18: ^( OPTIONAL qname )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(OPTIONAL, "OPTIONAL"), root_1);

                        adaptor.addChild(root_1, stream_qname.nextTree());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 3 :
                    // SimilaritySpec.g:159:4: qname MANDAT
                    {
                    pushFollow(FOLLOW_qname_in_qnameoptorrequired1385);
                    qname158=qname();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_qname.add(qname158.getTree());
                    MANDAT159=(Token)match(input,MANDAT,FOLLOW_MANDAT_in_qnameoptorrequired1387); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_MANDAT.add(MANDAT159);



                    // AST REWRITE
                    // elements: qname
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (Object)adaptor.nil();
                    // 159:17: -> ^( MANDATORY qname )
                    {
                        // SimilaritySpec.g:159:20: ^( MANDATORY qname )
                        {
                        Object root_1 = (Object)adaptor.nil();
                        root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(MANDATORY, "MANDATORY"), root_1);

                        adaptor.addChild(root_1, stream_qname.nextTree());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "qnameoptorrequired"

    public static class qname_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "qname"
    // SimilaritySpec.g:162:1: qname : ( prefixedName | unprefixedName );
    public final SimilaritySpecParser.qname_return qname() throws RecognitionException {
        SimilaritySpecParser.qname_return retval = new SimilaritySpecParser.qname_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        SimilaritySpecParser.prefixedName_return prefixedName160 = null;

        SimilaritySpecParser.unprefixedName_return unprefixedName161 = null;



        try {
            // SimilaritySpec.g:162:7: ( prefixedName | unprefixedName )
            int alt64=2;
            int LA64_0 = input.LA(1);

            if ( (LA64_0==NCNAME) ) {
                int LA64_1 = input.LA(2);

                if ( (LA64_1==51) ) {
                    alt64=1;
                }
                else if ( (LA64_1==EOF||LA64_1==OPT||LA64_1==MANDAT||LA64_1==ABSENT||(LA64_1>=TEXTNODE && LA64_1<=SPLITNODESTART)||LA64_1==COMMENTNODE||LA64_1==NCNAME||(LA64_1>=41 && LA64_1<=43)||(LA64_1>=47 && LA64_1<=49)) ) {
                    alt64=2;
                }
                else {
                    if (state.backtracking>0) {state.failed=true; return retval;}
                    NoViableAltException nvae =
                        new NoViableAltException("", 64, 1, input);

                    throw nvae;
                }
            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 64, 0, input);

                throw nvae;
            }
            switch (alt64) {
                case 1 :
                    // SimilaritySpec.g:162:9: prefixedName
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_prefixedName_in_qname1406);
                    prefixedName160=prefixedName();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, prefixedName160.getTree());

                    }
                    break;
                case 2 :
                    // SimilaritySpec.g:163:4: unprefixedName
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_unprefixedName_in_qname1411);
                    unprefixedName161=unprefixedName();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, unprefixedName161.getTree());

                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "qname"

    public static class prefixedName_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "prefixedName"
    // SimilaritySpec.g:166:1: prefixedName : NCNAME ':' NCNAME -> ^( PREFIXEDNAME ( NCNAME )+ ) ;
    public final SimilaritySpecParser.prefixedName_return prefixedName() throws RecognitionException {
        SimilaritySpecParser.prefixedName_return retval = new SimilaritySpecParser.prefixedName_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token NCNAME162=null;
        Token char_literal163=null;
        Token NCNAME164=null;

        Object NCNAME162_tree=null;
        Object char_literal163_tree=null;
        Object NCNAME164_tree=null;
        RewriteRuleTokenStream stream_NCNAME=new RewriteRuleTokenStream(adaptor,"token NCNAME");
        RewriteRuleTokenStream stream_51=new RewriteRuleTokenStream(adaptor,"token 51");

        try {
            // SimilaritySpec.g:166:17: ( NCNAME ':' NCNAME -> ^( PREFIXEDNAME ( NCNAME )+ ) )
            // SimilaritySpec.g:166:21: NCNAME ':' NCNAME
            {
            NCNAME162=(Token)match(input,NCNAME,FOLLOW_NCNAME_in_prefixedName1427); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_NCNAME.add(NCNAME162);

            char_literal163=(Token)match(input,51,FOLLOW_51_in_prefixedName1429); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_51.add(char_literal163);

            NCNAME164=(Token)match(input,NCNAME,FOLLOW_NCNAME_in_prefixedName1431); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_NCNAME.add(NCNAME164);



            // AST REWRITE
            // elements: NCNAME
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 166:39: -> ^( PREFIXEDNAME ( NCNAME )+ )
            {
                // SimilaritySpec.g:166:42: ^( PREFIXEDNAME ( NCNAME )+ )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(PREFIXEDNAME, "PREFIXEDNAME"), root_1);

                if ( !(stream_NCNAME.hasNext()) ) {
                    throw new RewriteEarlyExitException();
                }
                while ( stream_NCNAME.hasNext() ) {
                    adaptor.addChild(root_1, stream_NCNAME.nextNode());

                }
                stream_NCNAME.reset();

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "prefixedName"

    public static class unprefixedName_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "unprefixedName"
    // SimilaritySpec.g:169:1: unprefixedName : NCNAME -> ^( UNPREFIXEDNAME NCNAME ) ;
    public final SimilaritySpecParser.unprefixedName_return unprefixedName() throws RecognitionException {
        SimilaritySpecParser.unprefixedName_return retval = new SimilaritySpecParser.unprefixedName_return();
        retval.start = input.LT(1);

        Object root_0 = null;

        Token NCNAME165=null;

        Object NCNAME165_tree=null;
        RewriteRuleTokenStream stream_NCNAME=new RewriteRuleTokenStream(adaptor,"token NCNAME");

        try {
            // SimilaritySpec.g:169:19: ( NCNAME -> ^( UNPREFIXEDNAME NCNAME ) )
            // SimilaritySpec.g:169:22: NCNAME
            {
            NCNAME165=(Token)match(input,NCNAME,FOLLOW_NCNAME_in_unprefixedName1454); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_NCNAME.add(NCNAME165);



            // AST REWRITE
            // elements: NCNAME
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (Object)adaptor.nil();
            // 169:29: -> ^( UNPREFIXEDNAME NCNAME )
            {
                // SimilaritySpec.g:169:32: ^( UNPREFIXEDNAME NCNAME )
                {
                Object root_1 = (Object)adaptor.nil();
                root_1 = (Object)adaptor.becomeRoot((Object)adaptor.create(UNPREFIXEDNAME, "UNPREFIXEDNAME"), root_1);

                adaptor.addChild(root_1, stream_NCNAME.nextNode());

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "unprefixedName"

    // $ANTLR start synpred28_SimilaritySpec
    public final void synpred28_SimilaritySpec_fragment() throws RecognitionException {   
        // SimilaritySpec.g:93:24: ( ( ',' )? annoentry )
        // SimilaritySpec.g:93:24: ( ',' )? annoentry
        {
        // SimilaritySpec.g:93:24: ( ',' )?
        int alt75=2;
        int LA75_0 = input.LA(1);

        if ( (LA75_0==43) ) {
            alt75=1;
        }
        switch (alt75) {
            case 1 :
                // SimilaritySpec.g:93:25: ','
                {
                match(input,43,FOLLOW_43_in_synpred28_SimilaritySpec706); if (state.failed) return ;

                }
                break;

        }

        pushFollow(FOLLOW_annoentry_in_synpred28_SimilaritySpec711);
        annoentry();

        state._fsp--;
        if (state.failed) return ;

        }
    }
    // $ANTLR end synpred28_SimilaritySpec

    // $ANTLR start synpred31_SimilaritySpec
    public final void synpred31_SimilaritySpec_fragment() throws RecognitionException {   
        // SimilaritySpec.g:94:17: ( ( ',' )? annoentry )
        // SimilaritySpec.g:94:17: ( ',' )? annoentry
        {
        // SimilaritySpec.g:94:17: ( ',' )?
        int alt78=2;
        int LA78_0 = input.LA(1);

        if ( (LA78_0==43) ) {
            alt78=1;
        }
        switch (alt78) {
            case 1 :
                // SimilaritySpec.g:94:18: ','
                {
                match(input,43,FOLLOW_43_in_synpred31_SimilaritySpec723); if (state.failed) return ;

                }
                break;

        }

        pushFollow(FOLLOW_annoentry_in_synpred31_SimilaritySpec728);
        annoentry();

        state._fsp--;
        if (state.failed) return ;

        }
    }
    // $ANTLR end synpred31_SimilaritySpec

    // $ANTLR start synpred36_SimilaritySpec
    public final void synpred36_SimilaritySpec_fragment() throws RecognitionException {   
        // SimilaritySpec.g:103:18: ( ( ',' )? consentry )
        // SimilaritySpec.g:103:18: ( ',' )? consentry
        {
        // SimilaritySpec.g:103:18: ( ',' )?
        int alt80=2;
        int LA80_0 = input.LA(1);

        if ( (LA80_0==43) ) {
            alt80=1;
        }
        switch (alt80) {
            case 1 :
                // SimilaritySpec.g:103:19: ','
                {
                match(input,43,FOLLOW_43_in_synpred36_SimilaritySpec807); if (state.failed) return ;

                }
                break;

        }

        pushFollow(FOLLOW_consentry_in_synpred36_SimilaritySpec812);
        consentry();

        state._fsp--;
        if (state.failed) return ;

        }
    }
    // $ANTLR end synpred36_SimilaritySpec

    // $ANTLR start synpred39_SimilaritySpec
    public final void synpred39_SimilaritySpec_fragment() throws RecognitionException {   
        // SimilaritySpec.g:104:30: ( ( ',' )? consentry )
        // SimilaritySpec.g:104:30: ( ',' )? consentry
        {
        // SimilaritySpec.g:104:30: ( ',' )?
        int alt83=2;
        int LA83_0 = input.LA(1);

        if ( (LA83_0==43) ) {
            alt83=1;
        }
        switch (alt83) {
            case 1 :
                // SimilaritySpec.g:104:31: ','
                {
                match(input,43,FOLLOW_43_in_synpred39_SimilaritySpec838); if (state.failed) return ;

                }
                break;

        }

        pushFollow(FOLLOW_consentry_in_synpred39_SimilaritySpec843);
        consentry();

        state._fsp--;
        if (state.failed) return ;

        }
    }
    // $ANTLR end synpred39_SimilaritySpec

    // $ANTLR start synpred42_SimilaritySpec
    public final void synpred42_SimilaritySpec_fragment() throws RecognitionException {   
        // SimilaritySpec.g:105:33: ( ( ',' )? consentry )
        // SimilaritySpec.g:105:33: ( ',' )? consentry
        {
        // SimilaritySpec.g:105:33: ( ',' )?
        int alt86=2;
        int LA86_0 = input.LA(1);

        if ( (LA86_0==43) ) {
            alt86=1;
        }
        switch (alt86) {
            case 1 :
                // SimilaritySpec.g:105:34: ','
                {
                match(input,43,FOLLOW_43_in_synpred42_SimilaritySpec868); if (state.failed) return ;

                }
                break;

        }

        pushFollow(FOLLOW_consentry_in_synpred42_SimilaritySpec873);
        consentry();

        state._fsp--;
        if (state.failed) return ;

        }
    }
    // $ANTLR end synpred42_SimilaritySpec

    // $ANTLR start synpred45_SimilaritySpec
    public final void synpred45_SimilaritySpec_fragment() throws RecognitionException {   
        // SimilaritySpec.g:106:26: ( ( ',' )? consentry )
        // SimilaritySpec.g:106:26: ( ',' )? consentry
        {
        // SimilaritySpec.g:106:26: ( ',' )?
        int alt89=2;
        int LA89_0 = input.LA(1);

        if ( (LA89_0==43) ) {
            alt89=1;
        }
        switch (alt89) {
            case 1 :
                // SimilaritySpec.g:106:27: ','
                {
                match(input,43,FOLLOW_43_in_synpred45_SimilaritySpec898); if (state.failed) return ;

                }
                break;

        }

        pushFollow(FOLLOW_consentry_in_synpred45_SimilaritySpec903);
        consentry();

        state._fsp--;
        if (state.failed) return ;

        }
    }
    // $ANTLR end synpred45_SimilaritySpec

    // Delegated rules

    public final boolean synpred36_SimilaritySpec() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred36_SimilaritySpec_fragment(); // can never throw exception
        } catch (RecognitionException re) {
            System.err.println("impossible: "+re);
        }
        boolean success = !state.failed;
        input.rewind(start);
        state.backtracking--;
        state.failed=false;
        return success;
    }
    public final boolean synpred28_SimilaritySpec() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred28_SimilaritySpec_fragment(); // can never throw exception
        } catch (RecognitionException re) {
            System.err.println("impossible: "+re);
        }
        boolean success = !state.failed;
        input.rewind(start);
        state.backtracking--;
        state.failed=false;
        return success;
    }
    public final boolean synpred42_SimilaritySpec() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred42_SimilaritySpec_fragment(); // can never throw exception
        } catch (RecognitionException re) {
            System.err.println("impossible: "+re);
        }
        boolean success = !state.failed;
        input.rewind(start);
        state.backtracking--;
        state.failed=false;
        return success;
    }
    public final boolean synpred31_SimilaritySpec() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred31_SimilaritySpec_fragment(); // can never throw exception
        } catch (RecognitionException re) {
            System.err.println("impossible: "+re);
        }
        boolean success = !state.failed;
        input.rewind(start);
        state.backtracking--;
        state.failed=false;
        return success;
    }
    public final boolean synpred45_SimilaritySpec() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred45_SimilaritySpec_fragment(); // can never throw exception
        } catch (RecognitionException re) {
            System.err.println("impossible: "+re);
        }
        boolean success = !state.failed;
        input.rewind(start);
        state.backtracking--;
        state.failed=false;
        return success;
    }
    public final boolean synpred39_SimilaritySpec() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred39_SimilaritySpec_fragment(); // can never throw exception
        } catch (RecognitionException re) {
            System.err.println("impossible: "+re);
        }
        boolean success = !state.failed;
        input.rewind(start);
        state.backtracking--;
        state.failed=false;
        return success;
    }


    protected DFA52 dfa52 = new DFA52(this);
    static final String DFA52_eotS =
        "\13\uffff";
    static final String DFA52_eofS =
        "\1\uffff\2\6\1\uffff\1\6\4\uffff\2\6";
    static final String DFA52_minS =
        "\3\30\1\44\1\30\1\43\2\uffff\1\32\2\30";
    static final String DFA52_maxS =
        "\1\43\1\63\1\61\1\44\1\61\1\43\2\uffff\1\32\2\61";
    static final String DFA52_acceptS =
        "\6\uffff\1\1\1\2\3\uffff";
    static final String DFA52_specialS =
        "\13\uffff}>";
    static final String[] DFA52_transitionS = {
            "\1\2\1\3\2\uffff\1\4\6\uffff\1\1",
            "\2\6\2\uffff\1\6\6\uffff\1\6\6\uffff\2\6\3\uffff\2\6\1\7\1"+
            "\uffff\1\5",
            "\2\6\2\uffff\1\6\6\uffff\1\6\6\uffff\2\6\3\uffff\2\6\1\7",
            "\1\10",
            "\2\6\2\uffff\1\6\6\uffff\1\6\6\uffff\2\6\3\uffff\2\6\1\7",
            "\1\11",
            "",
            "",
            "\1\12",
            "\2\6\2\uffff\1\6\6\uffff\1\6\6\uffff\2\6\3\uffff\2\6\1\7",
            "\2\6\2\uffff\1\6\6\uffff\1\6\6\uffff\2\6\3\uffff\2\6\1\7"
    };

    static final short[] DFA52_eot = DFA.unpackEncodedString(DFA52_eotS);
    static final short[] DFA52_eof = DFA.unpackEncodedString(DFA52_eofS);
    static final char[] DFA52_min = DFA.unpackEncodedStringToUnsignedChars(DFA52_minS);
    static final char[] DFA52_max = DFA.unpackEncodedStringToUnsignedChars(DFA52_maxS);
    static final short[] DFA52_accept = DFA.unpackEncodedString(DFA52_acceptS);
    static final short[] DFA52_special = DFA.unpackEncodedString(DFA52_specialS);
    static final short[][] DFA52_transition;

    static {
        int numStates = DFA52_transitionS.length;
        DFA52_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA52_transition[i] = DFA.unpackEncodedString(DFA52_transitionS[i]);
        }
    }

    class DFA52 extends DFA {

        public DFA52(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 52;
            this.eot = DFA52_eot;
            this.eof = DFA52_eof;
            this.min = DFA52_min;
            this.max = DFA52_max;
            this.accept = DFA52_accept;
            this.special = DFA52_special;
            this.transition = DFA52_transition;
        }
        public String getDescription() {
            return "128:1: conseqelem : ( simpleelem | simpleelem equivspecdecl -> ^( EQUIVELEM simpleelem equivspecdecl ) );";
        }
    }
 

    public static final BitSet FOLLOW_equivspec_in_equivspecs233 = new BitSet(new long[]{0x0000000000002002L});
    public static final BitSet FOLLOW_EQUIVSPEC_in_equivspec259 = new BitSet(new long[]{0x0000000800000000L});
    public static final BitSet FOLLOW_NCNAME_in_equivspec262 = new BitSet(new long[]{0x000002000000C000L});
    public static final BitSet FOLLOW_namespace_in_equivspec265 = new BitSet(new long[]{0x0000020000000000L});
    public static final BitSet FOLLOW_extension_in_equivspec269 = new BitSet(new long[]{0x0000020000000000L});
    public static final BitSet FOLLOW_41_in_equivspec273 = new BitSet(new long[]{0x0000040420070000L});
    public static final BitSet FOLLOW_xmlnsdecls_in_equivspec277 = new BitSet(new long[]{0x0000040420070000L});
    public static final BitSet FOLLOW_element_in_equivspec282 = new BitSet(new long[]{0x0000040420060000L});
    public static final BitSet FOLLOW_glblmt_in_equivspec286 = new BitSet(new long[]{0x0000040420060000L});
    public static final BitSet FOLLOW_ignore_in_equivspec290 = new BitSet(new long[]{0x0000040420060000L});
    public static final BitSet FOLLOW_defaultspec_in_equivspec295 = new BitSet(new long[]{0x0000040000000000L});
    public static final BitSet FOLLOW_42_in_equivspec298 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_EXTENDS_in_extension320 = new BitSet(new long[]{0x0000000800000000L});
    public static final BitSet FOLLOW_NCNAME_in_extension322 = new BitSet(new long[]{0x0000080800000002L});
    public static final BitSet FOLLOW_43_in_extension325 = new BitSet(new long[]{0x0000000800000000L});
    public static final BitSet FOLLOW_NCNAME_in_extension328 = new BitSet(new long[]{0x0000080800000002L});
    public static final BitSet FOLLOW_XMLNSPREFIX_in_xmlnsdecls352 = new BitSet(new long[]{0x0000000800000000L});
    public static final BitSet FOLLOW_NCNAME_in_xmlnsdecls355 = new BitSet(new long[]{0x0000100000000000L});
    public static final BitSet FOLLOW_44_in_xmlnsdecls357 = new BitSet(new long[]{0x0000001000000000L});
    public static final BitSet FOLLOW_ATTVALUE_in_xmlnsdecls360 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_XMLNS_in_namespace373 = new BitSet(new long[]{0x0000001000000000L});
    public static final BitSet FOLLOW_ATTVALUE_in_namespace376 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ELEMENT_in_element391 = new BitSet(new long[]{0x0000000813000000L});
    public static final BitSet FOLLOW_simpleelem_in_element394 = new BitSet(new long[]{0x0000020000000000L});
    public static final BitSet FOLLOW_41_in_element396 = new BitSet(new long[]{0x0000040000280010L});
    public static final BitSet FOLLOW_alternative_in_element399 = new BitSet(new long[]{0x0000040000000000L});
    public static final BitSet FOLLOW_42_in_element402 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_DEFAULTSPEC_in_defaultspec413 = new BitSet(new long[]{0x0000020000000000L});
    public static final BitSet FOLLOW_41_in_defaultspec416 = new BitSet(new long[]{0x0000040000280010L});
    public static final BitSet FOLLOW_alternative_in_defaultspec419 = new BitSet(new long[]{0x0000040000000000L});
    public static final BitSet FOLLOW_42_in_defaultspec422 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_LIMIT_in_glblmt439 = new BitSet(new long[]{0x0000020000000000L});
    public static final BitSet FOLLOW_41_in_glblmt442 = new BitSet(new long[]{0x0000040813000000L});
    public static final BitSet FOLLOW_simpleelem_in_glblmt446 = new BitSet(new long[]{0x00000C0813000000L});
    public static final BitSet FOLLOW_43_in_glblmt451 = new BitSet(new long[]{0x0000000813000000L});
    public static final BitSet FOLLOW_simpleelem_in_glblmt456 = new BitSet(new long[]{0x00000C0813000000L});
    public static final BitSet FOLLOW_42_in_glblmt463 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ALTERNATIVES_in_alternative475 = new BitSet(new long[]{0x0000020000000000L});
    public static final BitSet FOLLOW_41_in_alternative478 = new BitSet(new long[]{0x0000020000000000L});
    public static final BitSet FOLLOW_41_in_alternative483 = new BitSet(new long[]{0x0000000000280010L});
    public static final BitSet FOLLOW_altentry_in_alternative486 = new BitSet(new long[]{0x0000040000000000L});
    public static final BitSet FOLLOW_42_in_alternative488 = new BitSet(new long[]{0x0000060000000000L});
    public static final BitSet FOLLOW_42_in_alternative494 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_altentry_in_alternative514 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_annotations_in_altentry536 = new BitSet(new long[]{0x0000000000280012L});
    public static final BitSet FOLLOW_constituents_in_altentry538 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_constituents_in_altentry569 = new BitSet(new long[]{0x0000000000080002L});
    public static final BitSet FOLLOW_annotations_in_altentry571 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ANNOTATIONS_in_annotations595 = new BitSet(new long[]{0x0000020000000000L});
    public static final BitSet FOLLOW_41_in_annotations598 = new BitSet(new long[]{0x0000200000000000L});
    public static final BitSet FOLLOW_annoallbut_in_annotations601 = new BitSet(new long[]{0x00000C0800100000L});
    public static final BitSet FOLLOW_43_in_annotations605 = new BitSet(new long[]{0x0000080800100000L});
    public static final BitSet FOLLOW_annoentry_in_annotations610 = new BitSet(new long[]{0x00000C0800100000L});
    public static final BitSet FOLLOW_42_in_annotations614 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ANNOTATIONS_in_annotations634 = new BitSet(new long[]{0x0000020000000000L});
    public static final BitSet FOLLOW_41_in_annotations637 = new BitSet(new long[]{0x00000C0800100000L});
    public static final BitSet FOLLOW_annoentry_in_annotations640 = new BitSet(new long[]{0x0000040000000000L});
    public static final BitSet FOLLOW_42_in_annotations643 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_45_in_annoallbut656 = new BitSet(new long[]{0x0000400000000002L});
    public static final BitSet FOLLOW_46_in_annoallbut660 = new BitSet(new long[]{0x0000020000000000L});
    public static final BitSet FOLLOW_41_in_annoallbut662 = new BitSet(new long[]{0x0000000800000000L});
    public static final BitSet FOLLOW_qname_in_annoallbut664 = new BitSet(new long[]{0x00000C0000000000L});
    public static final BitSet FOLLOW_43_in_annoallbut667 = new BitSet(new long[]{0x0000000800000000L});
    public static final BitSet FOLLOW_qname_in_annoallbut669 = new BitSet(new long[]{0x00000C0000000000L});
    public static final BitSet FOLLOW_42_in_annoallbut673 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_qnameoptorrequired_in_annoentry702 = new BitSet(new long[]{0x0000080800100002L});
    public static final BitSet FOLLOW_43_in_annoentry706 = new BitSet(new long[]{0x0000080800100000L});
    public static final BitSet FOLLOW_annoentry_in_annoentry711 = new BitSet(new long[]{0x0000080800100002L});
    public static final BitSet FOLLOW_annoabsent_in_annoentry719 = new BitSet(new long[]{0x0000080800100002L});
    public static final BitSet FOLLOW_43_in_annoentry723 = new BitSet(new long[]{0x0000080800100000L});
    public static final BitSet FOLLOW_annoentry_in_annoentry728 = new BitSet(new long[]{0x0000080800100002L});
    public static final BitSet FOLLOW_ABSENT_in_annoabsent742 = new BitSet(new long[]{0x0000020000000000L});
    public static final BitSet FOLLOW_41_in_annoabsent744 = new BitSet(new long[]{0x0000000800000000L});
    public static final BitSet FOLLOW_qname_in_annoabsent746 = new BitSet(new long[]{0x00000C0800000000L});
    public static final BitSet FOLLOW_43_in_annoabsent751 = new BitSet(new long[]{0x0000000800000000L});
    public static final BitSet FOLLOW_qname_in_annoabsent755 = new BitSet(new long[]{0x00000C0800000000L});
    public static final BitSet FOLLOW_42_in_annoabsent760 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_CONSTITUENTS_in_constituents780 = new BitSet(new long[]{0x0000020000000000L});
    public static final BitSet FOLLOW_41_in_constituents783 = new BitSet(new long[]{0x0000040000D40800L});
    public static final BitSet FOLLOW_consentry_in_constituents786 = new BitSet(new long[]{0x0000040000000000L});
    public static final BitSet FOLLOW_42_in_constituents789 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_consordered_in_consentry803 = new BitSet(new long[]{0x0000080000D40802L});
    public static final BitSet FOLLOW_43_in_consentry807 = new BitSet(new long[]{0x0000000000D40800L});
    public static final BitSet FOLLOW_consentry_in_consentry812 = new BitSet(new long[]{0x0000080000D40802L});
    public static final BitSet FOLLOW_consabsent_in_consentry834 = new BitSet(new long[]{0x0000080000D40802L});
    public static final BitSet FOLLOW_43_in_consentry838 = new BitSet(new long[]{0x0000000000D40800L});
    public static final BitSet FOLLOW_consentry_in_consentry843 = new BitSet(new long[]{0x0000080000D40802L});
    public static final BitSet FOLLOW_consmandatory_in_consentry864 = new BitSet(new long[]{0x0000080000D40802L});
    public static final BitSet FOLLOW_43_in_consentry868 = new BitSet(new long[]{0x0000000000D40800L});
    public static final BitSet FOLLOW_consentry_in_consentry873 = new BitSet(new long[]{0x0000080000D40802L});
    public static final BitSet FOLLOW_loclmt_in_consentry894 = new BitSet(new long[]{0x0000080000D40802L});
    public static final BitSet FOLLOW_43_in_consentry898 = new BitSet(new long[]{0x0000000000D40800L});
    public static final BitSet FOLLOW_consentry_in_consentry903 = new BitSet(new long[]{0x0000080000D40802L});
    public static final BitSet FOLLOW_UNORDERED_in_consordered933 = new BitSet(new long[]{0x0000020000000000L});
    public static final BitSet FOLLOW_ORDERED_in_consordered938 = new BitSet(new long[]{0x0000020000000000L});
    public static final BitSet FOLLOW_conscoll_in_consordered943 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ABSENT_in_consabsent959 = new BitSet(new long[]{0x0000020000000000L});
    public static final BitSet FOLLOW_41_in_consabsent961 = new BitSet(new long[]{0x0000000813000000L});
    public static final BitSet FOLLOW_simpleelem_in_consabsent963 = new BitSet(new long[]{0x00000C0813000000L});
    public static final BitSet FOLLOW_43_in_consabsent968 = new BitSet(new long[]{0x0000000813000000L});
    public static final BitSet FOLLOW_simpleelem_in_consabsent972 = new BitSet(new long[]{0x00000C0813000000L});
    public static final BitSet FOLLOW_42_in_consabsent977 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_MANDATORY_in_consmandatory998 = new BitSet(new long[]{0x0000020000000000L});
    public static final BitSet FOLLOW_41_in_consmandatory1000 = new BitSet(new long[]{0x0000000813000000L});
    public static final BitSet FOLLOW_simpleelem_in_consmandatory1002 = new BitSet(new long[]{0x00000C0813000000L});
    public static final BitSet FOLLOW_43_in_consmandatory1007 = new BitSet(new long[]{0x0000000813000000L});
    public static final BitSet FOLLOW_simpleelem_in_consmandatory1011 = new BitSet(new long[]{0x00000C0813000000L});
    public static final BitSet FOLLOW_42_in_consmandatory1016 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_41_in_conscoll1038 = new BitSet(new long[]{0x0000800813000000L});
    public static final BitSet FOLLOW_conseqelem_in_conscoll1042 = new BitSet(new long[]{0x00008C0813000000L});
    public static final BitSet FOLLOW_conselemtuple_in_conscoll1046 = new BitSet(new long[]{0x00008C0813000000L});
    public static final BitSet FOLLOW_43_in_conscoll1052 = new BitSet(new long[]{0x0000800813000000L});
    public static final BitSet FOLLOW_conseqelem_in_conscoll1058 = new BitSet(new long[]{0x00008C0813000000L});
    public static final BitSet FOLLOW_conselemtuple_in_conscoll1062 = new BitSet(new long[]{0x00008C0813000000L});
    public static final BitSet FOLLOW_42_in_conscoll1068 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_41_in_conscoll1075 = new BitSet(new long[]{0x0000200000000000L});
    public static final BitSet FOLLOW_consallbut_in_conscoll1078 = new BitSet(new long[]{0x0000040000000000L});
    public static final BitSet FOLLOW_42_in_conscoll1080 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_47_in_conselemtuple1092 = new BitSet(new long[]{0x0000000813000000L});
    public static final BitSet FOLLOW_conseqelem_in_conselemtuple1094 = new BitSet(new long[]{0x0001080000000000L});
    public static final BitSet FOLLOW_43_in_conselemtuple1097 = new BitSet(new long[]{0x0000000813000000L});
    public static final BitSet FOLLOW_conseqelem_in_conselemtuple1099 = new BitSet(new long[]{0x0001080000000000L});
    public static final BitSet FOLLOW_48_in_conselemtuple1103 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_simpleelem_in_conseqelem1125 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_simpleelem_in_conseqelem1132 = new BitSet(new long[]{0x0002000000000000L});
    public static final BitSet FOLLOW_equivspecdecl_in_conseqelem1134 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_49_in_equivspecdecl1157 = new BitSet(new long[]{0x0000000800000000L});
    public static final BitSet FOLLOW_NCNAME_in_equivspecdecl1160 = new BitSet(new long[]{0x0004000000000000L});
    public static final BitSet FOLLOW_50_in_equivspecdecl1162 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_qname_in_simpleelem1176 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_TEXTNODE_in_simpleelem1183 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_SPLITNODESTART_in_simpleelem1190 = new BitSet(new long[]{0x0000001000000000L});
    public static final BitSet FOLLOW_ATTVALUE_in_simpleelem1192 = new BitSet(new long[]{0x0000000004000000L});
    public static final BitSet FOLLOW_SPLITNODEEND_in_simpleelem1194 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_COMMENTNODE_in_simpleelem1208 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_45_in_consallbut1222 = new BitSet(new long[]{0x0000400000000002L});
    public static final BitSet FOLLOW_46_in_consallbut1226 = new BitSet(new long[]{0x0000020000000000L});
    public static final BitSet FOLLOW_41_in_consallbut1228 = new BitSet(new long[]{0x0000040813000000L});
    public static final BitSet FOLLOW_simpleelem_in_consallbut1231 = new BitSet(new long[]{0x00000C0000000000L});
    public static final BitSet FOLLOW_43_in_consallbut1235 = new BitSet(new long[]{0x0000000813000000L});
    public static final BitSet FOLLOW_simpleelem_in_consallbut1237 = new BitSet(new long[]{0x00000C0000000000L});
    public static final BitSet FOLLOW_42_in_consallbut1244 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_LIMIT_in_loclmt1279 = new BitSet(new long[]{0x0000020000000000L});
    public static final BitSet FOLLOW_41_in_loclmt1281 = new BitSet(new long[]{0x0000040813000000L});
    public static final BitSet FOLLOW_simpleelem_in_loclmt1284 = new BitSet(new long[]{0x00000C0813000000L});
    public static final BitSet FOLLOW_43_in_loclmt1289 = new BitSet(new long[]{0x0000000813000000L});
    public static final BitSet FOLLOW_simpleelem_in_loclmt1293 = new BitSet(new long[]{0x00000C0813000000L});
    public static final BitSet FOLLOW_42_in_loclmt1300 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_IGNORE_in_ignore1325 = new BitSet(new long[]{0x0000020000000000L});
    public static final BitSet FOLLOW_41_in_ignore1327 = new BitSet(new long[]{0x0000040813000000L});
    public static final BitSet FOLLOW_simpleelem_in_ignore1330 = new BitSet(new long[]{0x00000C0813000000L});
    public static final BitSet FOLLOW_43_in_ignore1335 = new BitSet(new long[]{0x0000000813000000L});
    public static final BitSet FOLLOW_simpleelem_in_ignore1339 = new BitSet(new long[]{0x00000C0813000000L});
    public static final BitSet FOLLOW_42_in_ignore1346 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_qname_in_qnameoptorrequired1364 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_qname_in_qnameoptorrequired1370 = new BitSet(new long[]{0x0000000000000100L});
    public static final BitSet FOLLOW_OPT_in_qnameoptorrequired1372 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_qname_in_qnameoptorrequired1385 = new BitSet(new long[]{0x0000000000000400L});
    public static final BitSet FOLLOW_MANDAT_in_qnameoptorrequired1387 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_prefixedName_in_qname1406 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_unprefixedName_in_qname1411 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_NCNAME_in_prefixedName1427 = new BitSet(new long[]{0x0008000000000000L});
    public static final BitSet FOLLOW_51_in_prefixedName1429 = new BitSet(new long[]{0x0000000800000000L});
    public static final BitSet FOLLOW_NCNAME_in_prefixedName1431 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_NCNAME_in_unprefixedName1454 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_43_in_synpred28_SimilaritySpec706 = new BitSet(new long[]{0x0000080800100000L});
    public static final BitSet FOLLOW_annoentry_in_synpred28_SimilaritySpec711 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_43_in_synpred31_SimilaritySpec723 = new BitSet(new long[]{0x0000080800100000L});
    public static final BitSet FOLLOW_annoentry_in_synpred31_SimilaritySpec728 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_43_in_synpred36_SimilaritySpec807 = new BitSet(new long[]{0x0000000000D40800L});
    public static final BitSet FOLLOW_consentry_in_synpred36_SimilaritySpec812 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_43_in_synpred39_SimilaritySpec838 = new BitSet(new long[]{0x0000000000D40800L});
    public static final BitSet FOLLOW_consentry_in_synpred39_SimilaritySpec843 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_43_in_synpred42_SimilaritySpec868 = new BitSet(new long[]{0x0000000000D40800L});
    public static final BitSet FOLLOW_consentry_in_synpred42_SimilaritySpec873 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_43_in_synpred45_SimilaritySpec898 = new BitSet(new long[]{0x0000000000D40800L});
    public static final BitSet FOLLOW_consentry_in_synpred45_SimilaritySpec903 = new BitSet(new long[]{0x0000000000000002L});

}