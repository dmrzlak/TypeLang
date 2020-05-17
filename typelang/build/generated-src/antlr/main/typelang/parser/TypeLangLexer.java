// Generated from typelang\parser\TypeLang.g by ANTLR 4.5
package typelang.parser; import static typelang.AST.*; import typelang.*; import typelang.Type.*;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class TypeLangLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.5", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, T__8=9, 
		T__9=10, T__10=11, T__11=12, T__12=13, T__13=14, T__14=15, Define=16, 
		Let=17, Lambda=18, If=19, Car=20, Cdr=21, Cons=22, List=23, Null=24, Less=25, 
		Equal=26, Greater=27, TrueLiteral=28, FalseLiteral=29, Ref=30, Deref=31, 
		Assign=32, Free=33, Fork=34, Lock=35, UnLock=36, Process=37, Send=38, 
		Stop=39, Self=40, Dot=41, Number=42, Identifier=43, Letter=44, LetterOrDigit=45, 
		StrLiteral=46, AT=47, ELLIPSIS=48, WS=49, Comment=50, Line_Comment=51;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] ruleNames = {
		"T__0", "T__1", "T__2", "T__3", "T__4", "T__5", "T__6", "T__7", "T__8", 
		"T__9", "T__10", "T__11", "T__12", "T__13", "T__14", "Define", "Let", 
		"Lambda", "If", "Car", "Cdr", "Cons", "List", "Null", "Less", "Equal", 
		"Greater", "TrueLiteral", "FalseLiteral", "Ref", "Deref", "Assign", "Free", 
		"Fork", "Lock", "UnLock", "Process", "Send", "Stop", "Self", "Dot", "Number", 
		"Identifier", "Letter", "LetterOrDigit", "DIGIT", "ESCQUOTE", "StrLiteral", 
		"AT", "ELLIPSIS", "WS", "Comment", "Line_Comment"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'-'", "'('", "'+'", "')'", "'*'", "'/'", "':'", "'bool'", "'unit'", 
		"'num'", "'List'", "','", "'Ref'", "'Str'", "'->'", "'define'", "'let'", 
		"'lambda'", "'if'", "'car'", "'cdr'", "'cons'", "'list'", "'null?'", "'<'", 
		"'='", "'>'", "'#t'", "'#f'", "'ref'", "'deref'", "'set!'", "'free'", 
		"'fork'", "'lock'", "'unlock'", "'process'", "'send'", "'stop'", "'self'", 
		"'.'", null, null, null, null, null, "'@'", "'...'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, "Define", "Let", "Lambda", "If", "Car", "Cdr", 
		"Cons", "List", "Null", "Less", "Equal", "Greater", "TrueLiteral", "FalseLiteral", 
		"Ref", "Deref", "Assign", "Free", "Fork", "Lock", "UnLock", "Process", 
		"Send", "Stop", "Self", "Dot", "Number", "Identifier", "Letter", "LetterOrDigit", 
		"StrLiteral", "AT", "ELLIPSIS", "WS", "Comment", "Line_Comment"
	};
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}


	public TypeLangLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "TypeLang.g"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	@Override
	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 43:
			return Letter_sempred((RuleContext)_localctx, predIndex);
		case 44:
			return LetterOrDigit_sempred((RuleContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean Letter_sempred(RuleContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return Character.isJavaIdentifierStart(_input.LA(-1));
		case 1:
			return Character.isJavaIdentifierStart(Character.toCodePoint((char)_input.LA(-2), (char)_input.LA(-1)));
		}
		return true;
	}
	private boolean LetterOrDigit_sempred(RuleContext _localctx, int predIndex) {
		switch (predIndex) {
		case 2:
			return Character.isJavaIdentifierPart(_input.LA(-1));
		case 3:
			return Character.isJavaIdentifierPart(Character.toCodePoint((char)_input.LA(-2), (char)_input.LA(-1)));
		}
		return true;
	}

	public static final String _serializedATN =
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\2\65\u0163\b\1\4\2"+
		"\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4"+
		"\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22"+
		"\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31"+
		"\t\31\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t"+
		" \4!\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\4\'\t\'\4(\t(\4)\t)\4*\t*\4+\t"+
		"+\4,\t,\4-\t-\4.\t.\4/\t/\4\60\t\60\4\61\t\61\4\62\t\62\4\63\t\63\4\64"+
		"\t\64\4\65\t\65\4\66\t\66\3\2\3\2\3\3\3\3\3\4\3\4\3\5\3\5\3\6\3\6\3\7"+
		"\3\7\3\b\3\b\3\t\3\t\3\t\3\t\3\t\3\n\3\n\3\n\3\n\3\n\3\13\3\13\3\13\3"+
		"\13\3\f\3\f\3\f\3\f\3\f\3\r\3\r\3\16\3\16\3\16\3\16\3\17\3\17\3\17\3\17"+
		"\3\20\3\20\3\20\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\22\3\22\3\22\3\22"+
		"\3\23\3\23\3\23\3\23\3\23\3\23\3\23\3\24\3\24\3\24\3\25\3\25\3\25\3\25"+
		"\3\26\3\26\3\26\3\26\3\27\3\27\3\27\3\27\3\27\3\30\3\30\3\30\3\30\3\30"+
		"\3\31\3\31\3\31\3\31\3\31\3\31\3\32\3\32\3\33\3\33\3\34\3\34\3\35\3\35"+
		"\3\35\3\36\3\36\3\36\3\37\3\37\3\37\3\37\3 \3 \3 \3 \3 \3 \3!\3!\3!\3"+
		"!\3!\3\"\3\"\3\"\3\"\3\"\3#\3#\3#\3#\3#\3$\3$\3$\3$\3$\3%\3%\3%\3%\3%"+
		"\3%\3%\3&\3&\3&\3&\3&\3&\3&\3&\3\'\3\'\3\'\3\'\3\'\3(\3(\3(\3(\3(\3)\3"+
		")\3)\3)\3)\3*\3*\3+\6+\u0114\n+\r+\16+\u0115\3,\3,\7,\u011a\n,\f,\16,"+
		"\u011d\13,\3-\3-\3-\3-\3-\3-\5-\u0125\n-\3.\3.\3.\3.\3.\3.\5.\u012d\n"+
		".\3/\3/\3\60\3\60\3\60\3\61\3\61\3\61\7\61\u0137\n\61\f\61\16\61\u013a"+
		"\13\61\3\61\3\61\3\62\3\62\3\63\3\63\3\63\3\63\3\64\6\64\u0145\n\64\r"+
		"\64\16\64\u0146\3\64\3\64\3\65\3\65\3\65\3\65\7\65\u014f\n\65\f\65\16"+
		"\65\u0152\13\65\3\65\3\65\3\65\3\65\3\65\3\66\3\66\3\66\3\66\7\66\u015d"+
		"\n\66\f\66\16\66\u0160\13\66\3\66\3\66\4\u0138\u0150\2\67\3\3\5\4\7\5"+
		"\t\6\13\7\r\b\17\t\21\n\23\13\25\f\27\r\31\16\33\17\35\20\37\21!\22#\23"+
		"%\24\'\25)\26+\27-\30/\31\61\32\63\33\65\34\67\359\36;\37= ?!A\"C#E$G"+
		"%I&K\'M(O)Q*S+U,W-Y.[/]\2_\2a\60c\61e\62g\63i\64k\65\3\2\t\6\2&&C\\aa"+
		"c|\4\2\2\u0101\ud802\udc01\3\2\ud802\udc01\3\2\udc02\ue001\7\2&&\62;C"+
		"\\aac|\4\2\f\f\17\17\5\2\13\f\16\17\"\"\u016b\2\3\3\2\2\2\2\5\3\2\2\2"+
		"\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3"+
		"\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2"+
		"\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2\2\2\2%\3\2\2\2\2\'\3\2"+
		"\2\2\2)\3\2\2\2\2+\3\2\2\2\2-\3\2\2\2\2/\3\2\2\2\2\61\3\2\2\2\2\63\3\2"+
		"\2\2\2\65\3\2\2\2\2\67\3\2\2\2\29\3\2\2\2\2;\3\2\2\2\2=\3\2\2\2\2?\3\2"+
		"\2\2\2A\3\2\2\2\2C\3\2\2\2\2E\3\2\2\2\2G\3\2\2\2\2I\3\2\2\2\2K\3\2\2\2"+
		"\2M\3\2\2\2\2O\3\2\2\2\2Q\3\2\2\2\2S\3\2\2\2\2U\3\2\2\2\2W\3\2\2\2\2Y"+
		"\3\2\2\2\2[\3\2\2\2\2a\3\2\2\2\2c\3\2\2\2\2e\3\2\2\2\2g\3\2\2\2\2i\3\2"+
		"\2\2\2k\3\2\2\2\3m\3\2\2\2\5o\3\2\2\2\7q\3\2\2\2\ts\3\2\2\2\13u\3\2\2"+
		"\2\rw\3\2\2\2\17y\3\2\2\2\21{\3\2\2\2\23\u0080\3\2\2\2\25\u0085\3\2\2"+
		"\2\27\u0089\3\2\2\2\31\u008e\3\2\2\2\33\u0090\3\2\2\2\35\u0094\3\2\2\2"+
		"\37\u0098\3\2\2\2!\u009b\3\2\2\2#\u00a2\3\2\2\2%\u00a6\3\2\2\2\'\u00ad"+
		"\3\2\2\2)\u00b0\3\2\2\2+\u00b4\3\2\2\2-\u00b8\3\2\2\2/\u00bd\3\2\2\2\61"+
		"\u00c2\3\2\2\2\63\u00c8\3\2\2\2\65\u00ca\3\2\2\2\67\u00cc\3\2\2\29\u00ce"+
		"\3\2\2\2;\u00d1\3\2\2\2=\u00d4\3\2\2\2?\u00d8\3\2\2\2A\u00de\3\2\2\2C"+
		"\u00e3\3\2\2\2E\u00e8\3\2\2\2G\u00ed\3\2\2\2I\u00f2\3\2\2\2K\u00f9\3\2"+
		"\2\2M\u0101\3\2\2\2O\u0106\3\2\2\2Q\u010b\3\2\2\2S\u0110\3\2\2\2U\u0113"+
		"\3\2\2\2W\u0117\3\2\2\2Y\u0124\3\2\2\2[\u012c\3\2\2\2]\u012e\3\2\2\2_"+
		"\u0130\3\2\2\2a\u0133\3\2\2\2c\u013d\3\2\2\2e\u013f\3\2\2\2g\u0144\3\2"+
		"\2\2i\u014a\3\2\2\2k\u0158\3\2\2\2mn\7/\2\2n\4\3\2\2\2op\7*\2\2p\6\3\2"+
		"\2\2qr\7-\2\2r\b\3\2\2\2st\7+\2\2t\n\3\2\2\2uv\7,\2\2v\f\3\2\2\2wx\7\61"+
		"\2\2x\16\3\2\2\2yz\7<\2\2z\20\3\2\2\2{|\7d\2\2|}\7q\2\2}~\7q\2\2~\177"+
		"\7n\2\2\177\22\3\2\2\2\u0080\u0081\7w\2\2\u0081\u0082\7p\2\2\u0082\u0083"+
		"\7k\2\2\u0083\u0084\7v\2\2\u0084\24\3\2\2\2\u0085\u0086\7p\2\2\u0086\u0087"+
		"\7w\2\2\u0087\u0088\7o\2\2\u0088\26\3\2\2\2\u0089\u008a\7N\2\2\u008a\u008b"+
		"\7k\2\2\u008b\u008c\7u\2\2\u008c\u008d\7v\2\2\u008d\30\3\2\2\2\u008e\u008f"+
		"\7.\2\2\u008f\32\3\2\2\2\u0090\u0091\7T\2\2\u0091\u0092\7g\2\2\u0092\u0093"+
		"\7h\2\2\u0093\34\3\2\2\2\u0094\u0095\7U\2\2\u0095\u0096\7v\2\2\u0096\u0097"+
		"\7t\2\2\u0097\36\3\2\2\2\u0098\u0099\7/\2\2\u0099\u009a\7@\2\2\u009a "+
		"\3\2\2\2\u009b\u009c\7f\2\2\u009c\u009d\7g\2\2\u009d\u009e\7h\2\2\u009e"+
		"\u009f\7k\2\2\u009f\u00a0\7p\2\2\u00a0\u00a1\7g\2\2\u00a1\"\3\2\2\2\u00a2"+
		"\u00a3\7n\2\2\u00a3\u00a4\7g\2\2\u00a4\u00a5\7v\2\2\u00a5$\3\2\2\2\u00a6"+
		"\u00a7\7n\2\2\u00a7\u00a8\7c\2\2\u00a8\u00a9\7o\2\2\u00a9\u00aa\7d\2\2"+
		"\u00aa\u00ab\7f\2\2\u00ab\u00ac\7c\2\2\u00ac&\3\2\2\2\u00ad\u00ae\7k\2"+
		"\2\u00ae\u00af\7h\2\2\u00af(\3\2\2\2\u00b0\u00b1\7e\2\2\u00b1\u00b2\7"+
		"c\2\2\u00b2\u00b3\7t\2\2\u00b3*\3\2\2\2\u00b4\u00b5\7e\2\2\u00b5\u00b6"+
		"\7f\2\2\u00b6\u00b7\7t\2\2\u00b7,\3\2\2\2\u00b8\u00b9\7e\2\2\u00b9\u00ba"+
		"\7q\2\2\u00ba\u00bb\7p\2\2\u00bb\u00bc\7u\2\2\u00bc.\3\2\2\2\u00bd\u00be"+
		"\7n\2\2\u00be\u00bf\7k\2\2\u00bf\u00c0\7u\2\2\u00c0\u00c1\7v\2\2\u00c1"+
		"\60\3\2\2\2\u00c2\u00c3\7p\2\2\u00c3\u00c4\7w\2\2\u00c4\u00c5\7n\2\2\u00c5"+
		"\u00c6\7n\2\2\u00c6\u00c7\7A\2\2\u00c7\62\3\2\2\2\u00c8\u00c9\7>\2\2\u00c9"+
		"\64\3\2\2\2\u00ca\u00cb\7?\2\2\u00cb\66\3\2\2\2\u00cc\u00cd\7@\2\2\u00cd"+
		"8\3\2\2\2\u00ce\u00cf\7%\2\2\u00cf\u00d0\7v\2\2\u00d0:\3\2\2\2\u00d1\u00d2"+
		"\7%\2\2\u00d2\u00d3\7h\2\2\u00d3<\3\2\2\2\u00d4\u00d5\7t\2\2\u00d5\u00d6"+
		"\7g\2\2\u00d6\u00d7\7h\2\2\u00d7>\3\2\2\2\u00d8\u00d9\7f\2\2\u00d9\u00da"+
		"\7g\2\2\u00da\u00db\7t\2\2\u00db\u00dc\7g\2\2\u00dc\u00dd\7h\2\2\u00dd"+
		"@\3\2\2\2\u00de\u00df\7u\2\2\u00df\u00e0\7g\2\2\u00e0\u00e1\7v\2\2\u00e1"+
		"\u00e2\7#\2\2\u00e2B\3\2\2\2\u00e3\u00e4\7h\2\2\u00e4\u00e5\7t\2\2\u00e5"+
		"\u00e6\7g\2\2\u00e6\u00e7\7g\2\2\u00e7D\3\2\2\2\u00e8\u00e9\7h\2\2\u00e9"+
		"\u00ea\7q\2\2\u00ea\u00eb\7t\2\2\u00eb\u00ec\7m\2\2\u00ecF\3\2\2\2\u00ed"+
		"\u00ee\7n\2\2\u00ee\u00ef\7q\2\2\u00ef\u00f0\7e\2\2\u00f0\u00f1\7m\2\2"+
		"\u00f1H\3\2\2\2\u00f2\u00f3\7w\2\2\u00f3\u00f4\7p\2\2\u00f4\u00f5\7n\2"+
		"\2\u00f5\u00f6\7q\2\2\u00f6\u00f7\7e\2\2\u00f7\u00f8\7m\2\2\u00f8J\3\2"+
		"\2\2\u00f9\u00fa\7r\2\2\u00fa\u00fb\7t\2\2\u00fb\u00fc\7q\2\2\u00fc\u00fd"+
		"\7e\2\2\u00fd\u00fe\7g\2\2\u00fe\u00ff\7u\2\2\u00ff\u0100\7u\2\2\u0100"+
		"L\3\2\2\2\u0101\u0102\7u\2\2\u0102\u0103\7g\2\2\u0103\u0104\7p\2\2\u0104"+
		"\u0105\7f\2\2\u0105N\3\2\2\2\u0106\u0107\7u\2\2\u0107\u0108\7v\2\2\u0108"+
		"\u0109\7q\2\2\u0109\u010a\7r\2\2\u010aP\3\2\2\2\u010b\u010c\7u\2\2\u010c"+
		"\u010d\7g\2\2\u010d\u010e\7n\2\2\u010e\u010f\7h\2\2\u010fR\3\2\2\2\u0110"+
		"\u0111\7\60\2\2\u0111T\3\2\2\2\u0112\u0114\5]/\2\u0113\u0112\3\2\2\2\u0114"+
		"\u0115\3\2\2\2\u0115\u0113\3\2\2\2\u0115\u0116\3\2\2\2\u0116V\3\2\2\2"+
		"\u0117\u011b\5Y-\2\u0118\u011a\5[.\2\u0119\u0118\3\2\2\2\u011a\u011d\3"+
		"\2\2\2\u011b\u0119\3\2\2\2\u011b\u011c\3\2\2\2\u011cX\3\2\2\2\u011d\u011b"+
		"\3\2\2\2\u011e\u0125\t\2\2\2\u011f\u0120\n\3\2\2\u0120\u0125\6-\2\2\u0121"+
		"\u0122\t\4\2\2\u0122\u0123\t\5\2\2\u0123\u0125\6-\3\2\u0124\u011e\3\2"+
		"\2\2\u0124\u011f\3\2\2\2\u0124\u0121\3\2\2\2\u0125Z\3\2\2\2\u0126\u012d"+
		"\t\6\2\2\u0127\u0128\n\3\2\2\u0128\u012d\6.\4\2\u0129\u012a\t\4\2\2\u012a"+
		"\u012b\t\5\2\2\u012b\u012d\6.\5\2\u012c\u0126\3\2\2\2\u012c\u0127\3\2"+
		"\2\2\u012c\u0129\3\2\2\2\u012d\\\3\2\2\2\u012e\u012f\4\62;\2\u012f^\3"+
		"\2\2\2\u0130\u0131\7^\2\2\u0131\u0132\7$\2\2\u0132`\3\2\2\2\u0133\u0138"+
		"\7$\2\2\u0134\u0137\5_\60\2\u0135\u0137\n\7\2\2\u0136\u0134\3\2\2\2\u0136"+
		"\u0135\3\2\2\2\u0137\u013a\3\2\2\2\u0138\u0139\3\2\2\2\u0138\u0136\3\2"+
		"\2\2\u0139\u013b\3\2\2\2\u013a\u0138\3\2\2\2\u013b\u013c\7$\2\2\u013c"+
		"b\3\2\2\2\u013d\u013e\7B\2\2\u013ed\3\2\2\2\u013f\u0140\7\60\2\2\u0140"+
		"\u0141\7\60\2\2\u0141\u0142\7\60\2\2\u0142f\3\2\2\2\u0143\u0145\t\b\2"+
		"\2\u0144\u0143\3\2\2\2\u0145\u0146\3\2\2\2\u0146\u0144\3\2\2\2\u0146\u0147"+
		"\3\2\2\2\u0147\u0148\3\2\2\2\u0148\u0149\b\64\2\2\u0149h\3\2\2\2\u014a"+
		"\u014b\7\61\2\2\u014b\u014c\7,\2\2\u014c\u0150\3\2\2\2\u014d\u014f\13"+
		"\2\2\2\u014e\u014d\3\2\2\2\u014f\u0152\3\2\2\2\u0150\u0151\3\2\2\2\u0150"+
		"\u014e\3\2\2\2\u0151\u0153\3\2\2\2\u0152\u0150\3\2\2\2\u0153\u0154\7,"+
		"\2\2\u0154\u0155\7\61\2\2\u0155\u0156\3\2\2\2\u0156\u0157\b\65\2\2\u0157"+
		"j\3\2\2\2\u0158\u0159\7\61\2\2\u0159\u015a\7\61\2\2\u015a\u015e\3\2\2"+
		"\2\u015b\u015d\n\7\2\2\u015c\u015b\3\2\2\2\u015d\u0160\3\2\2\2\u015e\u015c"+
		"\3\2\2\2\u015e\u015f\3\2\2\2\u015f\u0161\3\2\2\2\u0160\u015e\3\2\2\2\u0161"+
		"\u0162\b\66\2\2\u0162l\3\2\2\2\f\2\u0115\u011b\u0124\u012c\u0136\u0138"+
		"\u0146\u0150\u015e\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}