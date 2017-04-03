/* The following code was generated by JFlex 1.6.1 */

/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/ 

/********************************************************************************/
/* This file is used to generate a rule checker for COM.DATA.Alloc rule.	 	*/
/* For further information on this, we advise you to refer to RNC manuals.	    */
/* As many comments have been done on the ExampleRule.lex file, this file       */
/* will restrain its comments on modifications.								    */
/*																			    */
/********************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;


/**
 * This class is a scanner generated by 
 * <a href="http://www.jflex.de/">JFlex</a> 1.6.1
 * from the specification file <tt>lex/COMDESIGNAlloc.lex</tt>
 */
public class COMDESIGNAlloc extends AbstractRule {

  /** This character denotes the end of file */
  public static final int YYEOF = -1;

  /** initial size of the lookahead buffer */
  private static final int ZZ_BUFFERSIZE = 16384;

  /** lexical states */
  public static final int YYINITIAL = 0;
  public static final int COMMENT = 2;
  public static final int NAMING = 4;
  public static final int NEW_LINE = 6;
  public static final int LINE = 8;
  public static final int OPEN = 10;
  public static final int CLOSE = 12;
  public static final int ALLOC = 14;
  public static final int DEALLOC = 16;

  /**
   * ZZ_LEXSTATE[l] is the state in the DFA for the lexical state l
   * ZZ_LEXSTATE[l+1] is the state in the DFA for the lexical state l
   *                  at the beginning of a line
   * l is of the form l = 2*k, k a non negative integer
   */
  private static final int ZZ_LEXSTATE[] = { 
     0,  0,  1,  1,  2,  2,  3,  3,  4,  4,  5,  5,  6,  6,  7,  7, 
     8, 8
  };

  /** 
   * Translates characters to character classes
   */
  private static final String ZZ_CMAP_PACKED = 
    "\11\0\1\26\1\33\1\34\1\35\1\34\22\0\1\26\1\1\1\31"+
    "\4\0\1\30\10\0\12\32\7\0\1\20\1\16\1\5\1\14\1\13"+
    "\1\2\1\17\1\27\1\24\2\27\1\22\1\21\1\4\1\10\1\11"+
    "\1\27\1\12\1\25\1\6\1\3\5\27\4\0\1\23\1\0\1\20"+
    "\1\16\1\5\1\14\1\13\1\2\1\17\1\27\1\24\2\27\1\22"+
    "\1\21\1\4\1\10\1\11\1\27\1\12\1\25\1\6\1\3\5\27"+
    "\12\0\1\34\252\0\2\7\115\0\1\15\u1ea8\0\1\34\1\34\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\udfe6\0";

  /** 
   * Translates characters to character classes
   */
  private static final char [] ZZ_CMAP = zzUnpackCMap(ZZ_CMAP_PACKED);

  /** 
   * Translates DFA states to action switch labels.
   */
  private static final int [] ZZ_ACTION = zzUnpackAction();

  private static final String ZZ_ACTION_PACKED_0 =
    "\11\0\1\1\1\2\5\1\1\3\1\4\1\5\1\6"+
    "\1\1\1\2\12\1\1\5\1\4\1\5\1\2\11\5"+
    "\1\3\1\4\3\7\3\10\1\11\1\12\40\0\1\7"+
    "\1\10\37\0\1\7\1\10\40\0\2\5\46\0\1\13"+
    "\1\0\1\14\16\0\1\13\1\0\1\14\4\0\1\14"+
    "\7\0\1\15\11\0\1\5\3\0\1\15\3\0\1\15"+
    "\35\0\1\16\2\0\1\16\2\0\1\16\7\0\1\17"+
    "\1\0\1\17\1\0\1\17\15\0\2\5";

  private static int [] zzUnpackAction() {
    int [] result = new int[308];
    int offset = 0;
    offset = zzUnpackAction(ZZ_ACTION_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackAction(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }


  /** 
   * Translates a state to a row index in the transition table
   */
  private static final int [] ZZ_ROWMAP = zzUnpackRowMap();

  private static final String ZZ_ROWMAP_PACKED_0 =
    "\0\0\0\36\0\74\0\132\0\170\0\226\0\264\0\322"+
    "\0\360\0\u010e\0\u010e\0\u012c\0\u014a\0\u0168\0\u0186\0\u01a4"+
    "\0\u010e\0\u010e\0\u010e\0\u01c2\0\u01e0\0\u01e0\0\u01fe\0\u021c"+
    "\0\u023a\0\u0258\0\u0276\0\u0294\0\u02b2\0\u02d0\0\u02ee\0\u030c"+
    "\0\u01e0\0\u01e0\0\u032a\0\u032a\0\u012c\0\u0348\0\u0168\0\u0366"+
    "\0\u01a4\0\u014a\0\u0186\0\u0384\0\u03a2\0\u032a\0\u032a\0\u03c0"+
    "\0\u03de\0\u03fc\0\u041a\0\u0438\0\u0456\0\u0474\0\u0492\0\u04b0"+
    "\0\u04ce\0\u04ec\0\u050a\0\u0528\0\u0546\0\u0564\0\u0582\0\u05a0"+
    "\0\u05be\0\u05dc\0\u05fa\0\u0618\0\u0636\0\u0654\0\u0672\0\u0690"+
    "\0\u06ae\0\u06cc\0\u06ea\0\u0708\0\u0726\0\u0744\0\u0762\0\u0780"+
    "\0\u079e\0\u07bc\0\u07da\0\u07f8\0\u0816\0\u0834\0\u0852\0\u0870"+
    "\0\u088e\0\u08ac\0\u08ca\0\u08e8\0\u0906\0\u0924\0\u0942\0\u0960"+
    "\0\u097e\0\u099c\0\u09ba\0\u09d8\0\u09f6\0\u0a14\0\u0a32\0\u0a50"+
    "\0\u0a6e\0\u0a8c\0\u0aaa\0\u0ac8\0\u0ae6\0\u0b04\0\u0b22\0\u0b40"+
    "\0\u0b5e\0\u0b7c\0\u0b9a\0\u0bb8\0\u0bd6\0\u0bf4\0\u0c12\0\u0c30"+
    "\0\u0c4e\0\u0c6c\0\u0c8a\0\u0ca8\0\u0cc6\0\u0ce4\0\u0d02\0\u0d20"+
    "\0\u0d3e\0\u0d5c\0\u0d7a\0\u0d98\0\u0db6\0\u0dd4\0\u0df2\0\u0e10"+
    "\0\u0e2e\0\u0e4c\0\u0e6a\0\u0e88\0\u0ea6\0\u0ec4\0\u0ee2\0\u0f00"+
    "\0\u0f1e\0\u0f3c\0\u0f5a\0\u0f78\0\u0f96\0\u0fb4\0\u0fd2\0\u0ff0"+
    "\0\u100e\0\u102c\0\u03c0\0\u041a\0\u104a\0\u1068\0\u1086\0\u10a4"+
    "\0\u10c2\0\u10e0\0\u10fe\0\u111c\0\u113a\0\u1158\0\u1176\0\u1194"+
    "\0\u11b2\0\u11d0\0\u11ee\0\u120c\0\u122a\0\u1248\0\u1266\0\u1284"+
    "\0\u12a2\0\u12c0\0\u12de\0\u12fc\0\u131a\0\u1338\0\u1356\0\u1374"+
    "\0\u1392\0\u13b0\0\u13ce\0\u13ec\0\u140a\0\u1428\0\u1446\0\u1464"+
    "\0\u1482\0\u14a0\0\u010e\0\u14be\0\u010e\0\u14dc\0\u14fa\0\u1518"+
    "\0\u1536\0\u1554\0\u1572\0\u1590\0\u15ae\0\u15cc\0\u15ea\0\u1608"+
    "\0\u1626\0\u1644\0\u1662\0\u1680\0\u169e\0\u0708\0\u16bc\0\u16da"+
    "\0\u16f8\0\u1716\0\u07bc\0\u1734\0\u1752\0\u1770\0\u178e\0\u17ac"+
    "\0\u17ca\0\u17e8\0\u010e\0\u1806\0\u1824\0\u1842\0\u1860\0\u187e"+
    "\0\u189c\0\u18ba\0\u18d8\0\u18f6\0\u1680\0\u1914\0\u1932\0\u1950"+
    "\0\u0708\0\u196e\0\u198c\0\u19aa\0\u07bc\0\u19c8\0\u19e6\0\u1a04"+
    "\0\u1a22\0\u1a40\0\u1a5e\0\u1a7c\0\u1a9a\0\u1ab8\0\u1ad6\0\u1af4"+
    "\0\u1b12\0\u1b30\0\u1b4e\0\u1b6c\0\u1b8a\0\u1ba8\0\u1bc6\0\u1be4"+
    "\0\u1c02\0\u1c20\0\u1c3e\0\u1c5c\0\u1c7a\0\u1c98\0\u1cb6\0\u1cd4"+
    "\0\u1cf2\0\u1d10\0\u010e\0\u1d2e\0\u1d4c\0\u0708\0\u1d6a\0\u1d88"+
    "\0\u07bc\0\u1da6\0\u1dc4\0\u1de2\0\u1e00\0\u1e1e\0\u1e3c\0\u1e5a"+
    "\0\u010e\0\u1e78\0\u0708\0\u1e96\0\u07bc\0\u1eb4\0\u1ed2\0\u1ef0"+
    "\0\u1f0e\0\u1f2c\0\u1f4a\0\u1f68\0\u1f86\0\u1fa4\0\u1fc2\0\u1fe0"+
    "\0\u1ffe\0\u201c\0\u0708\0\u07bc";

  private static int [] zzUnpackRowMap() {
    int [] result = new int[308];
    int offset = 0;
    offset = zzUnpackRowMap(ZZ_ROWMAP_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackRowMap(String packed, int offset, int [] result) {
    int i = 0;  /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int high = packed.charAt(i++) << 16;
      result[j++] = high | packed.charAt(i++);
    }
    return j;
  }

  /** 
   * The transition table of the DFA
   */
  private static final int [] ZZ_TRANS = zzUnpackTrans();

  private static final String ZZ_TRANS_PACKED_0 =
    "\1\12\1\13\1\14\4\12\1\15\1\12\1\16\3\12"+
    "\1\17\3\12\1\20\2\12\1\15\1\17\5\12\1\21"+
    "\2\22\1\23\1\13\31\23\1\21\2\22\1\23\1\13"+
    "\5\24\1\23\5\24\1\23\5\24\1\23\2\24\1\23"+
    "\1\24\3\23\1\21\2\22\1\25\1\26\1\27\4\30"+
    "\1\31\1\30\1\32\3\30\1\33\3\30\1\34\2\30"+
    "\1\35\1\36\1\25\1\30\1\37\1\40\1\30\1\41"+
    "\2\42\1\43\1\44\1\45\4\23\1\46\1\23\1\47"+
    "\3\23\1\50\3\23\1\51\2\23\1\52\1\53\1\43"+
    "\1\23\1\54\1\55\1\23\1\56\2\57\1\23\1\13"+
    "\1\60\1\61\3\60\1\23\5\60\1\23\5\60\1\23"+
    "\2\60\1\23\1\60\2\23\1\62\1\23\2\22\1\23"+
    "\1\13\1\63\1\64\3\63\1\23\5\63\1\23\5\63"+
    "\1\23\2\63\1\23\1\63\2\23\1\65\1\23\2\22"+
    "\1\23\1\13\5\66\1\23\5\66\1\23\5\66\1\23"+
    "\2\66\1\23\1\66\4\23\2\22\1\23\1\13\5\67"+
    "\1\23\5\67\1\23\5\67\1\23\2\67\1\23\1\67"+
    "\4\23\2\22\41\0\1\70\36\0\1\71\43\0\1\72"+
    "\26\0\1\73\42\0\1\74\27\0\5\24\1\0\5\24"+
    "\1\0\10\24\1\0\1\24\2\0\1\24\10\0\1\75"+
    "\2\0\1\76\3\0\1\77\3\0\1\100\1\101\16\0"+
    "\1\102\1\103\3\0\1\104\1\0\1\105\3\0\1\106"+
    "\3\0\1\107\2\0\1\104\1\106\12\0\1\102\4\0"+
    "\1\104\1\0\1\105\3\0\1\106\3\0\1\107\2\0"+
    "\1\104\1\106\14\0\1\110\1\75\2\0\1\76\3\0"+
    "\1\77\3\0\1\100\1\101\16\0\1\102\4\0\1\104"+
    "\1\0\1\105\1\111\2\0\1\106\3\0\1\107\2\0"+
    "\1\104\1\106\13\0\1\112\1\0\1\75\2\0\1\76"+
    "\3\0\1\77\3\0\1\100\1\101\16\0\1\102\4\0"+
    "\1\104\1\113\1\105\3\0\1\106\3\0\1\107\2\0"+
    "\1\104\1\106\12\0\1\102\1\0\1\110\2\0\1\104"+
    "\1\0\1\105\3\0\1\106\3\0\1\107\2\0\1\104"+
    "\1\106\12\0\1\102\1\112\3\0\1\104\1\0\1\105"+
    "\3\0\1\106\3\0\1\107\2\0\1\104\1\106\10\0"+
    "\5\114\1\115\2\114\1\116\3\114\1\117\3\114\1\120"+
    "\1\121\6\114\1\12\5\114\5\122\1\123\2\122\1\124"+
    "\3\122\1\125\3\122\1\126\1\127\7\122\1\12\4\122"+
    "\5\0\1\75\2\0\1\76\3\0\1\77\3\0\1\100"+
    "\21\0\1\71\1\75\2\0\1\76\3\0\1\77\3\0"+
    "\1\100\20\0\1\73\1\0\1\75\2\0\1\76\3\0"+
    "\1\77\3\0\1\100\15\0\5\114\1\115\2\114\1\116"+
    "\3\114\1\117\3\114\1\120\7\114\1\12\5\114\5\122"+
    "\1\123\2\122\1\124\3\122\1\125\3\122\1\126\10\122"+
    "\1\12\4\122\2\0\5\60\1\0\5\60\1\0\10\60"+
    "\1\0\1\60\2\0\1\60\5\0\2\60\1\130\2\60"+
    "\1\0\5\60\1\0\10\60\1\0\1\60\2\0\1\60"+
    "\35\0\1\62\5\0\5\63\1\0\5\63\1\0\10\63"+
    "\1\0\1\63\2\0\1\63\5\0\2\63\1\131\2\63"+
    "\1\0\5\63\1\0\10\63\1\0\1\63\2\0\1\63"+
    "\35\0\1\65\5\0\5\66\1\0\5\66\1\0\10\66"+
    "\1\0\1\66\2\0\1\66\5\0\5\67\1\0\5\67"+
    "\1\0\10\67\1\0\1\67\2\0\1\67\7\0\1\132"+
    "\37\0\1\133\37\0\1\134\43\0\1\135\33\0\1\136"+
    "\43\0\1\137\24\0\1\140\37\0\1\141\44\0\1\142"+
    "\23\0\1\143\30\0\1\144\36\0\1\145\35\0\1\146"+
    "\43\0\1\147\26\0\1\150\42\0\1\151\33\0\1\152"+
    "\37\0\1\153\43\0\1\154\33\0\1\155\21\0\30\114"+
    "\1\12\27\114\1\156\5\114\1\12\16\114\1\157\16\114"+
    "\1\12\20\114\1\160\14\114\1\12\27\114\1\161\5\114"+
    "\1\12\15\114\1\162\17\114\1\12\5\114\31\122\1\12"+
    "\26\122\1\163\6\122\1\12\15\122\1\164\17\122\1\12"+
    "\17\122\1\165\15\122\1\12\26\122\1\166\6\122\1\12"+
    "\14\122\1\167\20\122\1\12\4\122\2\0\5\60\1\170"+
    "\5\60\1\0\6\60\1\171\1\60\1\0\1\60\2\0"+
    "\1\60\5\0\5\63\1\170\5\63\1\0\6\63\1\172"+
    "\1\63\1\0\1\63\2\0\1\63\10\0\1\173\43\0"+
    "\1\174\27\0\1\175\11\0\1\176\30\0\1\177\26\0"+
    "\1\200\42\0\1\201\40\0\1\202\42\0\1\203\37\0"+
    "\1\204\27\0\1\205\25\0\1\206\36\0\1\207\36\0"+
    "\1\210\37\0\1\211\43\0\1\212\33\0\1\213\34\0"+
    "\1\214\27\0\1\215\11\0\1\216\30\0\1\217\26\0"+
    "\1\220\32\0\10\114\1\221\17\114\1\12\20\114\1\222"+
    "\14\114\1\12\25\114\1\223\7\114\1\12\27\114\1\224"+
    "\5\114\1\12\21\114\1\225\13\114\1\12\5\114\10\122"+
    "\1\226\20\122\1\12\17\122\1\227\15\122\1\12\24\122"+
    "\1\230\10\122\1\12\26\122\1\231\6\122\1\12\20\122"+
    "\1\232\14\122\1\12\4\122\6\0\1\23\31\0\4\60"+
    "\1\233\1\0\5\60\1\0\10\60\1\0\1\60\2\0"+
    "\1\60\5\0\4\63\1\234\1\0\5\63\1\0\10\63"+
    "\1\0\1\63\2\0\1\63\11\0\1\235\41\0\1\236"+
    "\36\0\1\237\34\0\1\240\33\0\1\241\47\0\1\242"+
    "\30\0\1\243\7\0\1\243\14\0\1\244\53\0\1\245"+
    "\23\0\1\246\30\0\1\247\37\0\1\250\36\0\1\251"+
    "\42\0\1\252\27\0\1\253\11\0\1\254\30\0\1\255"+
    "\26\0\1\256\44\0\1\257\36\0\1\260\34\0\1\261"+
    "\33\0\1\262\47\0\1\263\13\0\15\114\1\264\7\114"+
    "\1\264\2\114\1\12\11\114\1\265\23\114\1\12\27\114"+
    "\1\266\5\114\1\12\15\114\1\267\17\114\1\12\10\114"+
    "\1\270\24\114\1\12\5\114\15\122\1\271\7\122\1\271"+
    "\3\122\1\12\10\122\1\272\24\122\1\12\26\122\1\273"+
    "\6\122\1\12\14\122\1\274\20\122\1\12\7\122\1\275"+
    "\25\122\1\12\4\122\7\0\1\276\14\0\1\276\13\0"+
    "\1\277\47\0\1\300\41\0\1\301\20\0\1\302\45\0"+
    "\1\303\35\0\1\304\22\0\2\305\5\0\1\305\5\0"+
    "\1\305\10\0\1\305\1\0\2\305\1\0\3\305\22\0"+
    "\1\306\20\0\1\307\52\0\1\310\21\0\1\311\36\0"+
    "\1\312\14\0\1\312\23\0\1\313\36\0\1\314\34\0"+
    "\1\315\33\0\1\316\47\0\1\317\15\0\1\320\47\0"+
    "\1\321\41\0\1\322\20\0\1\323\45\0\1\324\22\0"+
    "\13\114\1\325\14\114\1\12\5\114\2\326\5\114\1\326"+
    "\5\114\1\326\10\114\1\326\1\114\1\12\1\326\1\114"+
    "\3\326\22\114\1\327\5\114\1\12\12\114\1\330\22\114"+
    "\1\12\27\114\1\331\5\114\1\12\5\114\13\122\1\332"+
    "\15\122\1\12\4\122\2\333\5\122\1\333\5\122\1\333"+
    "\10\122\1\333\1\122\1\333\1\12\1\122\3\333\22\122"+
    "\1\334\6\122\1\12\11\122\1\335\23\122\1\12\26\122"+
    "\1\336\6\122\1\12\4\122\10\0\1\337\45\0\1\340"+
    "\20\0\1\341\53\0\1\303\22\0\1\342\27\0\2\343"+
    "\5\0\1\343\5\0\1\343\10\0\1\343\1\0\2\343"+
    "\1\0\3\343\10\0\1\344\45\0\1\345\30\0\1\346"+
    "\31\0\1\347\14\0\1\347\21\0\1\350\27\0\1\351"+
    "\47\0\1\352\41\0\1\353\20\0\1\354\45\0\1\355"+
    "\42\0\1\356\20\0\1\357\53\0\1\324\22\0\1\360"+
    "\31\0\5\23\1\0\5\23\1\0\10\23\1\0\1\23"+
    "\2\0\1\23\3\0\2\361\5\114\1\361\5\114\1\361"+
    "\10\114\1\361\1\114\1\12\1\361\1\114\3\361\10\114"+
    "\1\362\17\114\1\12\25\114\1\363\7\114\1\12\20\114"+
    "\1\364\14\114\1\12\5\114\2\365\5\122\1\365\5\122"+
    "\1\365\10\122\1\365\1\122\1\365\1\12\1\122\3\365"+
    "\10\122\1\366\20\122\1\12\24\122\1\367\10\122\1\12"+
    "\17\122\1\370\15\122\1\12\4\122\4\0\1\303\36\0"+
    "\1\242\42\0\1\242\32\0\1\371\14\0\1\371\16\0"+
    "\1\372\36\0\1\373\40\0\1\374\14\0\1\346\6\0"+
    "\1\346\10\0\1\375\31\0\1\324\51\0\1\376\20\0"+
    "\1\377\53\0\1\355\22\0\1\u0100\34\0\1\263\42\0"+
    "\1\263\32\0\1\u0101\14\0\1\u0101\11\0\5\114\1\u0102"+
    "\22\114\1\12\13\114\1\u0103\21\114\1\12\16\114\1\u0104"+
    "\14\114\1\364\1\114\1\12\4\114\1\364\5\122\1\u0105"+
    "\23\122\1\12\12\122\1\u0106\22\122\1\12\15\122\1\u0107"+
    "\14\122\1\370\2\122\1\12\3\122\1\370\4\0\1\242"+
    "\51\0\1\u0108\30\0\1\u0109\34\0\1\u010a\27\0\1\355"+
    "\36\0\1\317\42\0\1\317\32\0\1\u010b\14\0\1\u010b"+
    "\15\0\1\263\31\0\20\114\1\u010c\7\114\1\12\20\114"+
    "\1\u010d\14\114\1\12\17\114\1\u010e\15\114\1\12\5\114"+
    "\20\122\1\u010f\10\122\1\12\17\122\1\u0110\15\122\1\12"+
    "\16\122\1\u0111\16\122\1\12\4\122\6\0\1\u0112\27\0"+
    "\2\u0113\5\0\1\u0113\5\0\1\u0113\10\0\1\u0113\1\0"+
    "\2\u0113\1\0\3\u0113\10\0\1\u0114\31\0\1\317\31\0"+
    "\6\114\1\u0115\21\114\1\12\5\114\2\u0116\5\114\1\u0116"+
    "\5\114\1\u0116\10\114\1\u0116\1\114\1\12\1\u0116\1\114"+
    "\3\u0116\10\114\1\u0117\17\114\1\12\5\114\6\122\1\u0118"+
    "\22\122\1\12\4\122\2\u0119\5\122\1\u0119\5\122\1\u0119"+
    "\10\122\1\u0119\1\122\1\u0119\1\12\1\122\3\u0119\10\122"+
    "\1\u011a\20\122\1\12\4\122\13\0\1\u011b\27\0\1\u011c"+
    "\30\0\13\114\1\u011d\14\114\1\12\12\114\1\u011e\22\114"+
    "\1\12\5\114\13\122\1\u011f\15\122\1\12\11\122\1\u0120"+
    "\23\122\1\12\4\122\2\u0121\5\0\1\u0121\5\0\1\u0121"+
    "\10\0\1\u0121\1\0\2\u0121\1\0\3\u0121\13\0\1\u0122"+
    "\22\0\2\u0123\5\114\1\u0123\5\114\1\u0123\10\114\1\u0123"+
    "\1\114\1\12\1\u0123\1\114\3\u0123\13\114\1\u0124\14\114"+
    "\1\12\5\114\2\u0125\5\122\1\u0125\5\122\1\u0125\10\122"+
    "\1\u0125\1\122\1\u0125\1\12\1\122\3\u0125\13\122\1\u0126"+
    "\15\122\1\12\4\122\14\0\1\u0127\21\0\14\114\1\u0128"+
    "\13\114\1\12\5\114\14\122\1\u0129\14\122\1\12\4\122"+
    "\3\0\1\u012a\32\0\3\114\1\u012b\24\114\1\12\5\114"+
    "\3\122\1\u012c\25\122\1\12\4\122\12\0\1\u012d\23\0"+
    "\12\114\1\u012e\15\114\1\12\5\114\12\122\1\u012f\16\122"+
    "\1\12\4\122\13\0\1\u0130\22\0\13\114\1\u0131\14\114"+
    "\1\12\5\114\13\122\1\u0132\15\122\1\12\4\122\2\23"+
    "\5\0\1\23\5\0\1\23\10\0\1\23\1\0\2\23"+
    "\1\0\3\23\2\u0133\5\114\1\u0133\5\114\1\u0133\10\114"+
    "\1\u0133\1\114\1\12\1\u0133\1\114\3\u0133\2\u0134\5\122"+
    "\1\u0134\5\122\1\u0134\10\122\1\u0134\1\122\1\u0134\1\12"+
    "\1\122\3\u0134";

  private static int [] zzUnpackTrans() {
    int [] result = new int[8250];
    int offset = 0;
    offset = zzUnpackTrans(ZZ_TRANS_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackTrans(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      value--;
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }


  /* error codes */
  private static final int ZZ_UNKNOWN_ERROR = 0;
  private static final int ZZ_NO_MATCH = 1;
  private static final int ZZ_PUSHBACK_2BIG = 2;

  /* error messages for the codes above */
  private static final String ZZ_ERROR_MSG[] = {
    "Unknown internal scanner error",
    "Error: could not match input",
    "Error: pushback value was too large"
  };

  /**
   * ZZ_ATTRIBUTE[aState] contains the attributes of state <code>aState</code>
   */
  private static final int [] ZZ_ATTRIBUTE = zzUnpackAttribute();

  private static final String ZZ_ATTRIBUTE_PACKED_0 =
    "\11\0\2\11\5\1\3\11\44\1\40\0\2\1\37\0"+
    "\2\1\40\0\2\1\46\0\1\11\1\0\1\11\16\0"+
    "\1\1\1\0\1\1\4\0\1\1\7\0\1\11\11\0"+
    "\1\1\3\0\1\1\3\0\1\1\35\0\1\11\2\0"+
    "\1\1\2\0\1\1\7\0\1\11\1\0\1\1\1\0"+
    "\1\1\15\0\2\1";

  private static int [] zzUnpackAttribute() {
    int [] result = new int[308];
    int offset = 0;
    offset = zzUnpackAttribute(ZZ_ATTRIBUTE_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackAttribute(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }

  /** the input device */
  private java.io.Reader zzReader;

  /** the current state of the DFA */
  private int zzState;

  /** the current lexical state */
  private int zzLexicalState = YYINITIAL;

  /** this buffer contains the current text to be matched and is
      the source of the yytext() string */
  private char zzBuffer[] = new char[ZZ_BUFFERSIZE];

  /** the textposition at the last accepting state */
  private int zzMarkedPos;

  /** the current text position in the buffer */
  private int zzCurrentPos;

  /** startRead marks the beginning of the yytext() string in the buffer */
  private int zzStartRead;

  /** endRead marks the last character in the buffer, that has been read
      from input */
  private int zzEndRead;

  /** number of newlines encountered up to the start of the matched text */
  private int yyline;

  /** the number of characters up to the start of the matched text */
  private int yychar;

  /**
   * the number of characters from the last newline up to the start of the 
   * matched text
   */
  private int yycolumn;

  /** 
   * zzAtBOL == true <=> the scanner is currently at the beginning of a line
   */
  private boolean zzAtBOL = true;

  /** zzAtEOF == true <=> the scanner is at the EOF */
  private boolean zzAtEOF;

  /** denotes if the user-EOF-code has already been executed */
  private boolean zzEOFDone;
  
  /** 
   * The number of occupied positions in zzBuffer beyond zzEndRead.
   * When a lead/high surrogate has been read from the input stream
   * into the final zzBuffer position, this will have a value of 1;
   * otherwise, it will have a value of 0.
   */
  private int zzFinalHighSurrogate = 0;

  /* user code: */
	String location = "MAIN PROGRAM";
	Map<String, String> memory = new HashMap<String, String>();
	Map<String, String> files  = new HashMap<String, String>();
	Map<String, Integer> lines = new HashMap<String, Integer>();
	List<String> errors = new LinkedList<String>();
	
	public COMDESIGNAlloc(){
	}
	
	@Override
	public void setInputFile(IPath file) throws FileNotFoundException {
		super.setInputFile(file);
		this.zzReader = new FileReader(file.toOSString());
	}
	
	/**
     * Method used to throw errors due to allocate or open, not followed by a
     * deallocate or a close.
     **/
    public void raiseRemainingErrors() throws JFlexException {
        Iterator<Entry<String, String>> iterator =
                this.memory.entrySet().iterator();
        while (iterator.hasNext()) {
            final Map.Entry<String, String> pairs = iterator.next();
            this.setError(pairs.getValue(),"The resource named "+
            		pairs.getKey() + " has not been allocated and deallocate in the same algorithmic level.",
                    this.lines.get(pairs.getKey()));
            iterator.remove();
        }

        iterator = this.files.entrySet().iterator();
        while (iterator.hasNext()) {
            final Map.Entry<String, String> pairs = iterator.next();
            this.setError(pairs.getValue(),"The resource named "+
            		pairs.getKey() + " has not been allocated and deallocate in the same algorithmic level.",
                    this.lines.get(pairs.getKey()));
            iterator.remove();
        }
    }
	
	/**
     * Sort all violations.
     **/
    public void sortResults() {
        Collections.sort(getViolations(), new Comparator<Violation>() {
            @Override
            public int compare(final Violation viol1, final Violation viol2) {
                int res = viol1.getRuleId().compareTo(viol2.getRuleId());
                if (res == 0) {
                    res =
                            viol1.getFilePath()
                                    .toFile()
                                    .getName()
                                    .compareTo(
                                            viol2.getFilePath().toFile()
                                                    .getName());
                    if (res == 0) {
                        res = viol1.getLine().compareTo(viol2.getLine());
                        if (res == 0) {
                            res =
                                    viol1.getLocation().compareTo(
                                            viol2.getLocation());
                        }
                    }
                }
                return res;
            }
        });
    }
		


  /**
   * Creates a new scanner
   *
   * @param   in  the java.io.Reader to read input from.
   */
  public COMDESIGNAlloc(java.io.Reader in) {
    this.zzReader = in;
  }


  /** 
   * Unpacks the compressed character translation table.
   *
   * @param packed   the packed character translation table
   * @return         the unpacked character translation table
   */
  private static char [] zzUnpackCMap(String packed) {
    char [] map = new char[0x110000];
    int i = 0;  /* index in packed string  */
    int j = 0;  /* index in unpacked array */
    while (i < 172) {
      int  count = packed.charAt(i++);
      char value = packed.charAt(i++);
      do map[j++] = value; while (--count > 0);
    }
    return map;
  }


  /**
   * Refills the input buffer.
   *
   * @return      <code>false</code>, iff there was new input.
   * 
   * @exception   java.io.IOException  if any I/O-Error occurs
   */
  private boolean zzRefill() throws java.io.IOException {

    /* first: make room (if you can) */
    if (zzStartRead > 0) {
      zzEndRead += zzFinalHighSurrogate;
      zzFinalHighSurrogate = 0;
      System.arraycopy(zzBuffer, zzStartRead,
                       zzBuffer, 0,
                       zzEndRead-zzStartRead);

      /* translate stored positions */
      zzEndRead-= zzStartRead;
      zzCurrentPos-= zzStartRead;
      zzMarkedPos-= zzStartRead;
      zzStartRead = 0;
    }

    /* is the buffer big enough? */
    if (zzCurrentPos >= zzBuffer.length - zzFinalHighSurrogate) {
      /* if not: blow it up */
      char newBuffer[] = new char[zzBuffer.length*2];
      System.arraycopy(zzBuffer, 0, newBuffer, 0, zzBuffer.length);
      zzBuffer = newBuffer;
      zzEndRead += zzFinalHighSurrogate;
      zzFinalHighSurrogate = 0;
    }

    /* fill the buffer with new input */
    int requested = zzBuffer.length - zzEndRead;
    int numRead = zzReader.read(zzBuffer, zzEndRead, requested);

    /* not supposed to occur according to specification of java.io.Reader */
    if (numRead == 0) {
      throw new java.io.IOException("Reader returned 0 characters. See JFlex examples for workaround.");
    }
    if (numRead > 0) {
      zzEndRead += numRead;
      /* If numRead == requested, we might have requested to few chars to
         encode a full Unicode character. We assume that a Reader would
         otherwise never return half characters. */
      if (numRead == requested) {
        if (Character.isHighSurrogate(zzBuffer[zzEndRead - 1])) {
          --zzEndRead;
          zzFinalHighSurrogate = 1;
        }
      }
      /* potentially more input available */
      return false;
    }

    /* numRead < 0 ==> end of stream */
    return true;
  }

    
  /**
   * Closes the input stream.
   */
  public final void yyclose() throws java.io.IOException {
    zzAtEOF = true;            /* indicate end of file */
    zzEndRead = zzStartRead;  /* invalidate buffer    */

    if (zzReader != null)
      zzReader.close();
  }


  /**
   * Resets the scanner to read from a new input stream.
   * Does not close the old reader.
   *
   * All internal variables are reset, the old input stream 
   * <b>cannot</b> be reused (internal buffer is discarded and lost).
   * Lexical state is set to <tt>ZZ_INITIAL</tt>.
   *
   * Internal scan buffer is resized down to its initial length, if it has grown.
   *
   * @param reader   the new input stream 
   */
  public final void yyreset(java.io.Reader reader) {
    zzReader = reader;
    zzAtBOL  = true;
    zzAtEOF  = false;
    zzEOFDone = false;
    zzEndRead = zzStartRead = 0;
    zzCurrentPos = zzMarkedPos = 0;
    zzFinalHighSurrogate = 0;
    yyline = yychar = yycolumn = 0;
    zzLexicalState = YYINITIAL;
    if (zzBuffer.length > ZZ_BUFFERSIZE)
      zzBuffer = new char[ZZ_BUFFERSIZE];
  }


  /**
   * Returns the current lexical state.
   */
  public final int yystate() {
    return zzLexicalState;
  }


  /**
   * Enters a new lexical state
   *
   * @param newState the new lexical state
   */
  public final void yybegin(int newState) {
    zzLexicalState = newState;
  }


  /**
   * Returns the text matched by the current regular expression.
   */
  public final String yytext() {
    return new String( zzBuffer, zzStartRead, zzMarkedPos-zzStartRead );
  }


  /**
   * Returns the character at position <tt>pos</tt> from the 
   * matched text. 
   * 
   * It is equivalent to yytext().charAt(pos), but faster
   *
   * @param pos the position of the character to fetch. 
   *            A value from 0 to yylength()-1.
   *
   * @return the character at position pos
   */
  public final char yycharat(int pos) {
    return zzBuffer[zzStartRead+pos];
  }


  /**
   * Returns the length of the matched text region.
   */
  public final int yylength() {
    return zzMarkedPos-zzStartRead;
  }


  /**
   * Reports an error that occured while scanning.
   *
   * In a wellformed scanner (no or only correct usage of 
   * yypushback(int) and a match-all fallback rule) this method 
   * will only be called with things that "Can't Possibly Happen".
   * If this method is called, something is seriously wrong
   * (e.g. a JFlex bug producing a faulty scanner etc.).
   *
   * Usual syntax/scanner level error handling should be done
   * in error fallback rules.
   *
   * @param   errorCode  the code of the errormessage to display
   */
  private void zzScanError(int errorCode) {
    String message;
    try {
      message = ZZ_ERROR_MSG[errorCode];
    }
    catch (ArrayIndexOutOfBoundsException e) {
      message = ZZ_ERROR_MSG[ZZ_UNKNOWN_ERROR];
    }

    throw new Error(message);
  } 


  /**
   * Pushes the specified amount of characters back into the input stream.
   *
   * They will be read again by then next call of the scanning method
   *
   * @param number  the number of characters to be read again.
   *                This number must not be greater than yylength()!
   */
  public void yypushback(int number)  {
    if ( number > yylength() )
      zzScanError(ZZ_PUSHBACK_2BIG);

    zzMarkedPos -= number;
  }


  /**
   * Resumes scanning until the next regular expression is matched,
   * the end of input is encountered or an I/O-Error occurs.
   *
   * @return      the next token
   * @exception   java.io.IOException  if any I/O-Error occurs
   */
  public List<Violation> run() throws java.io.IOException, JFlexException {
    int zzInput;
    int zzAction;

    // cached fields:
    int zzCurrentPosL;
    int zzMarkedPosL;
    int zzEndReadL = zzEndRead;
    char [] zzBufferL = zzBuffer;
    char [] zzCMapL = ZZ_CMAP;

    int [] zzTransL = ZZ_TRANS;
    int [] zzRowMapL = ZZ_ROWMAP;
    int [] zzAttrL = ZZ_ATTRIBUTE;

    while (true) {
      zzMarkedPosL = zzMarkedPos;

      boolean zzR = false;
      int zzCh;
      int zzCharCount;
      for (zzCurrentPosL = zzStartRead  ;
           zzCurrentPosL < zzMarkedPosL ;
           zzCurrentPosL += zzCharCount ) {
        zzCh = Character.codePointAt(zzBufferL, zzCurrentPosL, zzMarkedPosL);
        zzCharCount = Character.charCount(zzCh);
        switch (zzCh) {
        case '\u000B':
        case '\u000C':
        case '\u0085':
        case '\u2028':
        case '\u2029':
          yyline++;
          zzR = false;
          break;
        case '\r':
          yyline++;
          zzR = true;
          break;
        case '\n':
          if (zzR)
            zzR = false;
          else {
            yyline++;
          }
          break;
        default:
          zzR = false;
        }
      }

      if (zzR) {
        // peek one character ahead if it is \n (if we have counted one line too much)
        boolean zzPeek;
        if (zzMarkedPosL < zzEndReadL)
          zzPeek = zzBufferL[zzMarkedPosL] == '\n';
        else if (zzAtEOF)
          zzPeek = false;
        else {
          boolean eof = zzRefill();
          zzEndReadL = zzEndRead;
          zzMarkedPosL = zzMarkedPos;
          zzBufferL = zzBuffer;
          if (eof) 
            zzPeek = false;
          else 
            zzPeek = zzBufferL[zzMarkedPosL] == '\n';
        }
        if (zzPeek) yyline--;
      }
      zzAction = -1;

      zzCurrentPosL = zzCurrentPos = zzStartRead = zzMarkedPosL;
  
      zzState = ZZ_LEXSTATE[zzLexicalState];

      // set up zzAction for empty match case:
      int zzAttributes = zzAttrL[zzState];
      if ( (zzAttributes & 1) == 1 ) {
        zzAction = zzState;
      }


      zzForAction: {
        while (true) {
    
          if (zzCurrentPosL < zzEndReadL) {
            zzInput = Character.codePointAt(zzBufferL, zzCurrentPosL, zzEndReadL);
            zzCurrentPosL += Character.charCount(zzInput);
          }
          else if (zzAtEOF) {
            zzInput = YYEOF;
            break zzForAction;
          }
          else {
            // store back cached positions
            zzCurrentPos  = zzCurrentPosL;
            zzMarkedPos   = zzMarkedPosL;
            boolean eof = zzRefill();
            // get translated positions and possibly new buffer
            zzCurrentPosL  = zzCurrentPos;
            zzMarkedPosL   = zzMarkedPos;
            zzBufferL      = zzBuffer;
            zzEndReadL     = zzEndRead;
            if (eof) {
              zzInput = YYEOF;
              break zzForAction;
            }
            else {
              zzInput = Character.codePointAt(zzBufferL, zzCurrentPosL, zzEndReadL);
              zzCurrentPosL += Character.charCount(zzInput);
            }
          }
          int zzNext = zzTransL[ zzRowMapL[zzState] + zzCMapL[zzInput] ];
          if (zzNext == -1) break zzForAction;
          zzState = zzNext;

          zzAttributes = zzAttrL[zzState];
          if ( (zzAttributes & 1) == 1 ) {
            zzAction = zzState;
            zzMarkedPosL = zzCurrentPosL;
            if ( (zzAttributes & 8) == 8 ) break zzForAction;
          }

        }
      }

      // store back cached position
      zzMarkedPos = zzMarkedPosL;

      if (zzInput == YYEOF && zzStartRead == zzCurrentPos) {
        zzAtEOF = true;
          { 	raiseRemainingErrors();
	sortResults();
	return getViolations();
 }
      }
      else {
        switch (zzAction < 0 ? zzAction : ZZ_ACTION[zzAction]) {
          case 1: 
            { yybegin(LINE);
            }
          case 16: break;
          case 2: 
            { yybegin(COMMENT);
            }
          case 17: break;
          case 3: 
            { yybegin(NEW_LINE);
            }
          case 18: break;
          case 4: 
            { throw new JFlexException( new Exception("Illegal character <" + yytext() + ">") );
            }
          case 19: break;
          case 5: 
            { 
            }
          case 20: break;
          case 6: 
            { location = location + " " + yytext(); 
							 yybegin(COMMENT);
            }
          case 21: break;
          case 7: 
            { files.put(yytext(), location); 
							 lines.put(yytext(), yyline+1);
							 yybegin(COMMENT);
            }
          case 22: break;
          case 8: 
            { String loc = files.get(yytext());
							 if (!location.equals(loc) && !errors.contains(yytext())) {
								setError(location,"The resource named "+
            							 yytext() + " has not been allocated and deallocate in the same algorithmic level.", yyline+1);
							 }
							 if (loc != null) {
								errors.add(yytext());
							 }
							 files.remove(yytext()); 
							 yybegin(COMMENT);
            }
          case 23: break;
          case 9: 
            { memory.put(yytext(), location); 
							 lines.put(yytext(), yyline+1);
							 yybegin(COMMENT);
            }
          case 24: break;
          case 10: 
            { String loc = memory.get(yytext());
							 if (!location.equals(loc) && !errors.contains(yytext())) {
								setError(location,"The resource named "+
            							 yytext() + " has not been allocated and deallocate in the same algorithmic level.", yyline+1);
							 }
							 if (loc != null) {
								errors.add(yytext());
							 }
							 memory.remove(yytext()); 
							 yybegin(COMMENT);
            }
          case 25: break;
          case 11: 
            { location = yytext(); 
							 yybegin(NAMING);
            }
          case 26: break;
          case 12: 
            { yybegin(OPEN);
            }
          case 27: break;
          case 13: 
            { yybegin(CLOSE);
            }
          case 28: break;
          case 14: 
            { yybegin(ALLOC);
            }
          case 29: break;
          case 15: 
            { yybegin(DEALLOC);
            }
          case 30: break;
          default:
            zzScanError(ZZ_NO_MATCH);
        }
      }
    }
  }


}