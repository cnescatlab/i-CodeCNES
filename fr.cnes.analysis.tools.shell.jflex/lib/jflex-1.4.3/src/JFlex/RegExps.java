/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * JFlex 1.4.3                                                             *
 * Copyright (C) 1998-2009  Gerwin Klein <lsf@jflex.de>                    *
 * All rights reserved.                                                    *
 *                                                                         *
 * This program is free software; you can redistribute it and/or modify    *
 * it under the terms of the GNU General Public License. See the file      *
 * COPYRIGHT for more information.                                         *
 *                                                                         *
 * This program is distributed in the hope that it will be useful,         *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
 * GNU General Public License for more details.                            *
 *                                                                         *
 * You should have received a copy of the GNU General Public License along *
 * with this program; if not, write to the Free Software Foundation, Inc., *
 * 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA                 *
 *                                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
package JFlex;

import java.util.*;


/**
 * Stores all rules of the specification for later access in RegExp -> NFA
 *
 * @author Gerwin Klein
 * @version JFlex 1.4.3, $Revision: 433 $, $Date: 2009-01-31 19:52:34 +1100 (Sat, 31 Jan 2009) $
 */
public class RegExps {
  
  /** the spec line in which a regexp is used */
  Vector /* of Integer */ lines;

  /** the lexical states in wich the regexp is used */
  Vector /* of Vector of Integer */ states;

  /** the regexp */
  Vector /* of RegExp */ regExps;

  /** the action of a regexp */
  Vector /* of Action */ actions;
  
  /** flag if it is a BOL regexp */
  Vector /* of Boolean */ BOL;

  /** the lookahead expression */
  Vector /* of RegExp */ look;

  /** the forward DFA entry point of the lookahead expression */
  Vector /* of Integer */ look_entry;

  /** Count of many general lookahead expressions there are. 
   *  Need 2*gen_look_count additional DFA entry points. */
  int gen_look_count;

  public RegExps() {
    states = new Vector();
    regExps = new Vector();
    actions = new Vector();
    BOL = new Vector();
    look = new Vector();
    lines = new Vector();
    look_entry = new Vector();
  }

  public int insert(int line, Vector stateList, RegExp regExp, Action action, 
                     Boolean isBOL, RegExp lookAhead) {      
    if (Options.DEBUG) {
      Out.debug("Inserting regular expression with statelist :"+Out.NL+stateList);  //$NON-NLS-1$
      Out.debug("and action code :"+Out.NL+action.content+Out.NL);     //$NON-NLS-1$
      Out.debug("expression :"+Out.NL+regExp);  //$NON-NLS-1$
    }

    states.addElement(stateList);
    regExps.addElement(regExp);
    actions.addElement(action);
    BOL.addElement(isBOL);
    look.addElement(lookAhead);
    lines.addElement(new Integer(line));
    look_entry.addElement(null);
    
    return states.size()-1;
  }

  public int insert(Vector stateList, Action action) {

    if (Options.DEBUG) {
      Out.debug("Inserting eofrule with statelist :"+Out.NL+stateList);   //$NON-NLS-1$
      Out.debug("and action code :"+Out.NL+action.content+Out.NL);      //$NON-NLS-1$
    }

    states.addElement(stateList);
    regExps.addElement(null);
    actions.addElement(action);
    BOL.addElement(null);
    look.addElement(null);
    lines.addElement(null);
    look_entry.addElement(null);
    
    return states.size()-1;
  }

  public void addStates(int regNum, Vector newStates) {
    Enumeration s = newStates.elements();
    
    while (s.hasMoreElements()) 
      ((Vector)states.elementAt(regNum)).addElement(s.nextElement());      
  }

  public int getNum() {
    return states.size();
  }

  public boolean isBOL(int num) {
    return ((Boolean) BOL.elementAt(num)).booleanValue();
  }
  
  public RegExp getLookAhead(int num) {
    return (RegExp) look.elementAt(num);
  }

  public boolean isEOF(int num) {
    return BOL.elementAt(num) == null;
  }

  public Vector getStates(int num) {
    return (Vector) states.elementAt(num);
  }

  public RegExp getRegExp(int num) {
    return (RegExp) regExps.elementAt(num);
  }

  public int getLine(int num) {
    return ((Integer) lines.elementAt(num)).intValue();
  }
  
  public int getLookEntry(int num) {
    return ((Integer) look_entry.elementAt(num)).intValue();
  }

  public void checkActions() {
    if ( actions.elementAt(actions.size()-1) == null ) {
      Out.error(ErrorMessages.NO_LAST_ACTION);
      throw new GeneratorException();
    }
  }

  public Action getAction(int num) {
    while ( num < actions.size() && actions.elementAt(num) == null )
      num++;

    return (Action) actions.elementAt(num);
  }

  public int NFASize(Macros macros) {
    int size = 0;
    Enumeration e = regExps.elements();
    while (e.hasMoreElements()) {
      RegExp r = (RegExp) e.nextElement();
      if (r != null) size += r.size(macros);
    }
    e = look.elements();
    while (e.hasMoreElements()) {
      RegExp r = (RegExp) e.nextElement();
      if (r != null) size += r.size(macros);
    }
    return size;
  }

  public void checkLookAheads() {
    for (int i=0; i < regExps.size(); i++) 
      lookAheadCase(i);
  }
  
  /**
   * Determine which case of lookahead expression regExpNum points to (if any).
   * Set case data in corresponding action.
   * Increment count of general lookahead expressions for entry points
   * of the two additional DFAs.
   * Register DFA entry point in RegExps
   *
   * Needs to be run before adding any regexps/rules to be able to reserve
   * the correct amount of space of lookahead DFA entry points.
   * 
   * @param regExpNum   the number of the regexp in RegExps. 
   */
  private void lookAheadCase(int regExpNum) {
    if ( getLookAhead(regExpNum) != null ) {
      RegExp r1 = getRegExp(regExpNum);
      RegExp r2 = getLookAhead(regExpNum);

      Action a = getAction(regExpNum);
            
      int len1 = SemCheck.length(r1);
      int len2 = SemCheck.length(r2);
      
      if (len1 >= 0) {
        a.setLookAction(Action.FIXED_BASE,len1);
      }
      else if (len2 >= 0) {
        a.setLookAction(Action.FIXED_LOOK,len2);
      }
      else if (SemCheck.isFiniteChoice(r2)) {
        a.setLookAction(Action.FINITE_CHOICE,0);
      }
      else {
        a.setLookAction(Action.GENERAL_LOOK,0);
        look_entry.setElementAt(new Integer(gen_look_count), regExpNum);
        gen_look_count++;
      }
    }
  }

}
