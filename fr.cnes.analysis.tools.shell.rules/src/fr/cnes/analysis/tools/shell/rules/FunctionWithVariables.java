
/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/
package fr.cnes.analysis.tools.shell.rules;

import java.util.ArrayList;

import fr.cnes.analysis.tools.shell.metrics.Function;

/**
 * This class can be used to handle functions and their local and global variables.
 */
public class FunctionWithVariables extends Function {

    /* localVariables: the list of local variables in the function */
    private ArrayList<String> localVariables = null;
    /* globalVariables: the list of global variables in the function */
    private ArrayList<String> globalVariables = null;

    /**
     * @param pName
     *            Function's name
     * @param pBeginLine
     *            Function's line
     * @param pStarter
     *            Function's starter.
     */
	public FunctionWithVariables(String pName, int pBeginLine, String pStarter) {
		super(pName, pBeginLine, pStarter);
		this.localVariables  = new ArrayList<String>();
		this.globalVariables = new ArrayList<String>();
	}
		
    public ArrayList<String> getLocalVariables() {
        return this.localVariables;
    }

	public ArrayList<String> getGlobalVariables() {
        return this.globalVariables;
    }

}
