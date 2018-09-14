package fr.cnes.analysis.tools.shell.rules;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This class is added to handle functions and their local and global variables for the COMDATAInvariant rule.
 * The COMDATAInvariant rule uses a hash table per function of variables that should be constants, and
 * a list per function of variables that are either declared as constants or have been assigned more than once.
 */

public class FunctionInvariant extends FunctionWithVariables {
	
	/* okVariables: contains all variables that have either been declared as consts or */
	/* should be variables. There should be no violation on them. */
	private ArrayList<String> okVariables;
	
	/* okGlobalVariables: contains all global to the function variables that should be variables. 
	 * There should be no violation on them. */
	private ArrayList<String> okGlobalVariables;

	/* errVariables: contains all variables that for the moment should be consts */
	/* String: variable name, Integer: variable line in code */
	private HashMap<String,Integer> errVariables;
	
	public FunctionInvariant(String pName, int pBeginLine, String pStarter) {
		super(pName, pBeginLine, pStarter);
		this.okVariables  = new ArrayList<String>();
		this.errVariables = new HashMap<String,Integer>();
		this.okGlobalVariables = new ArrayList<String>();
	}
	
	public HashMap<String,Integer> getErrVariables() {
		return this.errVariables;
	}
	
	public ArrayList<String> getOkVariables() {
		return this.okVariables;
	}
	
	public ArrayList<String> getOkGlobalVariables() {
		return this.okGlobalVariables;
	}
	
	/* update ok and err variables with variable status from son function */
	public void addSonOkVariables(ArrayList<String> sonOkVariables)	{
		for (String var : sonOkVariables) {
			if (this.errVariables.containsKey(var)) {
				this.errVariables.remove(var);
				this.okVariables.add(var);
			}
		}
	}
	
}
	
