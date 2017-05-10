/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */ 
package fr.cnes.analysis.tools.analyzer.datas;

import java.io.File;

import org.eclipse.core.runtime.IPath;

/**
 * This class represents one rule violation on a file. It contains the rule
 * name, rule id (for preferences use), file's path, violation location and
 * line.
 */
public class Violation implements Cloneable {
    /** Rule's name. **/
    private String ruleName;
    /** Rule's id. **/
    private String ruleId;
    
    /** The analyzed file. **/
    private File file;
    /** Violation location, such as method, function, etc. **/
    private String location;
    
    /** Violation message */
    private String message;
   
    /** Violation's line in the file. **/
    private Integer line;

    /**
     * Constructor with every attribute instantiated.
     * 
     * @param pRuleName
     *            rule's name
     * @param pRuleId
     *            rule's id
     * @param pFile
     *            analyzed file
     * @param pLocation
     *            violation's location
     * @param pLine
     *            violation's line
     */
    public Violation(final String pRuleName, final String pRuleId,
            final File pFile, final String pLocation, final String pMessage, final Integer pLine) {
        this.ruleName = pRuleName;
        this.ruleId = pRuleId;
        this.file = pFile;
        this.location = pLocation;
        this.message = pMessage;
        this.line = pLine;
    }

    /**
     * Constructor with rule's name, rule's id and file's path.
     * 
     * @param pRuleName
     *            the rule's name
     * @param pRuleId
     *            the rule's id
     * @param pFile
     *            the file analyzed
     */
    public Violation(final String pRuleName, final String pRuleId,
            final File pFile) {
        this.ruleName = pRuleName;
        this.ruleId = pRuleId;
        this.file = pFile;
        this.location = "";
        this.message = "";
        this.line = Integer.MIN_VALUE;
    }

    /**
     * Getter for the violation's message
     * 
     * @return the violation's message
     */
    public String getMessage() {
		return message;
	}

	/**
	 * Setter for the violation's message;
	 * 
	 * @param message the new message
	 */
	public void setMessage(String message) {
		this.message = message;
	}

	/**
     * Getter for rule's name.
     * 
     * @return the ruleName
     */
    public String getRuleName() {
        return this.ruleName;
    }

    /**
     * Getter for rule's id.
     * 
     * @return the ruleId
     */
    public String getRuleId() {
        return this.ruleId;
    }

    /**
     * Getter for file's path.
     * 
     * @return the filePath
     */
    public File getFile() {
        return this.file;
    }

    /**
     * Getter for violation's location.
     * 
     * @return the location
     */
    public String getLocation() {
        return this.location;
    }

    /**
     * Getter for violation's line.
     * 
     * @return the line
     */
    public Integer getLine() {
        return this.line;
    }

    /**
     * Setter for rule's name.
     * 
     * @param pRuleName
     *            the ruleName to set
     */
    public void setRuleName(final String pRuleName) {
        this.ruleName = pRuleName;
    }

    /**
     * Setter for rule's id.
     * 
     * @param pRuleId
     *            the ruleId to set
     */
    public void setRuleId(final String pRuleId) {
        this.ruleId = pRuleId;
    }

    /**
     * Setter for file.
     * 
     * @param pFile
     *            the file to set
     */
    public void setFile(final File pFile) {
        this.file = pFile;
    }

    /**
     * Setter for violation's location.
     * 
     * @param pLocation
     *            the location to set
     */
    public void setLocation(final String pLocation) {
        this.location = pLocation;
    }

    /**
     * Setter for violation's line.
     * 
     * @param pLine
     *            the line to set
     */
    public void setLine(final Integer pLine) {
        this.line = pLine;
    }

    /*
     * (non-Javadoc)
     * @see java.lang.Object#clone()
     */
    @Override
    public Violation clone() throws CloneNotSupportedException {
        final Violation violation = (Violation) super.clone();
        violation.setRuleName(this.getRuleName());
        violation.setRuleId(this.getRuleId());
        violation.setFile(this.getFile());
        violation.setLocation(this.getLocation());
        violation.setMessage(this.getMessage());
        violation.setLine(this.getLine());
        return violation;
    }

    /*
     * (non-Javadoc)
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(final Object object) {
        return (object instanceof Violation)
                && this.ruleId.equals(((Violation) object).getRuleId())
                && this.file.equals(((Violation) object).getFile())
                && this.location.equals(((Violation) object).getLocation())
                && this.message.equals(((Violation)object).getMessage())
                && (this.line.equals(((Violation) object).getLine()));
    }

    /*
     * (non-Javadoc)
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        assert false : "hashCode not designed";
        return this.line;
    }
}
