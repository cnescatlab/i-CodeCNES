/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor;

import org.eclipse.core.runtime.IPath;

/**
 * Descriptor for a violation that is intended to be shown.</br>
 * This descriptor would return the violation message {@link #name} and the line
 * of the violation in the file using {@link #getName()} and {@link #getValue()}
 * when instanced in a {@link IFileRuleDescriptor}.</br>
 * 
 * Descriptors contained by this descriptor are {@link ViolationDescriptor}.
 * 
 * @see IFileRuleDescriptor
 * @see FileRuleDescriptor
 * @see RuleDescriptor
 * @see ViolationDescriptor
 * @version 2.1
 * @since 2.0
 */
public class ViolationDescriptor implements IFileRuleDescriptor, Cloneable {
    /** Rule's id. **/
    private String ruleId;
    /** Violation's message. **/
    private String message;
    /** Function containing the violation. **/
    private String location;
    /** Line of the violation. **/
    private Integer line;
    /** The path of the file containing the violation. */
    private IPath filePath;

    /**
     * Constructor for Violation Descriptor
     * 
     * @param pRuleId
     *            The rule violated
     * @param pMessage
     *            The violation's message
     * @param pLocation
     *            The function where is violation is located
     * @param pLine
     *            The line of the violation
     * @param pFilePath
     *            The filePath to the violation
     */
    public ViolationDescriptor(final String pRuleId, final String pLocation, final String pMessage,
            final Integer pLine, final IPath pFilePath) {
        super();
        this.ruleId = pRuleId;
        this.message = pMessage;
        this.location = pLocation;
        this.line = pLine;
        this.filePath = pFilePath;
    }

    /**
     * Empty constructor
     */
    public ViolationDescriptor() {
        this.ruleId = "";
        this.message = "";
        this.location = "";
        this.line = -1;
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor.
     * IFileRuleDescriptor#getName()
     */
    @Override
    public String getName() {
        return this.message;
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor.
     * IFileRuleDescriptor#getLine()
     */
    @Override
    public Integer getValue() {
        return this.line;
    }

    /**
     * @return the ruleId
     */
    public String getRuleId() {
        return ruleId;
    }

    /**
     * @param pRuleId
     *            the ruleId to set
     */
    public void setRuleId(final String pRuleId) {
        this.ruleId = pRuleId;
    }

    /**
     * @return the location
     */
    public String getLocation() {
        return location;
    }

    /**
     * @param pLocation
     *            the location to set
     */
    public void setLocation(final String pLocation) {
        this.location = pLocation;
    }

    /**
     * @return the filePath
     */
    public IPath getFilePath() {
        return filePath;
    }

    /**
     * @param pFilePath
     *            the filePath to set
     */
    public void setFilePath(final IPath pFilePath) {
        this.filePath = pFilePath;
    }

    /**
     * @param pName
     *            the name to set
     */
    public void setName(final String pName) {
        this.message = pName;
    }

    /**
     * @param pLine
     *            the value to set
     */
    public void setLine(final Integer pLine) {
        this.line = pLine;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public ViolationDescriptor clone() throws CloneNotSupportedException {
        final ViolationDescriptor clone = (ViolationDescriptor) super.clone();
        clone.setRuleId(this.ruleId);
        clone.setName(this.message);
        clone.setMessage(this.message);
        clone.setFilePath(this.filePath);
        clone.setLocation(this.location);
        clone.setLine(this.line);
        return clone;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

}
