/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;

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
    /** Class name **/
    private static final String CLASS = ViolationDescriptor.class.getName();
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
        final String method = "ViolationDescriptor";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pRuleId, pLocation, pMessage, pLocation, pLine, pFilePath
        });
        this.ruleId = pRuleId;
        this.message = pMessage;
        this.location = pLocation;
        this.line = pLine;
        this.filePath = pFilePath;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Empty constructor
     */
    public ViolationDescriptor() {
        final String method = "ViolationDescriptor";
        ICodeLogger.entering(CLASS, method);
        this.ruleId = "";
        this.message = "";
        this.location = "";
        this.line = Integer.valueOf(-1);
        ICodeLogger.exiting(CLASS, method);
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor.
     * IFileRuleDescriptor#getName()
     */
    @Override
    public String getName() {
        final String method = "getName";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, message);
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
        final String method = "getValue";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, line);
        return this.line;
    }

    /**
     * @return the ruleId
     */
    public String getRuleId() {
        final String method = "getRuleId";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, ruleId);
        return ruleId;
    }

    /**
     * @param pRuleId
     *            the ruleId to set
     */
    public void setRuleId(final String pRuleId) {
        final String method = "setRuleId";
        ICodeLogger.entering(CLASS, method, pRuleId);
        this.ruleId = pRuleId;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the location
     */
    public String getLocation() {
        final String method = "getLocation";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, location);
        return location;
    }

    /**
     * @param pLocation
     *            the location to set
     */
    public void setLocation(final String pLocation) {
        final String method = "setLocation";
        ICodeLogger.entering(CLASS, method, pLocation);
        this.location = pLocation;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the filePath
     */
    public IPath getFilePath() {
        final String method = "getFilePath";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, filePath);
        return filePath;
    }

    /**
     * @param pFilePath
     *            the filePath to set
     */
    public void setFilePath(final IPath pFilePath) {
        final String method = "setFilePath";
        ICodeLogger.entering(CLASS, method, pFilePath);
        this.filePath = pFilePath;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @param pName
     *            the name to set
     */
    public void setName(final String pName) {
        final String method = "setName";
        ICodeLogger.entering(CLASS, method, pName);
        this.message = pName;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @param pLine
     *            the value to set
     */
    public void setLine(final Integer pLine) {
        final String method = "setLine";
        ICodeLogger.entering(CLASS, method, pLine);
        this.line = pLine;
        ICodeLogger.exiting(CLASS, method);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public ViolationDescriptor clone() throws CloneNotSupportedException {
        final String method = "clone";
        ICodeLogger.entering(CLASS, method);
        final ViolationDescriptor clone = (ViolationDescriptor) super.clone();
        clone.setRuleId(this.ruleId);
        clone.setName(this.message);
        clone.setMessage(this.message);
        clone.setFilePath(this.filePath);
        clone.setLocation(this.location);
        clone.setLine(this.line);
        ICodeLogger.exiting(CLASS, method, clone);
        return clone;
    }

    /**
     * @return Violation message.
     */
    public String getMessage() {
        final String method = "getMessage";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, message);
        return message;
    }

    /**
     * @param pMessage
     *            Violation message to set.
     */
    public void setMessage(final String pMessage) {
        final String method = "setMessage";
        ICodeLogger.entering(CLASS, method, pMessage);
        this.message = pMessage;
        ICodeLogger.exiting(CLASS, method);
    }

}
