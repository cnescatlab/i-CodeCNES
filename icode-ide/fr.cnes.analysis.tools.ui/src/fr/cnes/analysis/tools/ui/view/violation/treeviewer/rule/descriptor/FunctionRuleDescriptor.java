/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.descriptor;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;
import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;

/**
 * Descriptor for rule's violations in a function.
 * 
 * 
 */
public class FunctionRuleDescriptor implements IRuleDescriptor, Cloneable {

    /** Class name */
    private static final String CLASS = FunctionRuleDescriptor.class.getName();
    /** Function containing the violation. **/
    private String location;
    /** Violation's message */
    private String message;
    /** Line of the violation. **/
    private Integer value;
    /** Path of the file containing the violation. **/
    private IPath filePath;
    /** Id of the violated rule. **/
    private String ruleId;

    /**
     * Empty constructor.
     */
    public FunctionRuleDescriptor() {
        final String method = "FunctionRuleDescriptor";
        ICodeLogger.entering(CLASS, method);
        this.ruleId = "";
        this.location = "";
        this.message = "";
        this.value = Integer.valueOf(-1);
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Constructor with every attribute as parameter.
     * 
     * @param pRuleId
     *            rule's id
     * @param pFilePath
     *            file's path
     * @param pLocation
     *            violation's location
     * @param pMessage
     *            violation's message
     * @param pValue
     *            violation's line
     */
    public FunctionRuleDescriptor(final String pRuleId, final IPath pFilePath,
                    final String pLocation, final String pMessage, final Integer pValue) {
        final String method = "";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pRuleId, pFilePath, pLocation, pMessage, pValue
        });
        this.ruleId = pRuleId;
        this.filePath = pFilePath;
        this.location = pLocation;
        this.message = pMessage;
        this.value = pValue;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Getter for the location.
     * 
     * @return the location
     */
    public String getLocation() {
        final String method = "getLocation";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, this.location);
        return this.location;
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.rules.IRuleDescriptor#getValue()
     */
    @Override
    public Integer getValue() {
        final String method = "getValue";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, this.value);
        return this.value;
    }

    /**
     * Getter for file's path.
     * 
     * @return the file's path
     */
    public IPath getFilePath() {
        final String method = "getFilePath";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, this.filePath);
        return this.filePath;
    }

    /**
     * Getter for the id.
     * 
     * @return the id
     */
    public String getRuleId() {
        final String method = "getRuleId";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, this.ruleId);
        return this.ruleId;
    }

    /**
     * Setter for the location.
     * 
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
     * Setter for the line.
     * 
     * @param pValue
     *            line's violation
     */
    public void setValue(final Integer pValue) {
        final String method = "setValue";
        ICodeLogger.entering(CLASS, method, pValue);
        this.value = pValue;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Setter for the file's path.
     * 
     * @param pFilePath
     *            the file's path to set
     */
    public void setFilePath(final IPath pFilePath) {
        final String method = "setFilePath";
        ICodeLogger.entering(CLASS, method, pFilePath);
        this.filePath = pFilePath;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Setter for rule's id.
     * 
     * @param pRuleId
     *            the id to set
     */
    public void setRuleId(final String pRuleId) {
        final String method = "setRuleId";
        ICodeLogger.entering(CLASS, method, pRuleId);
        this.ruleId = pRuleId;
        ICodeLogger.exiting(CLASS, method);
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.rules.IRuleDescriptor#getName()
     */
    @Override
    public String getName() {
        final String method = "getName";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, this.location);
        return this.location;
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.rules.IRuleDescriptor#getCriticity()
     */
    @Override
    public String getSeverity() {
        final String method = "getSeverity";
        ICodeLogger.entering(CLASS, method);
        final String severity = UserPreferencesService.getCheckerSeverity(this.getRuleId());
        ICodeLogger.exiting(CLASS, method, severity);
        return severity;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public FunctionRuleDescriptor clone() throws CloneNotSupportedException {
        final String method = "clone";
        ICodeLogger.entering(CLASS, method);
        final FunctionRuleDescriptor clone = (FunctionRuleDescriptor) super.clone();
        clone.setRuleId(this.ruleId);
        clone.setFilePath(this.filePath);
        clone.setLocation(this.location);
        clone.setMessage(this.message);
        clone.setValue(this.value);
        ICodeLogger.exiting(CLASS, method, clone);
        return clone;
    }

    /**
     * @return function rule message
     */
    public String getMessage() {
        final String method = "getMessage";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, message);
        return message;
    }

    /**
     * @param message
     */
    public void setMessage(final String message) {
        final String method = "setMessage";
        ICodeLogger.entering(CLASS, method, message);
        this.message = message;
        ICodeLogger.exiting(CLASS, method);
    }
}
