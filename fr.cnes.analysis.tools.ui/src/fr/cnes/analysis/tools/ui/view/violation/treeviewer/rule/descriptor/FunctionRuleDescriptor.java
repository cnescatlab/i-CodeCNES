/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.descriptor;

import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.PlatformUI;

/**
 * Descriptor for rule's violations in a function.
 * 
 * 
 */
public class FunctionRuleDescriptor implements IRuleDescriptor, Cloneable {

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
        this.ruleId = "";
        this.location = "";
        this.message = "";
        this.value = -1;
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
        this.ruleId = pRuleId;
        this.filePath = pFilePath;
        this.location = pLocation;
        this.message = pMessage;
        this.value = pValue;
    }

    /**
     * Getter for the location.
     * 
     * @return the location
     */
    public String getLocation() {
        return this.location;
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.rules.IRuleDescriptor#getValue()
     */
    @Override
    public Integer getValue() {
        return this.value;
    }

    /**
     * Getter for file's path.
     * 
     * @return the file's path
     */
    public IPath getFilePath() {
        return this.filePath;
    }

    /**
     * Getter for the id.
     * 
     * @return the id
     */
    public String getRuleId() {
        return this.ruleId;
    }

    /**
     * Setter for the location.
     * 
     * @param pLocation
     *            the location to set
     */
    public void setLocation(final String pLocation) {
        this.location = pLocation;
    }

    /**
     * Setter for the line.
     * 
     * @param pValue
     *            line's violation
     */
    public void setValue(final Integer pValue) {
        this.value = pValue;
    }

    /**
     * Setter for the file's path.
     * 
     * @param pFilePath
     *            the file's path to set
     */
    public void setFilePath(final IPath pFilePath) {
        this.filePath = pFilePath;
    }

    /**
     * Setter for rule's id.
     * 
     * @param pRuleId
     *            the id to set
     */
    public void setRuleId(final String pRuleId) {
        this.ruleId = pRuleId;
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.rules.IRuleDescriptor#getName()
     */
    @Override
    public String getName() {
        return this.location;
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.rules.IRuleDescriptor#getCriticity()
     */
    @Override
    public String getCriticity() {
        return PlatformUI.getPreferenceStore().getString(this.ruleId + ".Criticity");
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public FunctionRuleDescriptor clone() throws CloneNotSupportedException {
        final FunctionRuleDescriptor clone = (FunctionRuleDescriptor) super.clone();
        clone.setRuleId(this.ruleId);
        clone.setFilePath(this.filePath);
        clone.setLocation(this.location);
        clone.setMessage(this.message);
        clone.setValue(this.value);
        return clone;
    }

    /**
     * @return
     */
    public String getMessage() {
        return message;
    }

    /**
     * @param message
     */
    public void setMessage(String message) {
        this.message = message;
    }
}
