/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;
import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;

/**
 * Descriptor for a Rule that is intended to be shown.</br>
 * This descriptor would return it's rules {@link #name} and the number of
 * violations that it contains while using {@link #getName()} and
 * {@link #getValue()} when instanced in a {@link IFileRuleDescriptor}.</br>
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
public class RuleDescriptor implements IFileRuleDescriptor, Cloneable {
    /** Class name **/
    private static final String CLASS = RuleDescriptor.class.getName();

    /** Rule's id. **/
    private String ruleId;
    /** Rule's name. **/
    private String name;
    /** Function containing the violation. **/
    private String location;
    /** Number of violations stored for the rule */
    private Integer value;
    /** The violations stored for the rule */
    private List<ViolationDescriptor> descriptors;
    /** File name */
    private IPath filePath;

    /**
     * Empty constructor.
     */
    public RuleDescriptor() {
        final String method = "";
        ICodeLogger.entering(CLASS, method);
        this.ruleId = "";
        this.name = "";
        this.location = "";
        this.value = Integer.valueOf(-1);
        this.descriptors = new LinkedList<>();
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Constructor for RuleDescriptor
     * 
     * @param pRuleId
     *            The ID of the rule
     * @param pName
     *            Name of the Rule
     * @param pLocation
     *            Location of the function, program, method containing the
     *            violation of the rule
     * @param pValue
     *            Number of Violation contained for this rule
     * @param pPath
     *            Path to the file containing the rule
     */
    public RuleDescriptor(final String pRuleId, final String pName, final String pLocation,
                    final Integer pValue, final IPath pPath) {
        super();
        final String method = "RuleDescriptor";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pRuleId, pName, pLocation, pValue, pPath
        });
        this.ruleId = pRuleId;
        this.name = pName;
        this.location = pLocation;
        this.value = pValue;
        this.descriptors = new LinkedList<>();
        this.filePath = pPath;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the descriptors
     */
    public List<ViolationDescriptor> getDescriptors() {
        final String method = "getDescriptors";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method);
        return descriptors;
    }

    /**
     * @param pDescriptors
     *            the descriptors to set
     */
    public void setDescriptors(final LinkedList<ViolationDescriptor> pDescriptors) {
        final String method = "setDescriptors";
        ICodeLogger.entering(CLASS, method, pDescriptors);
        this.descriptors = pDescriptors;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Getter for the id.
     * 
     * @return rule's id
     */
    public String getRuleId() {
        final String method = "getRuleId";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, this.ruleId);
        return this.ruleId;
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
        ICodeLogger.exiting(CLASS, method, this.name);
        return this.name;
    }

    /**
     * Setter for the id.
     * 
     * @param pRuleId
     *            the id to set
     */
    public void setRuleId(final String pRuleId) {
        final String method = "";
        ICodeLogger.entering(CLASS, method, pRuleId);
        this.ruleId = pRuleId;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Setter for the name
     * 
     * @param pName
     *            the name to set
     */
    public void setName(final String pName) {
        final String method = "";
        ICodeLogger.entering(CLASS, method, pName);
        this.name = pName;
        ICodeLogger.exiting(CLASS, method);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * fr.cnes.analysis.tools.ui.view.rules.treeviewer.file.IFileRuleDescriptor#
     * getValue()
     */
    @Override
    public Integer getValue() {
        final String method = "";
        ICodeLogger.entering(CLASS, method);
        final Integer value = Integer.valueOf(this.descriptors.size());
        ICodeLogger.exiting(CLASS, method, value);
        return value;
    }

    /**
     * @return The severity of the current Rule
     */
    public String getSeverity() {
        final String method = "getSeverity";
        ICodeLogger.entering(CLASS, method);
        final String severity = UserPreferencesService.getCheckerSeverity(this.ruleId);
        ICodeLogger.exiting(CLASS, method, severity);
        return severity;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final String method = "hashCode";
        ICodeLogger.entering(CLASS, method);
        final int prime = 31;
        int result = 1;
        result = prime * result + ((filePath == null) ? 0 : filePath.hashCode());
        result = prime * result + ((location == null) ? 0 : location.hashCode());
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        result = prime * result + ((ruleId == null) ? 0 : ruleId.hashCode());
        result = prime * result + ((value == null) ? 0 : value.hashCode());
        ICodeLogger.exiting(CLASS, method, Integer.valueOf(result));
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(final Object object) {
        final String method = "equals";
        ICodeLogger.entering(CLASS, method, object);
        final boolean isEqual;
        if (object instanceof RuleDescriptor) {
            isEqual = this.name.equals(((RuleDescriptor) object).getName());
        } else {
            isEqual = false;
        }
        ICodeLogger.exiting(CLASS, method, Boolean.valueOf(isEqual));
        return isEqual;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public RuleDescriptor clone() throws CloneNotSupportedException {
        final String method = "clone";
        ICodeLogger.entering(CLASS, method);
        final RuleDescriptor clone = (RuleDescriptor) super.clone();
        clone.setDescriptors(new LinkedList<ViolationDescriptor>(this.descriptors));
        clone.setRuleId(this.ruleId);
        clone.setName(this.name);
        clone.setFilePath(this.filePath);
        clone.setLocation(this.location);
        clone.setValue(this.value);
        ICodeLogger.exiting(CLASS, method, clone);
        return clone;
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
     * @param pValue
     *            the value to set
     */
    public void setValue(final Integer pValue) {
        final String method = "setValue";
        ICodeLogger.entering(CLASS, method, pValue);
        this.value = pValue;
        ICodeLogger.exiting(CLASS, method);
    }
}
