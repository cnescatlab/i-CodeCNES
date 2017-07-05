/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor;

import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;
import java.util.LinkedList;
import java.util.List;
import org.eclipse.core.runtime.IPath;

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
        this.ruleId = "";
        this.name = "";
        this.location = "";
        this.value = -1;
        this.descriptors = new LinkedList<>();
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
    public RuleDescriptor(String pRuleId, String pName, String pLocation, Integer pValue,
            IPath pPath) {
        super();
        this.ruleId = pRuleId;
        this.name = pName;
        this.location = pLocation;
        this.value = pValue;
        this.descriptors = new LinkedList<>();
        this.filePath = pPath;
    }

    /**
     * @return the descriptors
     */
    public List<ViolationDescriptor> getDescriptors() {
        return descriptors;
    }

    /**
     * @param pDescriptors
     *            the descriptors to set
     */
    public void setDescriptors(LinkedList<ViolationDescriptor> pDescriptors) {
        this.descriptors = pDescriptors;
    }

    /**
     * Getter for the id.
     * 
     * @return rule's id
     */
    public String getRuleId() {
        return this.ruleId;
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.rules.IRuleDescriptor#getName()
     */
    @Override
    public String getName() {
        return this.name;
    }

    /**
     * Setter for the id.
     * 
     * @param pRuleId
     *            the id to set
     */
    public void setRuleId(final String pRuleId) {
        this.ruleId = pRuleId;
    }

    /**
     * Setter for the name
     * 
     * @param pName
     *            the name to set
     */
    public void setName(final String pName) {
        this.name = pName;
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
        return this.descriptors.size();
    }

    /**
     * @return The criticity of the current Rule
     */
    public String getCriticity() {
        return UserPreferencesService.getCheckerSeverity(this.ruleId);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((filePath == null) ? 0 : filePath.hashCode());
        result = prime * result + ((location == null) ? 0 : location.hashCode());
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        result = prime * result + ((ruleId == null) ? 0 : ruleId.hashCode());
        result = prime * result + ((value == null) ? 0 : value.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(final Object object) {
        boolean isEqual;
        if (object instanceof RuleDescriptor) {
            isEqual = this.name.equals(((RuleDescriptor) object).getName());
        } else {
            isEqual = false;
        }
        return isEqual;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public RuleDescriptor clone() throws CloneNotSupportedException {
        final RuleDescriptor clone = (RuleDescriptor) super.clone();
        clone.setDescriptors(new LinkedList<ViolationDescriptor>(this.descriptors));
        clone.setRuleId(this.ruleId);
        clone.setName(this.name);
        clone.setFilePath(this.filePath);
        clone.setLocation(this.location);
        clone.setValue(this.value);
        return clone;
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
    public void setLocation(String pLocation) {
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
    public void setFilePath(IPath pFilePath) {
        this.filePath = pFilePath;
    }

    /**
     * @param pValue
     *            the value to set
     */
    public void setValue(Integer pValue) {
        this.value = pValue;
    }
}
