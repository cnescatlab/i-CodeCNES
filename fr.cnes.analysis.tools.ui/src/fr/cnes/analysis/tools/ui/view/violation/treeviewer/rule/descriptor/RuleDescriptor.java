/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.descriptor;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.ui.PlatformUI;

/**
 * Class for general description of a rule for a
 * {@link fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.RuleTreeViewer}
 * .
 * 
 */
public class RuleDescriptor implements IRuleDescriptor, Cloneable {

    /** Rule's id. **/
    private String                   ruleId;
    /** Rule's name. **/
    private String                   name;
    /** List of analyzed files with their violations. **/
    private List<FileRuleDescriptor> descriptors;

    /**
     * Empty constructor.
     */
    public RuleDescriptor() {
        this.ruleId = "";
        this.name = "";
        this.descriptors = new LinkedList<FileRuleDescriptor>();
    }

    /**
     * Constructor with the id and the name.
     * 
     * @param pRuleId
     *            rule's id
     * @param pName
     *            rule's name
     */
    public RuleDescriptor(final String pRuleId, final String pName) {
        this.ruleId = pRuleId;
        this.name = pName;
        this.descriptors = new LinkedList<FileRuleDescriptor>();
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
     * Getter for the descriptors.
     * 
     * @return the descriptors
     */
    public List<FileRuleDescriptor> getDescriptors() {
        return this.descriptors;
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

    /**
     * Setter for the descriptors
     * 
     * @param pDescriptors
     *            the descriptors to set
     */
    public void setDescriptors(final List<FileRuleDescriptor> pDescriptors) {
        this.descriptors = pDescriptors;
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.rules.IRuleDescriptor#getValue()
     */
    @Override
    public Integer getValue() {
        Integer value = 0;
        for (final FileRuleDescriptor descriptor : this.descriptors) {
            value = value + descriptor.getValue();
        }
        return value;
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
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        assert false : "hashCode not designed";
        return this.descriptors.size();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public RuleDescriptor clone() throws CloneNotSupportedException {
        final RuleDescriptor clone = (RuleDescriptor) super.clone();
        clone.setRuleId(this.ruleId);
        clone.setName(this.name);
        clone.setDescriptors(new LinkedList<FileRuleDescriptor>(this.descriptors));
        return clone;
    }
}
