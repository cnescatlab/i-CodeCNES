/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.descriptor;

import java.util.LinkedList;
import java.util.List;

import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;
import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;

/**
 * Class for general description of a rule for a
 * {@link fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.RuleTreeViewer}
 * .
 * 
 */
public class RuleDescriptor implements IRuleDescriptor, Cloneable {

    /** Class name */
    private static final String CLASS = RuleDescriptor.class.getName();
    /** Rule's id. **/
    private String ruleId;
    /** Rule's name. **/
    private String name;
    /** List of analyzed files with their violations. **/
    private List<FileRuleDescriptor> descriptors;

    /**
     * Empty constructor.
     */
    public RuleDescriptor() {
        final String method = "RuleDescriptor";
        ICodeLogger.entering(CLASS, method);

        this.ruleId = "";
        this.name = "";
        this.descriptors = new LinkedList<FileRuleDescriptor>();
        ICodeLogger.exiting(CLASS, method);
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
        final String method = "RuleDescriptor";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pRuleId, pName
        });
        this.ruleId = pRuleId;
        this.name = pName;
        this.descriptors = new LinkedList<FileRuleDescriptor>();
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
     * Getter for the descriptors.
     * 
     * @return the descriptors
     */
    public List<FileRuleDescriptor> getDescriptors() {
        final String method = "getDescriptors";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, this.descriptors);
        return this.descriptors;
    }

    /**
     * Setter for the id.
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

    /**
     * Setter for the name
     * 
     * @param pName
     *            the name to set
     */
    public void setName(final String pName) {
        final String method = "setName";
        ICodeLogger.entering(CLASS, method, pName);
        this.name = pName;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Setter for the descriptors
     * 
     * @param pDescriptors
     *            the descriptors to set
     */
    public void setDescriptors(final List<FileRuleDescriptor> pDescriptors) {
        final String method = "setDescriptors";
        ICodeLogger.entering(CLASS, method, pDescriptors);
        this.descriptors = pDescriptors;
        ICodeLogger.exiting(CLASS, method);
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
        Integer value = Integer.valueOf(0);
        for (final FileRuleDescriptor descriptor : this.descriptors) {
            value = Integer.valueOf(value.intValue() + descriptor.getValue().intValue());
        }
        ICodeLogger.exiting(CLASS, method, value);
        return value;
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
        final String severity = UserPreferencesService.getCheckerSeverity(this.ruleId);
        ICodeLogger.exiting(CLASS, method, severity);
        return severity;
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
        final String method = "clone";
        ICodeLogger.entering(CLASS, method);
        final RuleDescriptor clone = (RuleDescriptor) super.clone();
        clone.setRuleId(this.ruleId);
        clone.setName(this.name);
        clone.setDescriptors(new LinkedList<FileRuleDescriptor>(this.descriptors));
        ICodeLogger.exiting(CLASS, method, clone);
        return clone;
    }
}
