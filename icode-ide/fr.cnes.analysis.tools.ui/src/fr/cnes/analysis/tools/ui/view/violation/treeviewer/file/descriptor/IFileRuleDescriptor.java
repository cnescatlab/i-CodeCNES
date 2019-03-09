/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor;

/**
 * Interface for the rule, a file or a violation. This interface is mainly used
 * to display results in the ViolationsView.
 * 
 * @see IFileRuleDescriptor
 * @see FileRuleDescriptor
 * @see RuleDescriptor
 * @see ViolationDescriptor
 * @version 2.0
 * @since 2.0
 */
public interface IFileRuleDescriptor {
    /**
     * Returns the name of this descriptor.
     * 
     * @return the name
     */
    String getName();

    /**
     * Returns the value of this descriptor.
     * 
     * @return the value
     */
    Integer getValue();

}