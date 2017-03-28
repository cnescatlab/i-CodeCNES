/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.descriptor;

/**
 * Interface which represents any level of a rule description. It could describe
 * the rule, a file or a violation. This interface is mainly used to display
 * results in the ViolationsView.
 * 
 */
public interface IRuleDescriptor {
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

	/**
	 * Returns the criticity of the rule associated to this descriptor.
	 * 
	 * @return the criticity
	 */
	String getCriticity();
}