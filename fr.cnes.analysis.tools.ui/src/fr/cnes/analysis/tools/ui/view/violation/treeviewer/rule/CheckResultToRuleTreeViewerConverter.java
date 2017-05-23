/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule;

import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.descriptor.FileRuleDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.descriptor.FunctionRuleDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.descriptor.RuleDescriptor;

/**
 * Job used to converter inputs from analysis to valuable inputs for the
 * ViolationView.
 * 
 */
public class CheckResultToRuleTreeViewerConverter extends Job {
	/** Logger. **/
	public final static Logger LOGGER = Logger.getLogger(CheckResultToRuleTreeViewerConverter.class.getName());

	/** The original inputs. **/
	private CheckResult[] inputs;
	/** A value container which has all values of rules. **/
	private RuleDescriptor[] container;

	/**
	 * Empty constructor for this Job.
	 */
	public CheckResultToRuleTreeViewerConverter() {
		super("Converting results...");
		this.inputs = new CheckResult[0];
		this.container = new RuleDescriptor[0];
	}

	/**
	 * Constructor for this Job with an array of violations.
	 * 
	 * @param pInputs
	 *            the inputs
	 */
	public CheckResultToRuleTreeViewerConverter(final CheckResult[] pInputs) {
		super("Converting results...");
		this.inputs = pInputs.clone();
		this.container = new RuleDescriptor[0];
	}

	/**
	 * Getter for the inputs id.
	 * 
	 * @return the inputs
	 */
	public CheckResult[] getInputs() {
		return this.inputs.clone();
	}

	/**
	 * Getter for the container
	 * 
	 * @return the container
	 */
	public RuleDescriptor[] getContainer() {
		return this.container.clone();
	}

	/**
	 * Setter for the inputs.
	 * 
	 * @param pInputs
	 *            the inputs to set
	 */
	public void setInputs(final CheckResult[] pInputs) {
		this.inputs = pInputs.clone();
	}

	/**
	 * Setter for the container
	 * 
	 * @param pContainer
	 *            the container to set
	 */
	public void setContainer(final RuleDescriptor[] pContainer) {
		this.container = pContainer.clone();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
	 * IProgressMonitor)
	 */
	@Override
	public IStatus run(final IProgressMonitor monitor) {
		LOGGER.finest("Begin run method");
		// Instantiate return variable
		IStatus status = Status.OK_STATUS;
		final int totalWork = this.inputs.length;

		// Instantiate descriptors
		final List<RuleDescriptor> descriptors = new LinkedList<RuleDescriptor>();
		final RuleDescriptor rule = new RuleDescriptor();
		final FileRuleDescriptor file = new FileRuleDescriptor();
		final FunctionRuleDescriptor function = new FunctionRuleDescriptor();

		// Start converting
		monitor.beginTask("Converting...", totalWork);
		try {
			for (final CheckResult value : this.inputs) {
				if (descriptors.isEmpty()
						|| !descriptors.get(descriptors.size() - 1).getName().equals(value.getName())) {
					rule.getDescriptors().clear();
					rule.setRuleId(value.getId());
					rule.setName(value.getName());
					descriptors.add(rule.clone());

				}
				if (descriptors.get(descriptors.size() - 1).getDescriptors().isEmpty()
						|| !descriptors.get(descriptors.size() - 1).getDescriptors()
								.get(descriptors.get(descriptors.size() - 1).getDescriptors().size() - 1).getFilePath()
								.equals(new Path(value.getFile().getAbsolutePath()))) {
					file.getDescriptors().clear();
					file.setFilePath(new Path(value.getFile().getAbsolutePath()));
					descriptors.get(descriptors.size() - 1).getDescriptors().add(file.clone());
				}
				function.setRuleId(value.getId());
				function.setFilePath(new Path(value.getFile().getAbsolutePath()));
				function.setMessage(value.getMessage());
				function.setLocation(value.getLocation());
				function.setValue(value.getLine());
				descriptors.get(descriptors.size() - 1).getDescriptors()
						.get(descriptors.get(descriptors.size() - 1).getDescriptors().size() - 1).getDescriptors()
						.add(function.clone());
				monitor.worked(1);
			}
			this.container = descriptors.toArray(new RuleDescriptor[descriptors.size()]);
		} catch (final CloneNotSupportedException exception) {
			LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(), exception);
			status = new Status(Status.ERROR, "fr.cnes.analysis.tools.fortran.analyzer", Status.ERROR,
					exception.getMessage(), exception);
		}

		LOGGER.finest("End run method");
		return status;
	}
}
