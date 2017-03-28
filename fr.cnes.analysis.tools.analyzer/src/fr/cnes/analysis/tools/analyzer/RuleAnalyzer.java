/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.analysis.tools.analyzer;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

/**
 * This class is used to instantiate a new rule analyzer.
 */
public class RuleAnalyzer extends AbstractAnalyzer {
	/** Logger. */
	private static final Logger LOGGER = Logger.getLogger(RuleAnalyzer.class
			.getName());

	/** List of values found during analysis. **/
	private List<Violation> violations;

	/**
	 * Constructor that set the job with string name, extension id and a list of
	 * {@link org.eclipse.core.runtime.IPath}.
	 * 
	 * @param name
	 *            the name of this Job
	 * @param pFiles
	 *            the files to analyze
	 * @param pExtensionId
	 *            the id of rule/metric contribution
	 */
	public RuleAnalyzer(final String name, final List<IPath> pFiles,
			final String pExtensionId) {
		super(name, pFiles, pExtensionId);
		this.violations = new LinkedList<Violation>();
	}

	/**
	 * Retrieve the violations of analysis.
	 * 
	 * @return violations of the rule analysis
	 */
	public List<Violation> getViolations() {
		return this.violations;
	}

	/**
	 * Set the values with a list.
	 * 
	 * @param pViolations
	 *            the list of violations to set
	 */
	public void setViolations(final List<Violation> pViolations) {
		this.violations = pViolations;
	}

	/**
	 * Set the values with an array.
	 * 
	 * @param pViolations
	 *            the array of violations to set
	 */
	public void setDescriptors(final Violation[] pViolations) {
		this.violations = new LinkedList<Violation>();
		for (final Violation value : pViolations) {
			this.violations.add(value);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * fr.cnes.analysis.tools.analyzer.AbstractAnalyzer#runEvaluation(org.eclipse
	 * .core.runtime.IConfigurationElement, java.util.List,
	 * org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	protected IStatus runEvaluation(final IConfigurationElement contribution,
			final List<IPath> pFiles, final IProgressMonitor monitor)
			throws CloneNotSupportedException, CoreException, IOException,
			JFlexException {
		LOGGER.finest("Begin runEvaluation method");

		// Instantiate return variable
		IStatus status = Status.OK_STATUS;

		// Run analysis on all files
		for (final IPath file : pFiles) {

			// Get the evaluation
			final AbstractRule rule = (AbstractRule) contribution
					.createExecutableExtension("class");
			rule.setContribution(contribution);

			// Run the evaluation
			LOGGER.finest("File : " + file.toFile().getName());
			monitor.subTask("Analyzing " + contribution.getAttribute("id")
					+ " on file " + file.toFile().getName());
			this.violations.addAll(this.runRuleOnFile(rule, file));
			monitor.worked(1);

			// Stop analysis if cancel button selected
			if (monitor.isCanceled()) {
				status = Status.CANCEL_STATUS;
				break;
			}
		}

		LOGGER.finest("End runEvaluation method");
		return status;
	}

	/**
	 * Compute the provided rule on the file.
	 * 
	 * @param rule
	 *            the rule to compute
	 * @param file
	 *            the file to measure
	 * @return list of evaluation (metric value or rule violations)
	 * @throws IOException
	 *             internal error
	 * @throws JFlexException
	 *             JFlex analysis error
	 */
	private List<Violation> runRuleOnFile(final AbstractRule rule,
			final IPath file) throws IOException, JFlexException {
		LOGGER.finest("Begin runRuleOnFile method");

		// Initializing file reader in the metric
		rule.setInputFile(file);

		LOGGER.finest("End runRuleOnFile method");
		try {
			return rule.run();
		} catch (JFlexException e) {
			String msg = e.getCause().getMessage() + " : file " + file
					+ " : rule " + rule.getClass();
			throw new JFlexException(new Exception(msg));
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.runtime.jobs.Job#canceling()
	 */
	@Override
	protected void canceling() {
		this.violations.clear();
	}
}
