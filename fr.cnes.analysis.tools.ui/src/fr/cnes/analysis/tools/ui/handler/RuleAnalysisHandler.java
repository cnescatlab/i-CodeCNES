/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.handler;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.analyzer.AbstractAnalysisJob;
import fr.cnes.analysis.tools.analyzer.RuleAnalysisJob;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.ui.view.ViolationsView;

/**
 * This class is the Handler that linked together action button on the menu and
 * analyze function located
 * 
 */
public class RuleAnalysisHandler extends AbstractAnalysisHandler {
	/**
	 * Logger
	 */
	private static final Logger LOGGER = Logger.getLogger(RuleAnalysisHandler.class.getName());

	/**
	 * List of all files that will be analyzed
	 */
	private List<String> analyzedFiles = new ArrayList<String>();

	public RuleAnalysisHandler() {
		this(null);
	}

	public RuleAnalysisHandler(IPlatformUIProvider p) {
		super(p);
	}

	/**
	 * Run the analysis on the retrieved files.
	 * 
	 * @param files
	 *            the files to analyze
	 * @param pAnalyzerID
	 *            the id of analyzer on which the analysis is made
	 */
	@Override
	public void runAnalysis(final List<IPath> files, final String pAnalyzerID) {

		LOGGER.finest("Begin runAnalysis method");

		// Clear the analyzedFiles list in order to have the new analyzed files
		analyzedFiles.clear();

		// Instantiate analyzer
		List<File> analysisFiles = new ArrayList<>();
		for (IPath file : files) {
			analysisFiles.add(file.toFile());
		}
		final AbstractAnalysisJob analysis = new RuleAnalysisJob(pAnalyzerID, analysisFiles);

		// run analysis
		analysis.setUser(true);
		analysis.schedule();

		// add change listener to check when the job is done
		analysis.addJobChangeListener(new JobChangeAdapter() {

			@Override
			public void done(final IJobChangeEvent event) {
				Display.getDefault().asyncExec(new Runnable() {

					@Override

					public void run() {
						if (analysis.getResult().isOK()) {
							RuleAnalysisHandler.this.updateView(((RuleAnalysisJob) event.getJob()).getCheckResults());
						}
					}

				});
			}
		});

		LOGGER.finest("End runAnalysis method");
	}

	/**
	 * Update the violation's view
	 * 
	 * @param pCheckResults
	 *            .
	 */
	protected void updateView(final List<CheckResult> pCheckResults) {
		LOGGER.finest("Begin updateView method");

		try {
			// get the page
			final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();

			// open view
			page.showView(ViolationsView.VIEW_ID);

			// get view
			final ViolationsView view = (ViolationsView) page.findView(ViolationsView.VIEW_ID);

			// show rules analyze results
			if (view != null) {
				view.display(pCheckResults, this.getSelectedProject(), this.getAuthor(), this.getDate());
			}

		} catch (final PartInitException exception) {
			LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(), exception);
			showError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), "Internal Error",
					"Contact support service : \n" + exception.getMessage());
		}

		LOGGER.finest("End updateView method");
	}

}
