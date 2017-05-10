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

import fr.cnes.analysis.tools.analyzer.MetricAnalysisJob;
import fr.cnes.analysis.tools.analyzer.datas.FileValue;
import fr.cnes.analysis.tools.ui.exception.EmptyProviderException;
import fr.cnes.analysis.tools.ui.view.MetricsView;

/**
 * This class is the Handler that linked together action button on the menu and
 * analyze function located
 */
public class MetricAnalysisHandler extends AbstractAnalysisHandler {
	/** Logger. **/
	private static final Logger LOGGER = Logger.getLogger(MetricAnalysisHandler.class.getName());

	/**
	 * List of analyzed files
	 */
	private final List<String> analyzedFiles = new ArrayList<String>();

	public MetricAnalysisHandler() {
		this(null);
	}

	
	public MetricAnalysisHandler(IPlatformUIProvider p) {
		super(p);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * fr.cnes.analysis.tools.ui.handler.AbstractAnalysisHandler#runAnalysis
	 * (java.util.List, java.lang.String)
	 */
	@Override
	public void runAnalysis(final List<IPath> files, final String pAnalyzerID) {
		LOGGER.finest("Begin runAnalysis method");

		// Clear the analyzedFiles list in order to have the new analyzed files
		analyzedFiles.clear();
		List<File> analysisFiles = new ArrayList<>();
		for(IPath file : files){
			analysisFiles.add(file.toFile());
		}
		// Instantiate analyzer
		final MetricAnalysisJob analysis = new MetricAnalysisJob(pAnalyzerID, analysisFiles);

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
						// We generate an XML file only if the analysis wasn't
						// interrupted
						if (analysis.getResult().isOK()) {
							MetricAnalysisHandler.this.updateView(analysis.getValues());
						}
					}
				});
			}
		});

		LOGGER.finest("End runAnalysis method");
	}

	/**
	 * Update the view
	 * 
	 * @param values
	 *            list of file value .
	 */
	private void updateView(final List<FileValue> values) {
		LOGGER.finest("Begin updateView method");

		try {
			// get the page
			final IWorkbenchPage page = getPlatformUIProvider().getWorkbench().getActiveWorkbenchWindow()
					.getActivePage();

			// open view
			page.showView(MetricsView.VIEW_ID);

			// get view
			final MetricsView view = (MetricsView) page.findView(MetricsView.VIEW_ID);

			// show rules analyze results
			if (view != null) {
				view.display(values, this.getSelectedProject(), this.getAuthor(), this.getDate());
				/* Add IMarkers everywhere there is a metric event */
				view.insertMarkers();
			}

		} catch (final PartInitException exception) {
			LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(), exception);
			showError(getPlatformUIProvider().getWorkbench().getActiveWorkbenchWindow().getShell(),
					"Internal Error", "Contact support service : \n" + exception.getMessage());
		} catch (final EmptyProviderException exception) {
			LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(), exception);
			showError(getPlatformUIProvider().getWorkbench().getActiveWorkbenchWindow().getShell(),
					"Internal Error", "Contact support service : \n" + exception.getMessage());
		}

		LOGGER.finest("End updateView method");
	}




}
