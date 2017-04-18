/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.handler;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;

import fr.cnes.analysis.tools.analyzer.exception.NullContributionException;
import fr.cnes.analysis.tools.ui.exception.EmptyResourceException;
import fr.cnes.analysis.tools.ui.exception.EmptySelectionException;
import fr.cnes.analysis.tools.ui.exception.InvalidResourceTypeException;
import fr.cnes.analysis.tools.ui.exception.NonAccessibleResourceException;
import fr.cnes.analysis.tools.ui.exception.UnknownResourceTypeException;
import fr.cnes.analysis.tools.ui.utils.AnalysisHandlerUIUtils;
import fr.cnes.analysis.tools.ui.utils.PreferencesUIUtils;

/**
 * Metric Handler.
 *
 */
public class MetricAnalysisAllHandler extends AbstractHandler {
    /** Logger. **/
    private static final Logger LOGGER = Logger.getLogger(AbstractAnalysisHandler.class.getName());

    /** Selection into the package explorer. */
    private IStructuredSelection selection;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        try {
            LOGGER.finest("Begin execute method");

            // retrieve the active selection in the package explorer
            final IStructuredSelection selection = HandlerUtil.getCurrentStructuredSelection(event);

            // check selection
            AnalysisHandlerUIUtils.checkSelection(selection);

            final IExtensionRegistry registry = Platform.getExtensionRegistry();
            final IConfigurationElement[] configurationElem = registry
                    .getConfigurationElementsFor("org.eclipse.ui.handlers");

            if (configurationElem != null) {
                for (int i = 0; i < configurationElem.length; i++) {
                    // local variables to save extensionId and fileExtenion
                    String extId = "";
                    final List<String> fileExt = new ArrayList<String>();
                    // plugin.xml navigation
                    final String command = configurationElem[i].getAttribute("commandId");
                    if (command.contains("fr.cnes.analysis.tools.") && command.contains("metric")) {
                        // depend on the handler -> execute the rules or metrics
                        // get file extensions and extensions id
                        final IConfigurationElement[] classAttr = configurationElem[i]
                                .getChildren("class");
                        for (int j = 0; j < classAttr.length; j++) {
                            final IConfigurationElement[] param = classAttr[j]
                                    .getChildren("parameter");
                            for (int z = 0; z < param.length; z++) {
                                final String name = param[z].getAttribute("name");
                                if ("extensionId".equals(name)) {
                                    extId = param[z].getAttribute("value");
                                } else if (name.contains("fileExtension")) {
                                    fileExt.add(param[z].getAttribute("value"));
                                }
                            }
                        }
                        // execution
                        final String[] extensions = new String[fileExt.size()];
                        fileExt.toArray(extensions);
                        try {
                            runMetric(selection, extensions, extId);
                        } catch (EmptyResourceException | UnknownResourceTypeException
                                | InvalidResourceTypeException
                                | EmptySelectionException exception) {
                            LOGGER.log(Level.FINER,
                                    exception.getClass() + " : " + exception.getMessage(),
                                    exception);
                        }
                    }
                }
                LOGGER.finest("End execute method");
            }
        } catch (NonAccessibleResourceException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openWarning(
            		HandlerUtil.getActiveShell(event),
                    "The resource is not accessible", exception.getMessage());
        } catch (CoreException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openWarning(
            		HandlerUtil.getActiveShell(event),
                    "Core exception", exception.getMessage());
        } catch (NullContributionException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openWarning(
            		HandlerUtil.getActiveShell(event),
                    "Contribution is null", exception.getMessage());
        } catch (EmptySelectionException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openWarning(
            		HandlerUtil.getActiveShell(event),
                    "Selection is empty", exception.getMessage());
        }
        return null;
    }

    /**
     * Execute the analyzer on metrics over the list of files
     * 
     * @param selection
     *            files to execute the analysis
     * @param fileExtension
     *            list of extensions depend on the language
     * @param analyzerID
     *            identifier of the analysis language
     * 
     * @throws EmptyResourceException
     *             Resource is empty
     * @throws UnknownResourceTypeException
     *             Resource is unknown
     * @throws InvalidResourceTypeException
     *             Resource type is invalid
     * @throws NonAccessibleResourceException
     *             Resource is not accessible
     * @throws EmptySelectionException
     *             Selection is empty
     * @throws CoreException
     *             Internal error
     * @throws NullContributionException
     *             Contribution is null
     */
    private void runMetric(IStructuredSelection selection, String[] fileExtension,
            String analyzerID) throws EmptyResourceException, UnknownResourceTypeException,
                    InvalidResourceTypeException, NonAccessibleResourceException,
                    EmptySelectionException, CoreException, NullContributionException {

        final boolean analyserActivated = isAnalyzerActivated(analyzerID);

        if (analyserActivated) {
            // retrieve the file(s) of the selected language
            final List<IPath> files = AnalysisHandlerUIUtils.retrieveFiles(selection,
                    fileExtension);

            // run the analysis on the retrieved files
            final MetricAnalysisHandler metricAnalyzer = new MetricAnalysisHandler();

            metricAnalyzer.setAnalyzerId(analyzerID);
            metricAnalyzer.runAnalysis(files, analyzerID);
        }

    }

    /**
     * Check if the analyzer is activated in the preferences.
     * 
     * @param analyzerID
     *            analyzer id.
     * @return true if the analyzer is activated and false otherwise.
     */
    private boolean isAnalyzerActivated(String analyzerID) {
        final IPreferenceStore store = PlatformUI.getPreferenceStore();
        boolean analyserActivated = false;
        final String sh = "shell";
        final String f77 = "fortran77";
        final String f90 = "fortran90";

        final IConfigurationElement[] contributions = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(PreferencesUIUtils.PREF_EXT_PT_ID);
        String contributionId = "";
        for (final IConfigurationElement contribution : contributions) {
            contributionId = contribution.getAttribute(PreferencesUIUtils.CONTRIB_ID);
            if (analyzerID.contains(f90) && contributionId.contains(f90)) {
                analyserActivated = store
                        .getBoolean(contribution.getAttribute(PreferencesUIUtils.PARENT_ID))
                        && store.getBoolean(
                                contribution.getAttribute(PreferencesUIUtils.METRIC_PAGE_ID));
            } else if (analyzerID.contains(f77) && contributionId.contains(f77)) {
                analyserActivated = store
                        .getBoolean(contribution.getAttribute(PreferencesUIUtils.PARENT_ID))
                        && store.getBoolean(
                                contribution.getAttribute(PreferencesUIUtils.METRIC_PAGE_ID));
            } else if (analyzerID.contains(sh) && contributionId.contains(sh)) {
                analyserActivated = store
                        .getBoolean(contribution.getAttribute(PreferencesUIUtils.PARENT_ID))
                        && store.getBoolean(
                                contribution.getAttribute(PreferencesUIUtils.METRIC_PAGE_ID));
            }
        }
        return analyserActivated;
    }

    /**
     * @return the selection
     */
    public IStructuredSelection getSelection() {
        return selection;
    }

    /**
     * @param pSelection
     *            the selection to set
     */
    public void setSelection(IStructuredSelection pSelection) {
        this.selection = pSelection;
    }
}
