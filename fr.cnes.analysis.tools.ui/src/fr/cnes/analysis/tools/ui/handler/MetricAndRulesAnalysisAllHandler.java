/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.handler;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.ui.exception.EmptySelectionException;
import fr.cnes.analysis.tools.ui.utils.AnalysisHandlerUIUtils;

/**
 * Handler to manage both run Metrics and Rules.
 *
 */
public class MetricAndRulesAnalysisAllHandler extends AbstractHandler {
    /** Logger. **/
    private static final Logger LOGGER = Logger
            .getLogger(MetricAndRulesAnalysisAllHandler.class.getName());

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(final ExecutionEvent event) throws ExecutionException {
        try {
            // retrieve the active selection in the package explorer
            final IStructuredSelection selection = (IStructuredSelection) PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getSelectionService().getSelection();

            // check selection
            AnalysisHandlerUIUtils.checkSelection(selection);

            // Run rules
            final RuleAnalysisAllHandler ruleAnalysis = new RuleAnalysisAllHandler();
            ruleAnalysis.setSelection(selection);
            ruleAnalysis.execute(event);

            // Run metrics
            final MetricAnalysisAllHandler metricAnalysis = new MetricAnalysisAllHandler();
            ruleAnalysis.setSelection(selection);
            metricAnalysis.execute(event);

        } catch (EmptySelectionException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openWarning(
                    PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Selection is empty", exception.getMessage());
        }
        return null;
    }

}
