/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferencepage.metrics;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.ui.preferencepage.PreferenceLabelProvider;

/**
 * This class is the label provider for metric preference pages. It extends the
 * preference label provider by adding one column for the minimum and another
 * one for the maximum.
 * 
 * 
 */
public class MetricPreferenceLabelProvider extends PreferenceLabelProvider {
    /** Static values that determines column types. **/
    /** This value is for the check box column. **/
    public static final int METRIC_CHECK = 0;
    /** This value is for metric name column. **/
    public static final int METRIC_NAME = 1;
    /** This value is for metric value column. **/
    public static final int METRIC_VAL = 2;

    /** Logger **/
    private static final Logger LOGGER = Logger
            .getLogger(MetricPreferenceLabelProvider.class.getName());

    /**
     * Constructor with integer parameter which represents the column created
     * and the viewer on which the column is defined.
     * 
     * @param pType
     *            the column to create
     * @param viewer
     *            the viewer
     * 
     */
    public MetricPreferenceLabelProvider(final int pType, final TableViewer viewer) {
        super(pType, viewer);
    }

    /**
     * This functions set the text for each element of a column.
     * 
     * @param element
     *            the element store in the column
     * @return the text to store in column case
     */
    @Override
    public String getText(final Object element) {
        LOGGER.finest("Begin getText method");

        String text = null;

        switch (this.getType()) {
            case MetricPreferenceLabelProvider.METRIC_CHECK:
                break;
            case MetricPreferenceLabelProvider.METRIC_NAME:
                text = ((MetricPreferenceContainer) element).getMetricName();
                break;
            case MetricPreferenceLabelProvider.METRIC_VAL:
                final Float val = ((MetricPreferenceContainer) element).getValue();
                if (val.isInfinite()) {
                    text = "--";
                } else {
                    text = val.toString();
                }
                break;
            default:
                final RuntimeException exception = new ArrayIndexOutOfBoundsException(
                        "Wrong column value for MetricPreferenceLabelProvider class : "
                                + this.getType());
                LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                        exception);
                MessageDialog.openError(
                        PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                        "Internal Error", "Contact support service : \n" + exception.getMessage());
        }

        LOGGER.finest("End getText method");
        return text;
    }
}
