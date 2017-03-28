/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.metrics;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.ui.exception.UnknownInstanceException;
import fr.cnes.analysis.tools.ui.view.AbstractLabelProvider;

/**
 * This class provides column for the metric tree viewer.
 * 
 * 
 */
public class MetricLabelProvider extends AbstractLabelProvider {
    /** Logger **/
    private final static Logger LOGGER = Logger.getLogger(MetricLabelProvider.class.getName());

    /** Static values that determines column types. **/
    /** This value is for metric name column. **/
    public final static int METRIC_NAME = 0;
    /** This value is for metric value column. **/
    public final static int METRIC_VALUE = 1;
    /** This value is for mean value column. **/
    public final static int MEAN = 2;
    /** This value is for minimum value column. **/
    public final static int MINIMUM = 3;
    /** This value is for maximum value column. **/
    public final static int MAXIMUM = 4;
    /** This value is for minimum resource cause column. **/
    public final static int MIN_CAUSE = 5;
    /** This value is for maximum resource cause column. **/
    public final static int MAX_CAUSE = 6;

    /**
     * Constructor with integer parameter which represents the column created.
     * 
     * @param pType
     *            the column to create
     */
    public MetricLabelProvider(final int pType) {
        super(pType);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ColumnLabelProvider#getForeground(java.lang
     * .Object)
     */
    @Override
    public Color getForeground(final Object element) {
        LOGGER.finest("Begin getForeground method");

        Color color = Display.getCurrent().getSystemColor(SWT.COLOR_BLUE);
        if (element instanceof IMetricDescriptor) {
            if (!((IMetricDescriptor) element).hasRightValue()) {
                color = Display.getCurrent().getSystemColor(SWT.COLOR_RED);
            }
        } else {
            final UnknownInstanceException exception = new UnknownInstanceException(
                    "getForeground method of MetricLabelProvider class has a "
                            + element.getClass().getName()
                            + " element, but it should be a Viewable instance.");
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Internal Error", "Contact support service : \n" + exception.getMessage());
        }

        LOGGER.finest("End getForeground method");
        return color;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ColumnLabelProvider#getText(java.lang.Object)
     */
    @Override
    public String getText(final Object element) {
        LOGGER.finest("Begin getText method");

        String text = "";
        if (element instanceof IMetricDescriptor) {
            switch (this.getType()) {
                case METRIC_NAME:
                    text = ((IMetricDescriptor) element).getName();
                    break;
                case METRIC_VALUE:
                    if (((IMetricDescriptor) element).getValue().isInfinite()
                            || ((IMetricDescriptor) element).getValue().isNaN()) {
                        text = "--";
                    } else {
                        text = ((IMetricDescriptor) element).getValue().toString();
                    }
                    break;
                case MEAN:
                    if (((IMetricDescriptor) element).getValue().isInfinite()
                            || ((IMetricDescriptor) element).getMean().isNaN()) {
                        text = "--";
                    } else {
                        text = ((IMetricDescriptor) element).getMean().toString();
                    }
                    break;
                case MINIMUM:
                    if (((IMetricDescriptor) element).getValue().isInfinite()
                            || ((IMetricDescriptor) element).getMinimum().isNaN()) {
                        text = "--";
                    } else {
                        text = ((IMetricDescriptor) element).getMinimum().toString();
                    }
                    break;
                case MAXIMUM:
                    if (((IMetricDescriptor) element).getValue().isInfinite()
                            || ((IMetricDescriptor) element).getMaximum().isNaN()) {
                        text = "--";
                    } else {
                        text = ((IMetricDescriptor) element).getMaximum().toString();
                    }
                    break;
                case MIN_CAUSE:
                    text = ((IMetricDescriptor) element).getMinCause();
                    break;
                case MAX_CAUSE:
                    text = ((IMetricDescriptor) element).getMaxCause();
                    break;
                default:
                    final RuntimeException exception = new ArrayIndexOutOfBoundsException(
                            "Wrong column value for MetricLabelProvider class : " + this.getType());
                    LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                            exception);
                    MessageDialog.openError(
                            PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            "Internal Error",
                            "Contact support service : \n" + exception.getMessage());
            }
        } else {
            final UnknownInstanceException exception = new UnknownInstanceException(
                    "getText method of MetricLabelProvider class has a "
                            + element.getClass().getName()
                            + " element, but it should be a Viewable instance.");
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Internal Error", "Contact support service : \n" + exception.getMessage());
        }

        LOGGER.finest("End getText method");
        return text;
    }
}
