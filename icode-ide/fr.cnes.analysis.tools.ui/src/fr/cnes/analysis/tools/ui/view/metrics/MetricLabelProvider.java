/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.metrics;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;
import fr.cnes.analysis.tools.ui.exception.UnknownInstanceException;
import fr.cnes.analysis.tools.ui.view.AbstractLabelProvider;

/**
 * This class provides column for the metric tree viewer.
 * 
 * 
 */
public class MetricLabelProvider extends AbstractLabelProvider {

    /** Static values that determines column types. **/
    /** This value is for metric name column. **/
    public static final int METRIC_NAME = 0;
    /** This value is for metric value column. **/
    public static final int METRIC_VALUE = 1;
    /** This value is for mean value column. **/
    public static final int MEAN = 2;
    /** This value is for minimum value column. **/
    public static final int MINIMUM = 3;
    /** This value is for maximum value column. **/
    public static final int MAXIMUM = 4;
    /** This value is for minimum resource cause column. **/
    public static final int MIN_CAUSE = 5;
    /** This value is for maximum resource cause column. **/
    public static final int MAX_CAUSE = 6;

    /** Class name **/
    private static final String CLASS = MetricLabelProvider.class.getName();

    /** Logger **/
    /** Nan value displaying */
    private static final String NAN_VALUE_DISPLAY = "--";

    /**
     * Constructor with integer parameter which represents the column created.
     * 
     * @param pType
     *            the column to create
     */
    public MetricLabelProvider(final int pType) {
        super(pType);
        final String method = "MetricLabelProvider";
        ICodeLogger.entering(CLASS, method, Integer.valueOf(pType));

        ICodeLogger.exiting(CLASS, method);
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
        final String method = "getForeground";
        ICodeLogger.entering(CLASS, method, element);
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
            ICodeLogger.throwing(CLASS, method, exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            "Internal Error",
                            "Contact support service : \n" + exception.getMessage());
        }

        ICodeLogger.exiting(CLASS, method, color);
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
        final String method = "getText";
        ICodeLogger.entering(CLASS, method, element);
        String text = "";
        if (element instanceof IMetricDescriptor) {
            switch (this.getType()) {
            case METRIC_NAME:
                text = ((IMetricDescriptor) element).getName();
                break;
            case METRIC_VALUE:
                if (((IMetricDescriptor) element).getValue().isInfinite()
                                || ((IMetricDescriptor) element).getValue().isNaN()) {
                    text = NAN_VALUE_DISPLAY;
                } else {
                    text = ((IMetricDescriptor) element).getValue().toString();
                }
                break;
            case MEAN:
                if (((IMetricDescriptor) element).getValue().isInfinite()
                                || ((IMetricDescriptor) element).getMean().isNaN()) {
                    text = NAN_VALUE_DISPLAY;
                } else {
                    text = ((IMetricDescriptor) element).getMean().toString();
                }
                break;
            case MINIMUM:
                if (((IMetricDescriptor) element).getValue().isInfinite()
                                || ((IMetricDescriptor) element).getMinimum().isNaN()) {
                    text = NAN_VALUE_DISPLAY;
                } else {
                    text = ((IMetricDescriptor) element).getMinimum().toString();
                }
                break;
            case MAXIMUM:
                if (((IMetricDescriptor) element).getValue().isInfinite()
                                || ((IMetricDescriptor) element).getMaximum().isNaN()) {
                    text = NAN_VALUE_DISPLAY;
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
                                "Wrong column value for MetricLabelProvider class : "
                                                + this.getType());
                ICodeLogger.throwing(CLASS, method, exception);
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
            ICodeLogger.throwing(CLASS, method, exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            "Internal Error",
                            "Contact support service : \n" + exception.getMessage());
        }

        ICodeLogger.exiting(CLASS, method, text);
        return text;
    }
}
