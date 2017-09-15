/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.metrics;

import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;
import fr.cnes.analysis.tools.ui.exception.UnknownInstanceException;

/**
 * This class provides a content provider for the tree viewer in the metric
 * view.
 * 
 * @see org.eclipse.jface.viewers.ITreeContentProvider
 */
public class MetricContentProvider implements ITreeContentProvider {
    /** Class name **/
    private static final String CLASS = MetricContentProvider.class.getName();

    /** A value container which has all values of metrics. **/
    private MetricConverter converter;

    /**
     * Getter for the converter
     * 
     * @return the converter
     */
    public MetricConverter getConverter() {
        final String method = "getConverter";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, converter);
        return this.converter;
    }

    /**
     * Setter for the converter
     * 
     * @param pConverter
     *            the converter to set
     */
    public void setConverter(final MetricConverter pConverter) {
        final String method = "setConverter";
        ICodeLogger.entering(CLASS, method, pConverter);
        this.converter = pConverter;
        ICodeLogger.exiting(CLASS, method);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.
     * Object)
     */
    @Override
    public boolean hasChildren(final Object element) {
        final String method = "hasChildren";
        ICodeLogger.entering(CLASS, method, element);

        // Every type has a child except for FunctionValue type
        final boolean result;
        if (element instanceof FunctionMetricDescriptor) {
            result = false;
        } else if (element instanceof FileMetricDescriptor) {
            if (((FileMetricDescriptor) element).getDescriptors().isEmpty()) {
                result = false;
            } else {
                result = true;
            }
        } else {
            result = true;
        }
        ICodeLogger.exiting(CLASS, method, Boolean.valueOf(result));
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object
     * )
     */
    @Override
    public Object getParent(final Object element) {
        final String method = "getParent";
        ICodeLogger.entering(CLASS, method, element);
        ICodeLogger.exiting(CLASS, method, null);
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.
     * jface .viewers.Viewer, java.lang.Object, java.lang.Object)
     */
    @Override
    public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
        final String method = "inputChanged";
        ICodeLogger.entering(CLASS, method, new Object[] {
            viewer, oldInput, newInput
        });

        try {
            if (newInput instanceof CheckResult[]) {
                this.converter = new MetricConverter(((CheckResult[]) newInput).clone());
                // run analysis
                this.converter.setUser(true);
                this.converter.schedule();
                this.converter.join();

            } else if (newInput != null) {
                final UnknownInstanceException exception = new UnknownInstanceException(
                                "inputChanged method of AbstractContentProvider has a "
                                                + newInput.getClass().getName()
                                                + " type instead of a Descriptor<?>[] instance");
                ICodeLogger.throwing(CLASS, method, exception);
                MessageDialog.openError(
                                PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                                "Internal Error",
                                "Contact support service : \n" + exception.getMessage());
            }
        } catch (final InterruptedException exception) {
            ICodeLogger.throwing(CLASS, method, exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            "Internal Error",
                            "Contact support service : \n" + exception.getMessage());
        }
        ICodeLogger.exiting(CLASS, method);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ITreeContentProvider#getElements(java.lang.
     * Object)
     */
    @Override
    public Object[] getElements(final Object inputElement) {
        final String method = "getElements";
        ICodeLogger.entering(CLASS, method, inputElement);
        final Object[] elements = this.converter.getContainer();
        ICodeLogger.exiting(CLASS, method, elements);
        return elements;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.
     * Object)
     */
    @Override
    public Object[] getChildren(final Object parentElement) {
        final String method = "getChildren";
        ICodeLogger.entering(CLASS, method, parentElement);
        Object[] values = new FunctionMetricDescriptor[0];
        if (parentElement instanceof FileMetricDescriptor) {
            // The parent element can be a FileValue : we find array of
            // function values depending
            final List<FunctionMetricDescriptor> mVals = ((FileMetricDescriptor) parentElement)
                            .getDescriptors();
            values = mVals.toArray(new FunctionMetricDescriptor[mVals.size()]);

        } else if (parentElement instanceof MetricDescriptor) {
            // A Descriptor : we find array of file values depending
            final List<FileMetricDescriptor> mVals = ((MetricDescriptor) parentElement)
                            .getDescriptors();
            values = mVals.toArray(new FileMetricDescriptor[mVals.size()]);

        } else if (parentElement instanceof MetricDescriptor[]) {
            // An array of descriptors : we find each descriptor depending
            values = (MetricDescriptor[]) parentElement;

        } else if (!(parentElement instanceof FunctionMetricDescriptor)) {
            // Otherwise, an error is thrown on the interface
            final UnknownInstanceException exception = new UnknownInstanceException(
                            "Unknow type in getChildren method of AbstractContentProvider : "
                                            + parentElement.getClass().getName());
            ICodeLogger.throwing(CLASS, method, exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            "Internal Error",
                            "Contact support service : \n" + exception.getMessage());
        }
        ICodeLogger.exiting(CLASS, method, values);
        return values;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.IContentProvider#dispose()
     */
    @Override
    public void dispose() {
        final String method = "dispose";
        ICodeLogger.entering(CLASS, method);
        if (this.converter != null) {
            this.converter.setContainer(new MetricDescriptor[0]);
        }
        ICodeLogger.exiting(CLASS, method);
    }
}
