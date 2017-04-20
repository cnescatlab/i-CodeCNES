/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.ui.exception.UnknownInstanceException;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.descriptor.FileRuleDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.descriptor.FunctionRuleDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.descriptor.RuleDescriptor;

/**
 * This class provides a content provider for the tree viewer in the metric
 * view.
 * 
 * @see org.eclipse.jface.viewers.ITreeContentProvider
 */
public class RuleTreeViewerContentProvider implements ITreeContentProvider {
    /** Logger **/
    private final static Logger LOGGER = Logger.getLogger(RuleTreeViewerContentProvider.class.getName());

    /** The original inputs. **/
    private ViolationToRuleTreeViewerConverter converter;

    /**
     * Empty constructor.
     */
    public RuleTreeViewerContentProvider() {
        super();
        this.converter = new ViolationToRuleTreeViewerConverter();
    }

    /**
     * Getter for the converter.
     * 
     * @return the converter
     */
    public ViolationToRuleTreeViewerConverter getConverter() {
        return this.converter;
    }

    /**
     * Setter for the converter.
     * 
     * @param pConverter
     *            the converter to set
     */
    public void setConverter(final ViolationToRuleTreeViewerConverter pConverter) {
        this.converter = pConverter;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.
     * jface .viewers.Viewer, java.lang.Object, java.lang.Object)
     */
    @Override
    public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
        LOGGER.finest("Begin inputChanged method");

        try {
            if (newInput instanceof Violation[]) {
                this.converter.setInputs(((Violation[]) newInput).clone());
                // run analysis
                this.converter.setUser(true);
                this.converter.schedule();
                this.converter.join();

            } else if (newInput != null) {
                final UnknownInstanceException exception = new UnknownInstanceException(
                        "inputChanged method of AbstractContentProvider has a "
                                + newInput.getClass().getName()
                                + " type instead of a Descriptor<?>[] instance");
                LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                        exception);
                MessageDialog.openError(
                        PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                        "Internal Error", "Contact support service : \n" + exception.getMessage());
            }
        } catch (final InterruptedException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Internal Error", "Contact support service : \n" + exception.getMessage());
        }

        LOGGER.finest("End inputChanged method");
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
        LOGGER.finest("Begin getElements method");

        final Object[] elements = this.converter.getContainer();

        LOGGER.finest("End getElements method");
        return elements;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.IContentProvider#dispose()
     */
    @Override
    public void dispose() {
        LOGGER.finest("Begin dispose method");

        if (this.converter != null) {
            this.converter.setContainer(new RuleDescriptor[0]);
        }

        LOGGER.finest("End dispose method");
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
        return !(element instanceof FunctionRuleDescriptor);
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
        return null;
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
        LOGGER.finest("Begin getChildren method");

        Object[] values = null;
        if (parentElement instanceof FileRuleDescriptor) {

            // The parent element can be a FileValue : we find array of
            // function values depending
            final List<FunctionRuleDescriptor> mVals = ((FileRuleDescriptor) parentElement)
                    .getDescriptors();
            values = mVals.toArray(new FunctionRuleDescriptor[mVals.size()]);
        } else if (parentElement instanceof RuleDescriptor) {

            // A Descriptor : we find array of file values depending
            final List<FileRuleDescriptor> mVals = ((RuleDescriptor) parentElement)
                    .getDescriptors();
            values = mVals.toArray(new FileRuleDescriptor[mVals.size()]);
        } else if (parentElement instanceof RuleDescriptor[]) {

            // An array of descriptors : we find each descriptor depending
            values = (RuleDescriptor[]) parentElement;
        } else if (!(parentElement instanceof FunctionRuleDescriptor)) {

            // Otherwise, an error is thrown on the interface
            final UnknownInstanceException exception = new UnknownInstanceException(
                    "Unknow type in getChildren method of AbstractContentProvider : "
                            + parentElement.getClass().getName());
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Internal Error", "Contact support service : \n" + exception.getMessage());
        }

        LOGGER.finest("End getChildren method");
        return values;
    }
}
