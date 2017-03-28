/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferencepage.metrics;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CheckboxCellEditor;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.ui.PlatformUI;

/**
 * This class is the editing support for metric preferences. The first column
 * contains check boxes which are editable, second column is non editable
 * text(metric name), third and fourth column are editable text which represents
 * float values (respectively minimum and maximum).
 * 
 * 
 */
public class MetricEditingSupport extends EditingSupport {
    /** Logger. **/
    private static final Logger LOGGER = Logger.getLogger(MetricEditingSupport.class.getName());

    /** Table viewer to edit. **/
    private transient final TableViewer viewer;
    /** Cell editor. **/
    private CellEditor editor;
    /** Column's number. **/
    private transient final int type;

    /**
     * Constructor with the viewer and integer representation of the column.
     * 
     * @param pViewer
     *            the viewer
     * @param pType
     *            the column's number
     */
    public MetricEditingSupport(final TableViewer pViewer, final int pType) {
        super(pViewer);
        this.viewer = pViewer;
        this.type = pType;
    }

    /**
     * Getter for the viewer.
     * 
     * @return the viewer
     */
    @Override
    public TableViewer getViewer() {
        return this.viewer;
    }

    /**
     * Getter for the editor.
     * 
     * @return the editor
     */
    public CellEditor getEditor() {
        return this.editor;
    }

    /**
     * Getter for the type.
     * 
     * @return the type
     */
    public int getType() {
        return this.type;
    }

    /**
     * Setter for the editor.
     * 
     * @param pEditor
     *            the editor to set
     */
    public void setEditor(final CellEditor pEditor) {
        this.editor = pEditor;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.EditingSupport#getCellEditor(java.lang.Object)
     */
    @Override
    protected CellEditor getCellEditor(final Object element) {
        LOGGER.finest("Begin getCellEditor method");

        if (this.editor == null) {
            switch (this.type) {
                case MetricPreferenceLabelProvider.METRIC_CHECK:
                    this.editor = new CheckboxCellEditor(this.viewer.getTable(), SWT.CHECK);
                    break;
                case MetricPreferenceLabelProvider.METRIC_NAME:
                    this.editor = new TextCellEditor(this.viewer.getTable());
                    break;
                case MetricPreferenceLabelProvider.METRIC_VAL:
                    this.editor = new TextCellEditor(this.viewer.getTable());
                    break;
                default:
                    final RuntimeException exception = new ArrayIndexOutOfBoundsException(
                            "Wrong column value for MetricEditingSupport class : "
                                    + this.getType());
                    LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                            exception);
                    MessageDialog.openError(
                            PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            "Internal Error",
                            "Contact support service : \n" + exception.getMessage());
            }
        }

        LOGGER.finest("End getCellEditor method");
        return this.editor;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.EditingSupport#canEdit(java.lang.Object)
     */
    @Override
    protected boolean canEdit(final Object element) {
        return (this.type != MetricPreferenceLabelProvider.METRIC_NAME
                && PlatformUI.getPreferenceStore().getInt(".Level") == 4);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.EditingSupport#getValue(java.lang.Object)
     */
    @Override
    protected Object getValue(final Object element) {
        LOGGER.finest("Begin getValue method");

        Object value = null;
        switch (this.type) {
            case MetricPreferenceLabelProvider.METRIC_CHECK:
                value = ((MetricPreferenceContainer) element).getMetricCheckButton().getSelection();
                break;
            case MetricPreferenceLabelProvider.METRIC_VAL:
                final Float min = ((MetricPreferenceContainer) element).getValue();
                if (min.isInfinite()) {
                    value = "--";
                } else {
                    value = min.toString();
                }
                break;
            default:
                final RuntimeException exception = new ArrayIndexOutOfBoundsException(
                        "Wrong column value for MetricEditingSupport class : " + this.getType());
                LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                        exception);
                MessageDialog.openError(
                        PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                        "Internal Error", "Contact support service : \n" + exception.getMessage());
        }

        LOGGER.finest("End getValue method");
        return value;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.EditingSupport#setValue(java.lang.Object,
     * java.lang.Object)
     */
    @Override
    protected void setValue(final Object element, final Object value) {
        LOGGER.finest("Begin setValue method");

        switch (this.type) {
            case MetricPreferenceLabelProvider.METRIC_CHECK:
                ((MetricPreferenceContainer) element).getMetricCheckButton()
                        .setSelection(((Boolean) value).booleanValue());
                break;
            case MetricPreferenceLabelProvider.METRIC_VAL:
                if ("".equals(value)) {
                    ((MetricPreferenceContainer) element).setValue(Float.NEGATIVE_INFINITY);
                } else {
                    try {
                        ((MetricPreferenceContainer) element)
                                .setValue(Float.valueOf((String) value));
                    } catch (final NumberFormatException exception) {
                        ((MetricPreferenceContainer) element)
                                .setValue(((MetricPreferenceContainer) element).getValue());
                    }

                }
                break;
            default:
                final RuntimeException exception = new ArrayIndexOutOfBoundsException(
                        "Wrong column value for MetricEditingSupport class : " + this.getType());
                LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                        exception);
                MessageDialog.openError(
                        PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                        "Internal Error", "Contact support service : \n" + exception.getMessage());
        }
        this.viewer.update(element, null);

        LOGGER.finest("End setValue method");
    }
}
