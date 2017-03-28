/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferencepage.rules;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CheckboxCellEditor;
import org.eclipse.jface.viewers.ComboBoxCellEditor;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.ui.PlatformUI;

/**
 * This class is the editing support for rule preferences. The first column
 * contains check boxes which are editable, second column is non editable
 * text(metric name) and third is editable text which represents the criticity
 * (Error or Warning).
 * 
 */
public class RuleEditingSupport extends EditingSupport {

    /** Logger. **/
    private static final Logger LOGGER = Logger.getLogger(RuleEditingSupport.class.getName());

    /** The critical level "Error". **/
    private static final String ERROR = "Error";

    /** The critical level "Warning". **/
    private static final String WARNING = "Warning";

    /** Table viewer to edit. **/
    private final transient TableViewer viewer;

    /** Cell editor. **/
    private CellEditor editor;

    /** Column's number. **/
    private final transient int type;

    /**
     * Constructor with the viewer and integer representation of the column.
     * 
     * @param pViewer
     *            the viewer
     * @param pType
     *            the column's number
     */
    public RuleEditingSupport(final TableViewer pViewer, final int pType) {
        super(pViewer);
        this.viewer = pViewer;
        this.type = pType;
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
                case RulePreferenceLabelProvider.RULE_CHECK:
                    this.editor = new CheckboxCellEditor(this.viewer.getTable(), SWT.CHECK);
                    break;
                case RulePreferenceLabelProvider.RULE_NAME:
                    this.editor = new TextCellEditor(this.viewer.getTable());
                    break;
                case RulePreferenceLabelProvider.RULE_CRITICITY:
                    final String[] comboBox = { ERROR, WARNING };
                    this.editor = new ComboBoxCellEditor(this.viewer.getTable(), comboBox);
                    break;
                default:
                    final RuntimeException exception = new ArrayIndexOutOfBoundsException(
                            "Wrong column value for RuleEditingSupport class : " + this.getType());
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
        return (this.type != RulePreferenceLabelProvider.RULE_NAME);
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
            case RulePreferenceLabelProvider.RULE_CHECK:
                value = ((RulePreferenceContainer) element).isChecked();
                break;
            case RulePreferenceLabelProvider.RULE_CRITICITY:
                final String criticity = ((RulePreferenceContainer) element).getRuleCriticity();
                if (ERROR.equals(criticity)) {
                    value = 0;
                } else {
                    value = 1;
                }
                break;
            default:
                final RuntimeException exception = new ArrayIndexOutOfBoundsException(
                        "Wrong column value for RuleEditingSupport class : " + this.getType());
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
            case RulePreferenceLabelProvider.RULE_CHECK:
                ((RulePreferenceContainer) element).setChecked(((Boolean) value).booleanValue());
                break;
            case RulePreferenceLabelProvider.RULE_CRITICITY:
                if ((Integer) value == 0) {
                    ((RulePreferenceContainer) element).setRuleCriticity(ERROR);
                } else {
                    ((RulePreferenceContainer) element).setRuleCriticity(WARNING);
                }
                break;
            default:
                final RuntimeException exception = new ArrayIndexOutOfBoundsException(
                        "Wrong column value for RuleEditingSupport class : " + this.getType());
                LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                        exception);
                MessageDialog.openError(
                        PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                        "Internal Error", "Contact support service : \n" + exception.getMessage());
        }
        this.viewer.update(element, null);

        LOGGER.finest("End setValue method");
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
}
