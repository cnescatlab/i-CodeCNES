/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferences.checkerstables;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CheckboxCellEditor;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;

import fr.cnes.analysis.tools.ui.logger.UILogger;
import fr.cnes.analysis.tools.ui.preferences.CheckerPreferencesContainer;
import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;

/**
 * Editing support for Enabled cells.
 */
public class EnabledEditingSupport extends EditingSupport {
    /** Class name **/
    private static final String CLASS = EnabledEditingSupport.class.getName();

    /** Column viewer containing the cell */
    private final ColumnViewer viewer;
    /** TableViewer containing the column */
    private final CheckersComposite checkerTableViewer;

    /**
     * @param pViewer
     *            Column viewer containing the cell
     * @param pCheckerTableViewer
     *            TableViewer containing the column
     */
    public EnabledEditingSupport(final TableViewer pViewer,
                    final CheckersComposite pCheckerTableViewer) {
        super(pViewer);
        final String method = "EnabledEditingSupport";
        UILogger.entering(CLASS, method, new Object[] {
            pViewer, pCheckerTableViewer
        });
        this.viewer = pViewer;
        this.checkerTableViewer = pCheckerTableViewer;
        UILogger.exiting(CLASS, method);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.EditingSupport#getCellEditor(java.lang.Object)
     */
    @Override
    protected CellEditor getCellEditor(Object element) {
        final String method = "getCellEditor";
        UILogger.entering(CLASS, method, element);
        final CellEditor editor = new CheckboxCellEditor(null, SWT.CHECK | SWT.READ_ONLY);
        UILogger.exiting(CLASS, method, editor);
        return editor;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.EditingSupport#canEdit(java.lang.Object)
     */
    @Override
    protected boolean canEdit(Object element) {
        final String method = "canEdit";
        UILogger.entering(CLASS, method, element);
        return UserPreferencesService.isDefaultConfigurationActive();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.EditingSupport#getValue(java.lang.Object)
     */
    @Override
    protected Object getValue(Object element) {
        final String method = "getValue";
        UILogger.entering(CLASS, method, element);
        final Object value;
        if (UserPreferencesService.isDefaultConfigurationActive()) {
            value = Boolean.valueOf(((CheckerPreferencesContainer) element).isChecked());
        } else {
            value = Boolean.valueOf(UserPreferencesService
                            .isEnabledChecker(((CheckerPreferencesContainer) element).getId()));
        }
        UILogger.exiting(CLASS, method, value);
        return value;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.EditingSupport#setValue(java.lang.Object,
     * java.lang.Object)
     */
    @Override
    protected void setValue(Object element, Object value) {
        final String method = "setValue";
        UILogger.entering(CLASS, method, new Object[] {
            element, value
        });
        ((CheckerPreferencesContainer) element).setChecked(((Boolean) value).booleanValue());

        if (!((Boolean) value).booleanValue()) {
            this.checkerTableViewer.setAllEnabledChecker(false);
        }
        viewer.refresh();
        UILogger.exiting(CLASS, method);

    }

}
