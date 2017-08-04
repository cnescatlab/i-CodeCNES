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

import fr.cnes.analysis.tools.ui.preferences.CheckerPreferencesContainer;
import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;

/**
 * Editing support for Enabled cells.
 */
public class EnabledEditingSupport extends EditingSupport {

    /** Column viewer containing the cell */
    private final ColumnViewer viewer;
    /** TableViewer containing the column */
    private CheckerTableViewer checkerTableViewer;

    /**
     * @param pViewer
     *            Column viewer containing the cell
     * @param pCheckerTableViewer
     *            TableViewer containing the column
     */
    public EnabledEditingSupport(TableViewer pViewer, CheckerTableViewer pCheckerTableViewer) {
        super(pViewer);
        this.viewer = pViewer;
        this.checkerTableViewer = pCheckerTableViewer;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.EditingSupport#getCellEditor(java.lang.Object)
     */
    @Override
    protected CellEditor getCellEditor(Object element) {
        return new CheckboxCellEditor(null, SWT.CHECK | SWT.READ_ONLY);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.EditingSupport#canEdit(java.lang.Object)
     */
    @Override
    protected boolean canEdit(Object element) {
        return UserPreferencesService.isDefaultConfigurationActive();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.EditingSupport#getValue(java.lang.Object)
     */
    @Override
    protected Object getValue(Object element) {
        final Object value;
        if (UserPreferencesService.isDefaultConfigurationActive()) {
            value = Boolean.valueOf(((CheckerPreferencesContainer) element).isChecked());
        } else {
            value = Boolean.valueOf(UserPreferencesService
                            .isEnabledChecker(((CheckerPreferencesContainer) element).getId()));
        }
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
        ((CheckerPreferencesContainer) element).setChecked(((Boolean) value).booleanValue());

        if (!((Boolean) value).booleanValue()) {
            this.checkerTableViewer.setAllEnabledChecker(false);
        }
        viewer.refresh();

    }

}
