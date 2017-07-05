/**
 * 
 */
package fr.cnes.analysis.tools.ui.preferences.checkerstables;

import fr.cnes.analysis.tools.ui.preferences.CheckerPreferencesContainer;
import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CheckboxCellEditor;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;

/**
 *
 */
public class EnabledEditingSupport extends EditingSupport {

    private final ColumnViewer viewer;
    private CheckerTableViewer checkerTableViewer;

    /**
     * @param pViewer
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
        if (UserPreferencesService.isDefaultConfigurationActive()) {
            return ((CheckerPreferencesContainer) element).isChecked();
        } else {
            return UserPreferencesService
                    .isEnabledChecker(((CheckerPreferencesContainer) element).getId());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.EditingSupport#setValue(java.lang.Object,
     * java.lang.Object)
     */
    @Override
    protected void setValue(Object element, Object value) {
        ((CheckerPreferencesContainer) element).setChecked((Boolean) value);

        if (!(Boolean) value) {
            this.checkerTableViewer.setAllEnabledChecker(false);
        }
        viewer.refresh();

    }

}
