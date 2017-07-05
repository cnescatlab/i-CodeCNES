/**
 * 
 */
package fr.cnes.analysis.tools.ui.preferences.checkerstables;

import fr.cnes.analysis.tools.ui.preferences.CheckerPreferencesContainer;
import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ComboBoxCellEditor;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.TableViewer;

/**
 *
 */
public class SeverityEditingSupport extends EditingSupport {

    private final TableViewer viewer;

    /**
     * @param pViewer
     */
    public SeverityEditingSupport(TableViewer pViewer) {
        super(pViewer);
        this.viewer = pViewer;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.EditingSupport#getCellEditor(java.lang.Object)
     */
    @Override
    protected CellEditor getCellEditor(Object element) {
        String[] severity = new String[3];
        severity[0] = UserPreferencesService.PREF_SEVERITY_INFO_VALUE;
        severity[1] = UserPreferencesService.PREF_SEVERITY_WARNING_VALUE;
        severity[2] = UserPreferencesService.PREF_SEVERITY_ERROR_VALUE;

        return new ComboBoxCellEditor(viewer.getTable(), severity);
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
        Integer severityCode;
        String severity;
        if (UserPreferencesService.isDefaultConfigurationActive()) {
            severity = ((CheckerPreferencesContainer) element).getSeverity();
        } else {
            severity = UserPreferencesService
                    .getCheckerSeverity(((CheckerPreferencesContainer) element).getId());
        }
        switch (severity) {
            case UserPreferencesService.PREF_SEVERITY_ERROR_VALUE:
                severityCode = 2;
                break;
            case UserPreferencesService.PREF_SEVERITY_WARNING_VALUE:
                severityCode = 1;
                break;
            case UserPreferencesService.PREF_SEVERITY_INFO_VALUE:
            default:
                severityCode = 0;
                break;
        }
        return severityCode;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.EditingSupport#setValue(java.lang.Object,
     * java.lang.Object)
     */
    @Override
    protected void setValue(Object element, Object value) {
        Integer severityCode = (Integer) value;
        String severity;
        switch (severityCode) {
            case 2:
                severity = UserPreferencesService.PREF_SEVERITY_ERROR_VALUE;
                break;
            case 1:
                severity = UserPreferencesService.PREF_SEVERITY_WARNING_VALUE;
                break;
            case 0:
            default:
                severity = UserPreferencesService.PREF_SEVERITY_INFO_VALUE;
                break;
        }
        ((CheckerPreferencesContainer) element).setSeverity(severity);
        viewer.refresh();
    }

}
