/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferences.checkerstables;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ComboBoxCellEditor;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.TableViewer;

import fr.cnes.analysis.tools.ui.preferences.CheckerPreferencesContainer;
import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;

/**
 * Editing support for Severity.
 */
public class SeverityEditingSupport extends EditingSupport {

    /** TableViewer containing the column */
    private final TableViewer viewer;

    /**
     * @param pViewer
     *            TableViewer containing the column
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
        final int options = 3;
        final String[] severity = new String[options];
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
        final Integer severityCode;
        final String severity;
        if (UserPreferencesService.isDefaultConfigurationActive()) {
            severity = ((CheckerPreferencesContainer) element).getSeverity();
        } else {
            severity = UserPreferencesService
                            .getCheckerSeverity(((CheckerPreferencesContainer) element).getId());
        }
        switch (severity) {
        case UserPreferencesService.PREF_SEVERITY_ERROR_VALUE:
            severityCode = Integer.valueOf(2);
            break;
        case UserPreferencesService.PREF_SEVERITY_WARNING_VALUE:
            severityCode = Integer.valueOf(1);
            break;
        case UserPreferencesService.PREF_SEVERITY_INFO_VALUE:
        default:
            severityCode = Integer.valueOf(0);
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
        final int severityCode = ((Integer) value).intValue();
        final String severity;
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
