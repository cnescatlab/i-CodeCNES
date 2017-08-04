/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferences.checkerstables;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;

import fr.cnes.analysis.tools.ui.preferences.CheckerPreferencesContainer;
import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;

/**
 * Editing support for minimum
 */
public class MinValueEditingSupport extends EditingSupport {

    /** Cell editor */
    private final CellEditor editor;

    /**
     * @param pViewer
     *            Table viewer containing the cell
     */
    public MinValueEditingSupport(TableViewer pViewer) {
        super(pViewer);
        this.editor = new TextCellEditor(pViewer.getTable());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.EditingSupport#getCellEditor(java.lang.Object)
     */
    @Override
    protected CellEditor getCellEditor(Object element) {
        return editor;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.EditingSupport#canEdit(java.lang.Object)
     */
    @Override
    protected boolean canEdit(Object element) {
        return UserPreferencesService.isDefaultConfigurationActive()
                        && ((CheckerPreferencesContainer) element).isMetric();
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
            value = Float.toString(
                            ((CheckerPreferencesContainer) element).getMinValue().floatValue());
        } else {
            value = UserPreferencesService
                            .getMinValue(((CheckerPreferencesContainer) element).getId());
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
        try {
            ((CheckerPreferencesContainer) element).setMinValue(Float.parseFloat((String) value));
        } catch (@SuppressWarnings("unused") NullPointerException | NumberFormatException e) {
            ((CheckerPreferencesContainer) element).setMinValue(Float.NaN);
        }

    }

}
