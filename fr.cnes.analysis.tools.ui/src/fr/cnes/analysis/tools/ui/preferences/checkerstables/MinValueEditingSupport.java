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

import fr.cnes.analysis.tools.ui.logger.UILogger;
import fr.cnes.analysis.tools.ui.preferences.CheckerPreferencesContainer;
import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;

/**
 * Editing support for minimum
 */
public class MinValueEditingSupport extends EditingSupport {

    /** Class name **/
    private static final String CLASS = MinValueEditingSupport.class.getName();

    /** Cell editor */
    private final CellEditor editor;

    /**
     * @param pViewer
     *            Table viewer containing the cell
     */
    public MinValueEditingSupport(final TableViewer pViewer) {
        super(pViewer);
        final String method = "MinValueEditingSupport";
        UILogger.entering(CLASS, method, pViewer);
        this.editor = new TextCellEditor(pViewer.getTable());
        UILogger.exiting(CLASS, method);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.EditingSupport#getCellEditor(java.lang.Object)
     */
    @Override
    protected CellEditor getCellEditor(final Object element) {
        final String method = "getCellEditor";
        UILogger.entering(CLASS, method, element);
        UILogger.exiting(CLASS, method, editor);
        return editor;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.EditingSupport#canEdit(java.lang.Object)
     */
    @Override
    protected boolean canEdit(final Object element) {
        final String method = "canEdit";
        UILogger.entering(CLASS, method, element);
        final boolean canEdit = UserPreferencesService.isDefaultConfigurationActive()
                        && ((CheckerPreferencesContainer) element).isMetric();
        UILogger.exiting(CLASS, method, Boolean.valueOf(canEdit));
        return canEdit;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.EditingSupport#getValue(java.lang.Object)
     */
    @Override
    protected Object getValue(final Object element) {
        final String method = "getValue";
        UILogger.entering(CLASS, method, element);
        final Object value;
        if (UserPreferencesService.isDefaultConfigurationActive()) {
            value = Float.toString(
                            ((CheckerPreferencesContainer) element).getMinValue().floatValue());
        } else {
            value = UserPreferencesService
                            .getMinValue(((CheckerPreferencesContainer) element).getId());
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
    protected void setValue(final Object element, final Object value) {
        final String method = "setValue";
        UILogger.entering(CLASS, method, element);
        try {
            ((CheckerPreferencesContainer) element).setMinValue(Float.parseFloat((String) value));
        } catch (@SuppressWarnings("unused") NullPointerException | NumberFormatException e) {
            ((CheckerPreferencesContainer) element).setMinValue(Float.NaN);
        }
        UILogger.exiting(CLASS, method);

    }

}
