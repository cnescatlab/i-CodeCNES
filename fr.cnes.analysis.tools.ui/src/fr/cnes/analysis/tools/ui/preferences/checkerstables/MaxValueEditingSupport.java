/**
 * 
 */
package fr.cnes.analysis.tools.ui.preferences.checkerstables;

import fr.cnes.analysis.tools.ui.preferences.CheckerPreferencesContainer;
import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;

/**
 * @author waldmao
 *
 */
public class MaxValueEditingSupport extends EditingSupport {

    private final ColumnViewer viewer;
    private final CellEditor editor;

    /**
     * @param pViewer
     */
    public MaxValueEditingSupport(TableViewer pViewer) {
        super(pViewer);
        this.viewer = pViewer;
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
        if (UserPreferencesService.isDefaultConfigurationActive()) {
            return Float.toString(((CheckerPreferencesContainer) element).getMaxValue());
        } else {
            return UserPreferencesService
                    .getMaxValue(((CheckerPreferencesContainer) element).getId());
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
        try {
            ((CheckerPreferencesContainer) element).setMaxValue(Float.parseFloat((String) value));
        } catch (NullPointerException | NumberFormatException e) {
            ((CheckerPreferencesContainer) element).setMaxValue(Float.NaN);
        }

    }

}
