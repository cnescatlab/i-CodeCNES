/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferencepage;

import java.util.logging.Logger;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableEditor;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.TableItem;

import fr.cnes.analysis.tools.ui.preferencepage.rules.RulePreferenceContainer;

/**
 * This is a label provider for AbstractEvaluationPreference classes. This class
 * only provides the first columns, which contains check boxes.
 * 
 * 
 */
public class PreferenceLabelProvider extends ColumnLabelProvider {
    /** Static values that determines column types. **/
    /** Column containing the check box **/
    public static final int CHECKED = 0;

    /** Column containing preference name **/
    public static final int NAME = 1;

    /** Logger **/
    private static final Logger LOGGER = Logger.getLogger(PreferenceLabelProvider.class.getName());

    /** The viewer on which this label provider is applied **/
    private final transient TableViewer viewer;

    /** An integer to determine which column has to be provide. **/
    private int type;

    /**
     * Constructor with integer parameter which represents the column created
     * and the viewer on which the column is created.
     * 
     * @param pType
     *            the column to create
     * @param pViewer
     *            the viewer
     */
    public PreferenceLabelProvider(final int pType, final TableViewer pViewer) {
        super();
        this.type = pType;
        this.viewer = pViewer;
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
     * Getter for the table viewer.
     * 
     * @return the table viewer
     */
    public int getTableViewer() {
        return this.type;
    }

    /**
     * Setter for the type.
     * 
     * @param pType
     *            the type to set
     */
    public void setType(final int pType) {
        this.type = pType;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ColumnLabelProvider#update(org.eclipse.jface.
     * viewers.ViewerCell)
     */
    @Override
    public void update(final ViewerCell cell) {
        LOGGER.finest("Begin update method");

        if (this.type == PreferenceLabelProvider.CHECKED) {
            final TableItem item = (TableItem) cell.getItem();
            final RulePreferenceContainer container = (RulePreferenceContainer) item.getData();

            final TableEditor editor = new TableEditor(this.viewer.getTable());
            final Button button = container.getRuleCheckButton();
            button.pack();
            editor.minimumWidth = button.getSize().x;
            editor.horizontalAlignment = SWT.LEFT;
            editor.setEditor(button, item, 0);
        } else {
            super.update(cell);
        }

        LOGGER.finest("End update method");
    }
}
