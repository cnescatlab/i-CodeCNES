/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferencepage.rules;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableEditor;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.ui.preferencepage.PreferenceLabelProvider;

/**
 * This class is the label provider for rule preference pages.
 * 
 */
public class RulePreferenceLabelProvider extends ColumnLabelProvider {
    /** Static values that determines column types. **/

    /** This value is for check box column. **/
    public static final int RULE_CHECK = 0;

    /** This value is for name column. **/
    public static final int RULE_NAME = 1;

    /** This value is for value column. **/
    public static final int RULE_CRITICITY = 2;

    /** Logger **/
    private static final Logger LOGGER = Logger
            .getLogger(RulePreferenceLabelProvider.class.getName());

    /** The viewer on which this label provider is applied **/
    private final transient TableViewer viewer;

    /** An integer to determine which column has to be provide. **/
    private int type;

    /**
     * Constructor with integer parameter which represents the column created
     * and the viewer which contains the column.
     * 
     * @param pType
     *            the column to create
     * @param pViewer
     *            the viewer
     */
    public RulePreferenceLabelProvider(final int pType, final TableViewer pViewer) {
        super();
        this.viewer = pViewer;
        this.type = pType;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ColumnLabelProvider#getText(java.lang.Object)
     */
    @Override
    public String getText(final Object element) {
        LOGGER.finest("Begin getText method");

        String text = null;

        switch (this.getType()) {
            case RulePreferenceLabelProvider.RULE_CHECK:
                break;
            case RulePreferenceLabelProvider.RULE_NAME:
                text = ((RulePreferenceContainer) element).getRuleName();
                break;
            case RulePreferenceLabelProvider.RULE_CRITICITY:
                text = ((RulePreferenceContainer) element).getRuleCriticity();
                break;
            default:
                final RuntimeException exception = new ArrayIndexOutOfBoundsException(
                        "Wrong column value for RulePreferenceLabelProvider class : "
                                + this.getType());
                LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                        exception);
                MessageDialog.openError(
                        PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                        "Internal Error", "Contact support service : \n" + exception.getMessage());
        }

        LOGGER.finest("End getText method");
        return text;
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

    /**
     * @return the type
     */
    public int getType() {
        return type;
    }

    /**
     * @param pType
     *            the type to set
     */
    public void setType(int pType) {
        this.type = pType;
    }

    /**
     * @return the viewer
     */
    public TableViewer getViewer() {
        return viewer;
    }

}
