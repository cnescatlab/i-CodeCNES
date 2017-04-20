/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import fr.cnes.analysis.tools.ui.exception.UnknownInstanceException;
import fr.cnes.analysis.tools.ui.view.AbstractLabelProvider;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.descriptor.FunctionRuleDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.descriptor.IRuleDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.descriptor.RuleDescriptor;

/**
 * This class provides column for the table viewer.
 * 
 */
public class RuleTreeViewerLabelProvider extends AbstractLabelProvider {
    /** Static values that determines column types. **/
    /** This value is for rule criticity column. **/
    public final static int CRITICITY = 0;
    /** This value is for rule name column. **/
    public final static int NAME = 1;
    /** This value is for error's line column. **/
    public final static int LINE = 2;
    /** This value is for number of violations column. **/
    public final static int NB_VIOL = 3;
    /** This value is for location column. **/
    public final static int MESSAGE = 4;

    /** Logger **/
    private final static Logger LOGGER = Logger
            .getLogger(RuleTreeViewerLabelProvider.class.getName());

    /**
     * Constructor with integer parameter which represents the column created.
     * 
     * @param pType
     *            the column to create
     */
    public RuleTreeViewerLabelProvider(final int pType) {
        super(pType);
    }

    /**
     * This functions set the text for each element of a column.
     * 
     * @param element
     *            the element store in the column
     * @return the text to store in column case
     */
    @Override
    public String getText(final Object element) {
        LOGGER.finest("Begin getText method");

        String text = "";
        if (element instanceof IRuleDescriptor) {
            switch (this.getType()) {
                case CRITICITY:
                    break;
                case NAME:
                    if (element instanceof FunctionRuleDescriptor) {
                        text = ((FunctionRuleDescriptor) element).getLocation();
                    } else {
                        text = ((IRuleDescriptor) element).getName();
                    }
                    break;
                case LINE:
                    if (element instanceof FunctionRuleDescriptor) {
                        text = ((IRuleDescriptor) element).getValue().toString();
                    } else {
                        text = "--";
                    }
                    break;
                case NB_VIOL:
                    if (element instanceof FunctionRuleDescriptor) {
                        text = "--";
                    } else {
                        text = ((IRuleDescriptor) element).getValue().toString();
                    }
                    break;
                case MESSAGE:
                    if (element instanceof FunctionRuleDescriptor) {
                        text = ((FunctionRuleDescriptor) element).getMessage();
                    }
                    break;
                default:
                    final RuntimeException exception = new ArrayIndexOutOfBoundsException(
                            "Wrong column value for ViolationsLabelProvider class : "
                                    + this.getType());
                    LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                            exception);
                    MessageDialog.openError(
                            PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            "Internal Error",
                            "Contact support service : \n" + exception.getMessage());
                    break;
            }
        } else {
            final UnknownInstanceException exception = new UnknownInstanceException(
                    "getText method of ViolationsLabelProvider class has a "
                            + element.getClass().getName()
                            + " element, but it should be an IRuleDescriptor instance.");
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Internal Error", "Contact support service : \n" + exception.getMessage());
        }

        LOGGER.finest("End getText method");
        return text;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ColumnLabelProvider#getImage(java.lang.Object)
     */
    @Override
    public Image getImage(final Object element) {
        LOGGER.finest("Begin getImage method");

        Image image = null;
        if (this.getType() == CRITICITY && element instanceof RuleDescriptor) {
            if (((RuleDescriptor) element).getCriticity().contains("Error")) {
                image = AbstractUIPlugin.imageDescriptorFromPlugin("fr.cnes.analysis.tools.ui",
                        "icons/logo-i-code-rouge-16x16.png").createImage();
            } else {
                image = AbstractUIPlugin.imageDescriptorFromPlugin("fr.cnes.analysis.tools.ui",
                        "icons/logo-i-code-orange-16x16.png").createImage();
            }
        }

        LOGGER.finest("End getImage method");
        return image;
    }
}
