/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;
import fr.cnes.analysis.tools.ui.exception.UnknownInstanceException;
import fr.cnes.analysis.tools.ui.images.ImageFactory;
import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;
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
    public final static int SEVERITY = 0;
    /** This value is for rule name column. **/
    public final static int NAME = 1;
    /** This value is for error's line column. **/
    public final static int LINE = 2;
    /** This value is for number of violations column. **/
    public final static int NB_VIOL = 3;
    /** This value is for location column. **/
    public final static int MESSAGE = 4;
    /** Class name */
    private static final String CLASS = RuleTreeViewerLabelProvider.class.getName();

    /**
     * Constructor with integer parameter which represents the column created.
     * 
     * @param pType
     *            the column to create
     */
    public RuleTreeViewerLabelProvider(final int pType) {
        super(pType);
        final String method = "RuleTreeViewerLabelProvider";
        ICodeLogger.entering(CLASS, method, Integer.valueOf(pType));
        ICodeLogger.exiting(CLASS, method);
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
        final String method = "getText";
        ICodeLogger.entering(CLASS, method, element);
        String text = "";
        if (element instanceof IRuleDescriptor) {
            switch (this.getType()) {
            case SEVERITY:
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
                ICodeLogger.error(CLASS, method, exception);
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
            ICodeLogger.error(CLASS, method, exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                            "Internal Error",
                            "Contact support service : \n" + exception.getMessage());
        }

        ICodeLogger.exiting(CLASS, method, text);
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
        final String method = "getImage";
        ICodeLogger.entering(CLASS, method, element);
        Image image = null;
        if (this.getType() == SEVERITY && element instanceof RuleDescriptor) {
            switch (((RuleDescriptor) element).getSeverity()) {
            case UserPreferencesService.PREF_SEVERITY_ERROR_VALUE:
                image = ImageFactory.getImage(ImageFactory.ERROR_SMALL);
                break;
            case UserPreferencesService.PREF_SEVERITY_WARNING_VALUE:
                image = ImageFactory.getImage(ImageFactory.WARNING_SMALL);
                break;
            case UserPreferencesService.PREF_SEVERITY_INFO_VALUE:
            default:
                image = ImageFactory.getImage(ImageFactory.INFO_SMALL);
                break;
            }
        }

        ICodeLogger.exiting(CLASS, method, image);
        return image;
    }
}
