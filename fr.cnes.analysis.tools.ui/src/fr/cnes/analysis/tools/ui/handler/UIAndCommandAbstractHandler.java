package fr.cnes.analysis.tools.ui.handler;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;

/**
 * This class is provided to manage both handler usage in UI or in a command mode In command mode,
 * it is not possible to display dialogs (no workbench available) It provides warning and error
 * methods available for all handlers This class should be used as base class for all handlers
 * 
 * @author olivier
 *
 */
public abstract class UIAndCommandAbstractHandler extends AbstractHandler {

  protected void showWarning(Shell s, String title, String message) {
    if (s != null) { // IN UI Mode
      MessageDialog.openWarning(s, title, message);
    } else {
      // In command mode -> no dialogs available
      System.out.println("WARN : " + title + " : " + message);
    }

  }

  protected static void showError(Shell s, String title, String message) {
    if (s != null) { // IN UI Mode
      MessageDialog.openWarning(s, title, message);
    } else {
      // In command mode -> no dialogs available
      System.out.println("ERROR : " + title + " : " + message);
    }

  }

}
