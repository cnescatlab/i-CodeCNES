/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.handler;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.commands.IHandlerListener;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.ui.view.ViolationsView;

/**
 * This Handler is being used by {@link ViolationsView} to dispose and show
 * different tree viewers available for the user.
 * 
 * @version 2.0
 * @since 2.0
 */
public class ShowRuleTreeViewerHandler implements IHandler {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#addHandlerListener(org.eclipse.core.
     * commands.IHandlerListener)
     */
    @Override
    public void addHandlerListener(IHandlerListener handlerListener) {
        // Do nothing
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.IHandler#dispose()
     */
    @Override
    public void dispose() {
        // Do nothing.
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        final ViolationsView view = (ViolationsView) PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getActivePage()
                        .findView(ViolationsView.VIEW_ID);
        final String name = event.getParameter("TreeViewer");
        if (!view.getTreeViewerType().equals(name)) {
            view.setTreeViewerType(name);
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.IHandler#isEnabled()
     */
    @Override
    public boolean isEnabled() {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.IHandler#isHandled()
     */
    @Override
    public boolean isHandled() {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#removeHandlerListener(org.eclipse.core
     * .commands.IHandlerListener)
     */
    @Override
    public void removeHandlerListener(IHandlerListener handlerListener) {
        // Do nothing
    }

}
