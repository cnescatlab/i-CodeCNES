package fr.cnes.analysis.tools.ui.handler;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.commands.IHandlerListener;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.ui.view.ViolationsView;

public class ShowRuleTreeViewerHandler implements IHandler {

    @Override
    public void addHandlerListener(IHandlerListener handlerListener) {
        // TODO Auto-generated method stub

    }

    @Override
    public void dispose() {
        // TODO Auto-generated method stub
    }

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        final ViolationsView view = (ViolationsView) PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage().findView(ViolationsView.VIEW_ID);
        final String name = event.getParameter("TreeViewer");
        if (!view.getTreeViewerType().equals(name)) {
            view.setTreeViewerType(name);
        }
        return null;
    }

    @Override
    public boolean isEnabled() {
        return true;
    }

    @Override
    public boolean isHandled() {
        return true;
    }

    @Override
    public void removeHandlerListener(IHandlerListener handlerListener) {
        // TODO Auto-generated method stub

    }

}
