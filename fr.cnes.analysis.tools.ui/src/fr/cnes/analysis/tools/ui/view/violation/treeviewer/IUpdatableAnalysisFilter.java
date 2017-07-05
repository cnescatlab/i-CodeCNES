package fr.cnes.analysis.tools.ui.view.violation.treeviewer;

public interface IUpdatableAnalysisFilter {

    public void update(String searchString, boolean showInfo, boolean showWarning,
            boolean showError);
}
