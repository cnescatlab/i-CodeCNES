package fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.filter;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

import fr.cnes.analysis.tools.ui.view.violation.treeviewer.IUpdatableAnalysisFilter;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.descriptor.FileRuleDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.descriptor.FunctionRuleDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.descriptor.RuleDescriptor;

/**
 * This class is a filter to apply on RuleTreeViewers.
 *
 */
public class RuleViewerFilter extends ViewerFilter implements IUpdatableAnalysisFilter {

    /** String filtered */
    private String  searchString      = "";
    /** Is the filter focusing a file ? */
    private boolean filteringFile     = false;
    /** Is the filter focusing a Rule ? */
    private boolean filteringRule     = false;
    /** Should we show violation of Warning criticity ? */
    private boolean showWarning       = true;
    /** Should we show violation of Error criticity ? */
    private boolean showError         = true;

    @Override
    public boolean select(Viewer pViewer, Object pParentElement, Object pElement) {
        boolean show = false;
        /*
         * Setting filtering level
         */
        if (pElement instanceof RuleDescriptor) {
            final RuleDescriptor rule = (RuleDescriptor) pElement;
            if ((rule.getCriticity().equals("Warning")) && showWarning
                    || (rule.getCriticity().equals("Error") && showError)) {

                if (rule.getName().toUpperCase().contains(searchString.toUpperCase())) {
                    show = true;
                    filteringRule = true;
                } else {
                    show = false;
                    for (FileRuleDescriptor file : rule.getDescriptors()) {
                        if (file.getFilePath().toString().toUpperCase()
                                .contains(searchString.toUpperCase())) {
                            show = true;
                            filteringFile = true;

                        } else {
                            for (FunctionRuleDescriptor function : file.getDescriptors()) {
                                if (function.getName().toUpperCase()
                                        .contains(searchString.toUpperCase())) {
                                    show = true;
                                }
                            }
                        }
                    }
                }
            }
        } else if (pElement instanceof FileRuleDescriptor) {
            final FileRuleDescriptor file = (FileRuleDescriptor) pElement;
            if (file.getFilePath().toString().toUpperCase().contains(searchString.toUpperCase())
                    || filteringRule) {
                show = true;
            } else {
                for (FunctionRuleDescriptor function : file.getDescriptors()) {
                    if (function.getName().toUpperCase().contains(searchString.toUpperCase())) {
                        show = true;
                    }
                }
            }
        } else if (pElement instanceof FunctionRuleDescriptor) {
            final FunctionRuleDescriptor function = (FunctionRuleDescriptor) pElement;
            show = function.getName().toUpperCase().contains(searchString.toUpperCase())
                    || filteringFile || filteringRule;
        }
        return show;

    }

    @Override
    public void update(String searchString, boolean showWarning, boolean showError) {
        this.searchString = searchString;
        this.showError = showError;
        this.showWarning = showWarning;
        filteringRule = false;
        filteringFile = false;

    }
}