package fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.filter;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

import fr.cnes.analysis.tools.ui.view.violation.treeviewer.IUpdatableAnalysisFilter;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor.FileRuleDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor.FunctionDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor.RuleDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor.ViolationDescriptor;

/**
 * This class is a filter to apply on
 * {@link fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.FileTreeViewer}
 *
 */
public class FileTreeViewerFilter extends ViewerFilter implements IUpdatableAnalysisFilter {

    /** String filtered */
    private String  searchString      = "";
    /** Is the filter focusing a file ? */
    private boolean filteringFile     = false;
    /** Is the filter focusing a function ? */
    private boolean filteringFunction = false;
    /** Is the filter focusing a Rule ? */
    private boolean filteringRule     = false;
    /** Should we show violation of Warning criticity ? */
    private boolean showWarning       = true;
    /** Should we show violation of Error criticity ? */
    private boolean showError         = true;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ViewerFilter#select(org.eclipse.jface.viewers.
     * Viewer, java.lang.Object, java.lang.Object)
     */
    @Override
    public boolean select(Viewer pViewer, Object pParentElement, Object pElement) {
        boolean show = false;
        boolean ruleBeingShown = false;
        /*
         * Setting filtering level
         * 
         * Should we show a FileName ? - Yes if it's theone filtered or if it's
         * contain a function, rule, or violation filtered; - No otherwise;
         * Should we show a Function ? - Yes if file containing it is being the
         * focus of the filter, or if the function is being itself filtered, or
         * if a rule, or violation in it is the one focused by the searchString.
         * etc...
         */
        if (pElement instanceof FileRuleDescriptor) {
            final FileRuleDescriptor file = (FileRuleDescriptor) pElement;
            if (file.getName().toUpperCase().contains(searchString.toUpperCase())) {
                show = true;
                filteringFile = true;
            } else {
                for (FunctionDescriptor function : file.getDescriptors()) {
                    if (function.getName().toString().toUpperCase()
                            .contains(searchString.toUpperCase())) {
                        show = true;
                        filteringFunction = true;
                    } else {
                        for (RuleDescriptor rule : function.getDescriptors()) {
                            if (rule.getName().toUpperCase().contains(searchString.toUpperCase())
                                    && ((rule.getCriticity().equals("Warning") && showWarning)
                                            || (rule.getCriticity().equals("Error")
                                                    && showError))) {
                                show = true;
                                filteringRule = true;
                                ruleBeingShown = true;
                            } else {
                                for (ViolationDescriptor violation : rule.getDescriptors()) {
                                    if ((violation.getName().toString().toUpperCase()
                                            .contains(searchString.toUpperCase()))
                                            && ((rule.getCriticity().equals("Warning")
                                                    && showWarning)
                                                    || (rule.getCriticity().equals("Error")
                                                            && showError))) {
                                        show = true;
                                        ruleBeingShown = true;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        } else if (pElement instanceof FunctionDescriptor) {
            final FunctionDescriptor function = (FunctionDescriptor) pElement;
            if (function.getName().toString().toUpperCase().contains(searchString.toUpperCase())
                    || filteringFile) {
                show = true;
            } else {
                for (RuleDescriptor rule : function.getDescriptors()) {
                    if (rule.getName().toUpperCase().contains(searchString.toUpperCase())
                            && ((rule.getCriticity().equals("Warning") && showWarning)
                                    || (rule.getCriticity().equals("Error") && showError))) {
                        show = true;
                        ruleBeingShown = true;
                    } else {
                        for (ViolationDescriptor violation : rule.getDescriptors()) {
                            if (violation.getName().toString().toUpperCase()
                                    .contains(searchString.toUpperCase())) {
                                show = true;
                                ruleBeingShown = true;
                            }
                        }
                    }
                }
            }
        } else if (pElement instanceof RuleDescriptor) {
            final RuleDescriptor rule = (RuleDescriptor) pElement;
            if ((rule.getName().toUpperCase().contains(searchString.toUpperCase()) || filteringFile
                    || filteringFunction)
                    && ((rule.getCriticity().equals("Warning") && showWarning)
                            || (rule.getCriticity().equals("Error") && showError))) {
                show = true;
            } else {
                for (ViolationDescriptor violation : rule.getDescriptors()) {
                    if (violation.getName().toString().toUpperCase()
                            .contains(searchString.toUpperCase())
                            && ((rule.getCriticity().equals("Warning") && showWarning)
                                    || (rule.getCriticity().equals("Error") && showError))) {
                        show = true;
                    }
                }
            }
        } else if (pElement instanceof ViolationDescriptor) {
            final ViolationDescriptor violation = (ViolationDescriptor) pElement;
            show = violation.getName().toString().toUpperCase().contains(searchString.toUpperCase())
                    || filteringFile || filteringFunction || filteringRule;
        }
        if (pElement instanceof FileRuleDescriptor || pElement instanceof FunctionDescriptor) {
            show = (show || ruleBeingShown);
        }
        return show;

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * fr.cnes.analysis.tools.ui.view.rules.treeviewer.IUpdatableAnalysisFilter#
     * update(java.lang.String, boolean, boolean)
     */
    @Override
    public void update(String pSearchString, boolean pShowWarning, boolean pShowError) {
        this.searchString = pSearchString;
        this.showError = pShowError;
        this.showWarning = pShowWarning;
        filteringRule = false;
        filteringFile = false;
        filteringFunction = false;

    }
}