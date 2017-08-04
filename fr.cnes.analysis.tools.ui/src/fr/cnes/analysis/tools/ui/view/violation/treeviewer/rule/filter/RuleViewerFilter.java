/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.violation.treeviewer.rule.filter;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;
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
    private String searchString = "";
    /** Is the filter focusing a file ? */
    private boolean filteringFile = false;
    /** Is the filter focusing a Rule ? */
    private boolean filteringRule = false;
    /** Should we show violation of Warning criticity ? */
    private boolean showWarning = true;
    /** Should we show violation of Error criticity ? */
    private boolean showError = true;
    /** Should we show violation of Info criticity ? */
    private boolean showInfo = true;

    @Override
    public boolean select(Viewer pViewer, Object pParentElement, Object pElement) {
        boolean show = false;
        /*
         * Setting filtering level
         */
        if (pElement instanceof RuleDescriptor) {
            final RuleDescriptor rule = (RuleDescriptor) pElement;
            if (checkSeverity(rule)) {

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

    private boolean checkSeverity(RuleDescriptor rule) {
        return (rule.getSeverity().equals(UserPreferencesService.PREF_SEVERITY_WARNING_VALUE)
                        && showWarning)
                        || (rule.getSeverity()
                                        .equals(UserPreferencesService.PREF_SEVERITY_ERROR_VALUE)
                                        && showError)
                        || (rule.getSeverity()
                                        .equals(UserPreferencesService.PREF_SEVERITY_INFO_VALUE)
                                        && showInfo);
    }

    @Override
    public void update(String searchString, boolean showInfo, boolean showWarning,
                    boolean showError) {
        this.searchString = searchString;
        this.showError = showError;
        this.showWarning = showWarning;
        this.showInfo = showInfo;
        filteringRule = false;
        filteringFile = false;

    }

}