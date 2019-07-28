/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.filter;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.IUpdatableAnalysisFilter;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor.FileRuleDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor.FunctionDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor.RuleDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor.ViolationDescriptor;
import fr.cnes.icode.logger.ICodeLogger;

/**
 * This class is a filter to apply on
 * {@link fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.FileTreeViewer}
 */
public class FileTreeViewerFilter extends ViewerFilter implements IUpdatableAnalysisFilter {
    /**
     * Class name
     */
    private static final String CLASS = FileTreeViewerFilter.class.getName();

    /**
     * String filtered
     */
    private String searchString = "";
    /**
     * Is the filter focusing a file ?
     */
    private boolean filteringFile = false;
    /**
     * Is the filter focusing a function ?
     */
    private boolean filteringFunction = false;
    /**
     * Is the filter focusing a Rule ?
     */
    private boolean filteringRule = false;
    /**
     * Should we show violation of Warning criticity ?
     */
    private boolean showWarning = true;
    /**
     * Should we show violation of Error criticity ?
     */
    private boolean showError = true;
    /**
     * Should info severity violation be shown ?
     */
    private boolean showInfo = true;

    /*
     * (non-Javadoc)
     *
     * @see
     * org.eclipse.jface.viewers.ViewerFilter#select(org.eclipse.jface.viewers.
     * Viewer, java.lang.Object, java.lang.Object)
     */
    @Override
    public boolean select(final Viewer pViewer, final Object pParentElement,
                          final Object pElement) {
        final String method = "select";
        ICodeLogger.entering(CLASS, method, new Object[]{
                pViewer, pParentElement, pElement
        });
        boolean show = false;
        boolean ruleBeingShown = false;
        /*
         * Setting filtering level
         */
        if (pElement instanceof FileRuleDescriptor) {
            final FileRuleDescriptor file = (FileRuleDescriptor) pElement;
            if (file.getName().toUpperCase().contains(searchString.toUpperCase())) {
                show = true;
                filteringFile = true;
            } else {
                for (final FunctionDescriptor function : file.getDescriptors()) {
                    if (function.getName().toString().toUpperCase()
                            .contains(searchString.toUpperCase())) {
                        show = true;
                        filteringFunction = true;
                    } else {
                        for (final RuleDescriptor rule : function.getDescriptors()) {
                            if (rule.getName().toUpperCase().contains(searchString.toUpperCase())
                                    && checkSeverity(rule)) {
                                show = true;
                                filteringRule = true;
                                ruleBeingShown = true;
                            } else {
                                for (ViolationDescriptor violation : rule.getDescriptors()) {
                                    if ((violation.getName().toString().toUpperCase()
                                            .contains(searchString.toUpperCase()))
                                            && checkSeverity(rule)) {
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
                for (final RuleDescriptor rule : function.getDescriptors()) {
                    if (rule.getName().toUpperCase().contains(searchString.toUpperCase())
                            && checkSeverity(rule)) {
                        show = true;
                        ruleBeingShown = true;
                    } else {
                        for (final ViolationDescriptor violation : rule.getDescriptors()) {
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
                    || filteringFunction) && checkSeverity(rule)) {
                show = true;
            } else {
                for (final ViolationDescriptor violation : rule.getDescriptors()) {
                    if (violation.getName().toString().toUpperCase()
                            .contains(searchString.toUpperCase()) && checkSeverity(rule)) {
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
        ICodeLogger.exiting(CLASS, method, Boolean.valueOf(show));
        return show;

    }

    /**
     * @param rule Rule to verify.
     * @return whether or not the CheckerResult should be shown pending it's
     * severity configuration.
     */
    private boolean checkSeverity(final RuleDescriptor rule) {
        final String method = "checkSeverity";
        ICodeLogger.entering(CLASS, method);
        final boolean checked = (rule.getSeverity()
                .equals(UserPreferencesService.PREF_SEVERITY_WARNING_VALUE) && showWarning)
                || (rule.getSeverity()
                .equals(UserPreferencesService.PREF_SEVERITY_ERROR_VALUE)
                && showError)
                || (rule.getSeverity()
                .equals(UserPreferencesService.PREF_SEVERITY_INFO_VALUE)
                && showInfo);
        ICodeLogger.exiting(CLASS, method, Boolean.valueOf(checked));
        return checked;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * fr.cnes.analysis.tools.ui.view.rules.treeviewer.IUpdatableAnalysisFilter#
     * update(java.lang.String, boolean, boolean)
     */
    @Override
    public void update(final String pSearchString, final boolean pShowInfo,
                       final boolean pShowWarning, final boolean pShowError) {
        final String method = "update";
        ICodeLogger.entering(CLASS, method);
        this.searchString = pSearchString;
        this.showError = pShowError;
        this.showWarning = pShowWarning;
        this.showInfo = pShowInfo;
        filteringRule = false;
        filteringFile = false;
        filteringFunction = false;
        ICodeLogger.exiting(CLASS, method);

    }
}