/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferences.checkerstables;

import java.util.List;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;

import fr.cnes.analysis.tools.ui.preferences.CheckerPreferencesContainer;
import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;

/**
 * This {@link CheckerTableViewer} is implemented to show and edit configuration
 * of i-Code Checker which are set as metrics.
 * 
 * @version 3.0
 * @since 3.0
 */
public class CheckerMetricTableViewer extends CheckerTableViewer {
    /** Enable or disable checkers column's index. */
    private static final int COLUMN_ENABLED_INDEX = 0;
    /** Enable or disable checkers column's bound. */
    private static final int COLUMN_ENABLED_BOUND = 30;
    /** Checker's name column's name. */
    private static final String COLUMN_CHECKER_NAME = "Checker";
    /** Checker's name checkers column's index. */
    private static final int COLUMN_CHECKER_INDEX = 1;
    /** Checker's name checkers column's bound. */
    private static final int COLUMN_CHECKER_BOUND = 200;
    /** Checker's languages column's name. */
    private static final String COLUMN_LANGUAGE_NAME = "Language";
    /** Checker's languages checkers column's index. */
    private static final int COLUMN_LANGUAGE_INDEX = 2;
    /** Checker's languages checkers column's bound. */
    private static final int COLUMN_LANGUAGE_BOUND = 80;
    /** Checker's minimum value column's name. */
    private static final String COLUMN_MINIMUM_NAME = "Minimum";
    /** Checker's minimum value column's index. */
    private static final int COLUMN_MINIMUM_INDEX = 3;
    /** Checker's minimum value column's bound. */
    private static final int COLUMN_MINIMUM_BOUND = 80;
    /** Checker's maximum value column's name. */
    private static final String COLUMN_MAXIMUM_NAME = "Maximum";
    /** Checker's maximum value column's index. */
    private static final int COLUMN_MAXIMUM_INDEX = 4;
    /** Checker's maximum value column's bound. */
    private static final int COLUMN_MAXIMUM_BOUND = 80;
    /** Checker's severity column's name. */
    private static final String COLUMN_SEVERITY_NAME = "Severity";
    /** Checker's severity column's index. */
    private static final int COLUMN_SEVERITY_INDEX = 5;
    /** Checker's severity column's bound. */
    private static final int COLUMN_SEVERITY_BOUND = 80;

    /** Float NaN display. */
    private static final String FLOAT_NAN_DISPLAY = "-";

    /**
     * @param parent
     *            composite containing the table.
     * @param checkers
     *            to show and configure in the table.
     */
    public CheckerMetricTableViewer(Composite parent, List<CheckerPreferencesContainer> checkers) {
        super(parent, checkers);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * fr.cnes.analysis.tools.ui.preferences.checkerstables.CheckerTableViewer#
     * createColumns(org.eclipse.swt.widgets.Composite,
     * org.eclipse.jface.viewers.TableViewer)
     */
    @Override
    protected void createColumns(Composite parent, TableViewer pCheckersTableViewer) {

        setEnabledColumn(createEnabledViewerColumn(COLUMN_ENABLED_BOUND, COLUMN_ENABLED_INDEX));
        getEnabledColumn().setEditingSupport(new EnabledEditingSupport(pCheckersTableViewer, this));
        getEnabledColumn().setLabelProvider(new ColumnLabelProvider() {
            @Override
            public String getText(Object element) {
                return null;
            }

            @Override
            public Image getImage(Object element) {
                final Image image;
                final CheckerPreferencesContainer checker = (CheckerPreferencesContainer) element;
                if (checker.isChecked()) {
                    image = getEnabledImage();
                } else {
                    image = getDisabledImage();
                }
                return image;
            }
        });
        TableViewerColumn col = createTableViewerColumn(COLUMN_CHECKER_NAME, COLUMN_CHECKER_BOUND,
                        COLUMN_CHECKER_INDEX);
        col.setLabelProvider(new ColumnLabelProvider() {
            @Override
            public String getText(Object element) {
                final CheckerPreferencesContainer checker = (CheckerPreferencesContainer) element;
                return checker.getName();
            }
        });
        col = createTableViewerColumn(COLUMN_LANGUAGE_NAME, COLUMN_LANGUAGE_BOUND,
                        COLUMN_LANGUAGE_INDEX);
        col.setLabelProvider(new ColumnLabelProvider() {
            @Override
            public String getText(Object element) {
                final CheckerPreferencesContainer checker = (CheckerPreferencesContainer) element;
                return checker.getLanguageName();
            }
        });

        col = createTableViewerColumn(COLUMN_MINIMUM_NAME, COLUMN_MINIMUM_BOUND,
                        COLUMN_MINIMUM_INDEX);
        col.setEditingSupport(new MinValueEditingSupport(pCheckersTableViewer));
        col.setLabelProvider(new ColumnLabelProvider() {
            @Override
            public String getText(Object element) {
                String floatValue = FLOAT_NAN_DISPLAY;
                final CheckerPreferencesContainer checker = (CheckerPreferencesContainer) element;
                if (UserPreferencesService.isDefaultConfigurationActive()) {
                    if (!checker.getMinValue().isNaN()) {
                        floatValue = Float.toString(checker.getMinValue().floatValue());
                    } else if (UserPreferencesService.hasMinValue(checker.getId())) {
                        floatValue = Float.toString(UserPreferencesService
                                        .getMinValue(checker.getId()).floatValue());
                    }
                } else {
                    if (UserPreferencesService.hasMinValue(checker.getId())) {
                        floatValue = Float.toString(UserPreferencesService
                                        .getMinValue(checker.getId()).floatValue());
                    }
                }
                return floatValue;
            }
        });

        col = createTableViewerColumn(COLUMN_MAXIMUM_NAME, COLUMN_MAXIMUM_BOUND,
                        COLUMN_MAXIMUM_INDEX);
        col.setEditingSupport(new MaxValueEditingSupport(pCheckersTableViewer));
        col.setLabelProvider(new ColumnLabelProvider() {
            @Override
            public String getText(Object element) {
                String floatValue = FLOAT_NAN_DISPLAY;
                final CheckerPreferencesContainer checker = (CheckerPreferencesContainer) element;
                if (UserPreferencesService.isDefaultConfigurationActive()) {
                    if (!checker.getMaxValue().isNaN()) {
                        floatValue = Float.toString(checker.getMaxValue().floatValue());
                    } else if (UserPreferencesService.hasMaxValue(checker.getId())) {
                        floatValue = Float.toString(UserPreferencesService
                                        .getMaxValue(checker.getId()).floatValue());
                    }
                } else {
                    if (UserPreferencesService.hasMaxValue(checker.getId())) {
                        floatValue = Float.toString(UserPreferencesService
                                        .getMaxValue(checker.getId()).floatValue());
                    }
                }
                return floatValue;
            }
        });
        col = createTableViewerColumn(COLUMN_SEVERITY_NAME, COLUMN_SEVERITY_BOUND,
                        COLUMN_SEVERITY_INDEX);
        col.setEditingSupport(new SeverityEditingSupport(pCheckersTableViewer));
        col.setLabelProvider(new ColumnLabelProvider() {
            @Override
            public String getText(Object element) {
                return ((CheckerPreferencesContainer) element).getSeverity();
            }

            @Override
            public Image getImage(Object element) {
                final CheckerPreferencesContainer checker = (CheckerPreferencesContainer) element;
                final Image severityImage;
                switch (checker.getSeverity()) {
                case UserPreferencesService.PREF_SEVERITY_ERROR_VALUE:
                    severityImage = getErrorImage();
                    break;
                case UserPreferencesService.PREF_SEVERITY_WARNING_VALUE:
                    severityImage = getWarningImage();
                    break;
                case UserPreferencesService.PREF_SEVERITY_INFO_VALUE:
                default:
                    severityImage = getInfoImage();
                    break;
                }

                return severityImage;

            }

        });

    }

}
