package fr.cnes.analysis.tools.ui.preferences.checkerstables;

import fr.cnes.analysis.tools.ui.preferences.CheckerPreferencesContainer;
import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;
import java.util.List;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;

public class CheckerMetricTableViewer extends CheckerTableViewer {

    public CheckerMetricTableViewer(Composite parent, List<CheckerPreferencesContainer> checkers) {
        super(parent, checkers);
    }

    private void createColumns(Composite parent, TableViewer pCheckersTableViewer) {

        String[] titles = { "", "Checker", "Language", "Minimum", "Maximum", "Severity" };
        int[] bounds = { 30, 200, 80, 80, 80, 80 };

        TableViewerColumn col = createEnabledViewerColumn(bounds[0], 0);
        col.setEditingSupport(new EnabledEditingSupport(pCheckersTableViewer, this));
        col.setLabelProvider(new ColumnLabelProvider() {
            @Override
            public String getText(Object element) {
                return null;
            }

            @Override
            public Image getImage(Object element) {
                Image image;
                CheckerPreferencesContainer checker = (CheckerPreferencesContainer) element;
                if (checker.isChecked()) {
                    image = enabledImage;
                } else {
                    image = disabledImage;
                }
                return image;
            }
        });
        col = createTableViewerColumn(titles[1], bounds[1], 1);
        col.setLabelProvider(new ColumnLabelProvider() {
            @Override
            public String getText(Object element) {
                final CheckerPreferencesContainer checker = (CheckerPreferencesContainer) element;
                return checker.getName();
            }
        });
        col = createTableViewerColumn(titles[2], bounds[2], 2);
        col.setLabelProvider(new ColumnLabelProvider() {
            @Override
            public String getText(Object element) {
                final CheckerPreferencesContainer checker = (CheckerPreferencesContainer) element;
                return checker.getLanguageName();
            }
        });

        col = createTableViewerColumn(titles[3], bounds[3], 3);
        col.setEditingSupport(new MinValueEditingSupport(pCheckersTableViewer));
        col.setLabelProvider(new ColumnLabelProvider() {
            @Override
            public String getText(Object element) {
                String floatValue = "-";
                final CheckerPreferencesContainer checker = (CheckerPreferencesContainer) element;
                if (UserPreferencesService.isDefaultConfigurationActive()) {
                    if (!checker.getMinValue().isNaN()) {
                        floatValue = Float.toString(checker.getMinValue());
                    } else if (UserPreferencesService.hasMinValue(checker.getId())) {
                        floatValue = Float
                                .toString(UserPreferencesService.getMinValue(checker.getId()));
                    }
                } else {
                    if (UserPreferencesService.hasMinValue(checker.getId())) {
                        floatValue = Float
                                .toString(UserPreferencesService.getMinValue(checker.getId()));
                    }
                }
                return floatValue;
            }
        });

        col = createTableViewerColumn(titles[4], bounds[4], 4);
        col.setEditingSupport(new MaxValueEditingSupport(pCheckersTableViewer));
        col.setLabelProvider(new ColumnLabelProvider() {
            @Override
            public String getText(Object element) {
                String floatValue = "-";
                final CheckerPreferencesContainer checker = (CheckerPreferencesContainer) element;
                if (UserPreferencesService.isDefaultConfigurationActive()) {
                    if (!checker.getMaxValue().isNaN()) {
                        floatValue = Float.toString(checker.getMaxValue());
                    } else if (UserPreferencesService.hasMaxValue(checker.getId())) {
                        floatValue = Float
                                .toString(UserPreferencesService.getMaxValue(checker.getId()));
                    }
                } else {
                    if (UserPreferencesService.hasMaxValue(checker.getId())) {
                        floatValue = Float
                                .toString(UserPreferencesService.getMaxValue(checker.getId()));
                    }
                }
                return floatValue;
            }
        });
        col = createTableViewerColumn(titles[5], bounds[5], 5);
        col.setEditingSupport(new SeverityEditingSupport(pCheckersTableViewer));
        col.setLabelProvider(new ColumnLabelProvider() {
            @Override
            public String getText(Object element) {
                return ((CheckerPreferencesContainer) element).getSeverity();
            }

            @Override
            public Image getImage(Object element) {
                CheckerPreferencesContainer checker = (CheckerPreferencesContainer) element;
                Image severityImage;
                switch (checker.getSeverity()) {
                    case UserPreferencesService.PREF_SEVERITY_ERROR_VALUE:
                        severityImage = errorImage;
                        break;
                    case UserPreferencesService.PREF_SEVERITY_WARNING_VALUE:
                        severityImage = warningImage;
                        break;
                    case UserPreferencesService.PREF_SEVERITY_INFO_VALUE:
                    default:
                        severityImage = infoImage;
                        break;
                }

                return severityImage;

            }

        });

    }

}
