package fr.cnes.analysis.tools.ui.preferences.checkerstables;

import fr.cnes.analysis.tools.ui.images.ImageService;
import fr.cnes.analysis.tools.ui.preferences.CheckerPreferencesContainer;
import fr.cnes.analysis.tools.ui.preferences.LanguagePreferencesContainer;
import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;
import java.util.List;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;

public class CheckerTableViewer {
    private List<Button> checkersButtons;
    private List<Button> languagesButtons;

    private TableViewer checkersTableViewer;
    private CheckersFilter checkersFilter;

    private LanguagePreferencesContainer language;
    private List<CheckerPreferencesContainer> inputs;

    private boolean allEnabledChecked;

    public CheckerTableViewer(Composite parent, List<CheckerPreferencesContainer> checkers) {
        this.inputs = checkers;
        GridLayout layout = new GridLayout(2, false);
        parent.setLayout(layout);
        Label searchLabel = new Label(parent, SWT.NONE);
        searchLabel.setText("Search: ");
        final Text searchText = new Text(parent, SWT.BORDER | SWT.SEARCH);
        searchText.setLayoutData(
                new GridData(GridData.GRAB_HORIZONTAL | GridData.HORIZONTAL_ALIGN_FILL));

        createViewer(parent);
        CheckersFilter filter = new CheckersFilter();
        searchText.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent e) {
                filter.setSearchText(searchText.getText());
                checkersTableViewer.refresh();
            }
        });
        checkersTableViewer.addFilter(filter);
    }

    private void createViewer(Composite parent) {
        checkersTableViewer = new TableViewer(parent,
                SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.BORDER);
        createColumns(parent, checkersTableViewer);
        final Table table = checkersTableViewer.getTable();
        table.setHeaderVisible(true);
        table.setLinesVisible(true);

        checkersTableViewer.setContentProvider(new ArrayContentProvider());
        // get the content for the viewer, setInput will call getElements in the
        // contentProvider
        checkersTableViewer.setInput(inputs);
        // make the selection available to other views

        // define layout for the viewer
        GridData gridData = new GridData();
        gridData.verticalAlignment = GridData.FILL;
        gridData.horizontalSpan = 2;
        gridData.grabExcessHorizontalSpace = true;
        gridData.grabExcessVerticalSpace = true;
        gridData.horizontalAlignment = GridData.FILL;
        checkersTableViewer.getControl().setLayoutData(gridData);
    }

    private void createColumns(Composite parent, TableViewer pCheckersTableViewer) {

        String[] titles = { "Enabled", "Checker", "Language", "Severity" };
        int[] bounds = { 30, 200, 80, 80 };

        TableViewerColumn col = createEnabledViewerColumn(bounds[0], 0);
        col.setEditingSupport(new EnabledEditingSupport(pCheckersTableViewer));
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
                    image = ImageService.ENABLED;
                } else {
                    image = ImageService.DISABLED;
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
                        severityImage = ImageService.ERROR_SMALL;
                        break;
                    case UserPreferencesService.PREF_SEVERITY_WARNING_VALUE:
                        severityImage = ImageService.WARNING_SMALL;
                        break;
                    case UserPreferencesService.PREF_SEVERITY_INFO_VALUE:
                    default:
                        severityImage = ImageService.INFO_SMALL;
                        break;
                }

                return severityImage;

            }

        });

    }

    private TableViewerColumn createTableViewerColumn(String title, int bound,
            final int colNumber) {
        final TableViewerColumn viewerColumn = new TableViewerColumn(this.checkersTableViewer,
                SWT.NONE);
        final TableColumn column = viewerColumn.getColumn();
        column.setText(title);
        column.setWidth(bound);
        column.setResizable(true);
        column.setMoveable(true);
        return viewerColumn;
    }

    private TableViewerColumn createEnabledViewerColumn(int bound, final int colNumber) {
        final TableViewerColumn viewerColumn = new TableViewerColumn(this.checkersTableViewer,
                SWT.CENTER);
        final TableColumn column = viewerColumn.getColumn();
        column.setImage(ImageService.DISABLED);
        column.setToolTipText("Check to select or unselect every rules in the table.");
        column.setWidth(bound);
        allEnabledChecked = false;
        column.setResizable(true);
        column.setMoveable(true);
        column.addListener(SWT.Selection, new Listener() {

            @Override
            public void handleEvent(Event event) {
                if (!allEnabledChecked) {
                    column.setImage(ImageService.ENABLED);
                    for (CheckerPreferencesContainer checker : inputs) {
                        checker.setChecked(true);
                    }
                    allEnabledChecked = true;
                } else {
                    column.setImage(ImageService.DISABLED);
                    for (CheckerPreferencesContainer checker : inputs) {
                        checker.setChecked(false);
                    }
                    allEnabledChecked = false;
                }
                checkersTableViewer.refresh();
            }
        });

        return viewerColumn;
    }

    public void refresh() {
        this.checkersTableViewer.refresh();
    }

    /**
     * @return the language
     */
    public final LanguagePreferencesContainer getLanguage() {
        return language;
    }

    /**
     * @param pLanguage
     *            the language to set
     */
    public final void setLanguage(LanguagePreferencesContainer pLanguage) {
        this.language = pLanguage;
    }

    /**
     * @return the inputs
     */
    public final List<CheckerPreferencesContainer> getInputs() {
        return inputs;
    }

    /**
     * @param pInputs
     *            the inputs to set
     */
    public final void setInputs(List<CheckerPreferencesContainer> pInputs) {
        this.inputs = pInputs;
    }
}
