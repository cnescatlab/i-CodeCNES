package fr.cnes.analysis.tools.ui.preferences.checkerstables;

import fr.cnes.analysis.tools.ui.images.ImageFactory;
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
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;

public class CheckerTableViewer {

    private TableViewer checkersTableViewer;
    protected Image infoImage;
    protected Image warningImage;
    protected Image errorImage;
    protected Image enabledImage;
    protected Image disabledImage;
    private Composite parent;
    protected TableViewerColumn enabledColumn;

    private LanguagePreferencesContainer language;
    private List<CheckerPreferencesContainer> inputs;

    protected boolean allEnabledChecked;
    private Listener enableAllListerner;

    public CheckerTableViewer(Composite pParent, List<CheckerPreferencesContainer> checkers) {
        this.inputs = checkers;
        parent = pParent;
        infoImage = ImageFactory.getImage(ImageFactory.INFO_SMALL);
        warningImage = ImageFactory.getImage(ImageFactory.WARNING_SMALL);
        errorImage = ImageFactory.getImage(ImageFactory.ERROR_SMALL);
        enabledImage = ImageFactory.getImage(ImageFactory.ENABLED);
        disabledImage = ImageFactory.getImage(ImageFactory.DISABLED);
        GridLayout layout = new GridLayout(2, false);
        pParent.setLayout(layout);
        Label searchLabel = new Label(pParent, SWT.NONE);
        searchLabel.setText("Search: ");
        final Text searchText = new Text(pParent, SWT.BORDER | SWT.SEARCH);
        searchText.setLayoutData(
                new GridData(GridData.GRAB_HORIZONTAL | GridData.HORIZONTAL_ALIGN_FILL));

        createViewer(pParent);
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

    protected void createColumns(Composite parent, TableViewer pCheckersTableViewer) {

        String[] titles = { "Enabled", "Checker", "Language", "Severity" };
        int[] bounds = { 30, 200, 80, 80 };

        enabledColumn = createEnabledViewerColumn(bounds[0], 0);
        enabledColumn.setEditingSupport(new EnabledEditingSupport(pCheckersTableViewer, this));
        enabledColumn.setLabelProvider(new ColumnLabelProvider() {

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
        TableViewerColumn col = createTableViewerColumn(titles[1], bounds[1], 1);
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

    protected TableViewerColumn createTableViewerColumn(String title, int bound,
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

    protected TableViewerColumn createEnabledViewerColumn(int bound, final int colNumber) {
        final TableViewerColumn viewerColumn = new TableViewerColumn(this.checkersTableViewer,
                SWT.CENTER);
        final TableColumn column = viewerColumn.getColumn();
        column.setImage(ImageFactory.getImage(ImageFactory.DISABLED));
        column.setToolTipText("Check to select or unselect every rules in the table.");
        column.setWidth(bound);
        column.setResizable(true);
        column.setMoveable(true);
        enableAllListerner = new Listener() {

            @Override
            public void handleEvent(Event event) {
                /*
                 * If the event is a selection one, then we have to set inputs
                 * to get all of them checked or unchecked.
                 */
                if (event.type == SWT.Selection) {
                    if (UserPreferencesService.isDefaultConfigurationActive()) {
                        if (!allEnabledChecked) {
                            column.setImage(ImageFactory.getImage(ImageFactory.ENABLED));
                            for (CheckerPreferencesContainer checker : inputs) {
                                checker.setChecked(true);
                            }
                            allEnabledChecked = true;
                        } else {
                            column.setImage(ImageFactory.getImage(ImageFactory.DISABLED));
                            for (CheckerPreferencesContainer checker : inputs) {
                                checker.setChecked(false);
                            }
                            allEnabledChecked = false;
                        }
                    } else {
                        column.setImage(null);
                    }
                }
                /*
                 * Image must be set pending the inputs and the configuration.
                 */
                if (UserPreferencesService.isDefaultConfigurationActive()) {
                    allEnabledChecked = isAllEnabled();
                    if (allEnabledChecked) {
                        column.setImage(ImageFactory.getImage(ImageFactory.ENABLED));
                    } else {
                        column.setImage(ImageFactory.getImage(ImageFactory.DISABLED));
                    }
                } else {
                    column.setImage(null);
                }

                checkersTableViewer.refresh();
            }

            private boolean isAllEnabled() {
                int i = 0;
                boolean allEnabled = true;
                while (i < inputs.size() && allEnabled) {
                    if (!inputs.get(i).isChecked()) {
                        allEnabled = false;
                    }
                    i++;
                }
                return allEnabled;
            }
        };
        column.addListener(SWT.Selection, enableAllListerner);
        this.refresh();

        return viewerColumn;

    }

    public void refresh() {
        this.checkersTableViewer.getControl().redraw();
        this.enableAllListerner.handleEvent(new Event());
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

    public void setAllEnabledChecker(boolean isEnabled) {
        if (allEnabledChecked && !isEnabled) {
            allEnabledChecked = false;
            enabledColumn.getColumn().setImage(disabledImage);
        }
    }
}
