/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferences.checkerstables;

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

import fr.cnes.analysis.tools.ui.images.ImageFactory;
import fr.cnes.analysis.tools.ui.preferences.CheckerPreferencesContainer;
import fr.cnes.analysis.tools.ui.preferences.LanguagePreferencesContainer;
import fr.cnes.analysis.tools.ui.preferences.UserPreferencesService;

/**
 * This viewer can show {@link CheckerPreferencesContainer} in a Table,
 * editable.
 */
public class CheckerTableViewer {
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
    /** Checker's severity column's name. */
    private static final String COLUMN_SEVERITY_NAME = "Severity";
    /** Checker's severity column's index. */
    private static final int COLUMN_SEVERITY_INDEX = 3;
    /** Checker's severity column's bound. */
    private static final int COLUMN_SEVERITY_BOUND = 80;

    /** Image of information severity level */
    private Image infoImage;
    /** Image of warning severity level */
    private Image warningImage;
    /** Image of error severity level */
    private Image errorImage;
    /** Image of information enabled checker */
    private Image enabledImage;
    /** Image of disabled checker */
    private Image disabledImage;
    /** Enabled column */
    private TableViewerColumn enabledColumn;
    /** All checkers are enabled */
    private boolean allEnabledChecked;

    /** Parent composite containing the Tableviewer */
    private Composite parent;
    /** The tableviewer */
    private TableViewer checkersTableViewer;
    /** Language preference container */
    private LanguagePreferencesContainer language;
    /** Checkers to configure in the table */
    private List<CheckerPreferencesContainer> inputs;
    /** Listener for all checker enabling/disabling */
    private Listener enableAllListerner;

    /**
     * @param pParent
     *            Composite containing the Table Viewer.
     * @param checkers
     *            Table viewer's inputs.
     */
    public CheckerTableViewer(Composite pParent, List<CheckerPreferencesContainer> checkers) {
        this.inputs = checkers;
        parent = pParent;
        infoImage = ImageFactory.getImage(ImageFactory.INFO_SMALL);
        warningImage = ImageFactory.getImage(ImageFactory.WARNING_SMALL);
        errorImage = ImageFactory.getImage(ImageFactory.ERROR_SMALL);
        enabledImage = ImageFactory.getImage(ImageFactory.ENABLED);
        disabledImage = ImageFactory.getImage(ImageFactory.DISABLED);
        final GridLayout layout = new GridLayout(2, false);
        parent.setLayout(layout);
        final Label searchLabel = new Label(parent, SWT.NONE);
        searchLabel.setText("Search: ");
        final Text searchText = new Text(parent, SWT.BORDER | SWT.SEARCH);
        searchText.setLayoutData(
                        new GridData(GridData.GRAB_HORIZONTAL | GridData.HORIZONTAL_ALIGN_FILL));

        createViewer(parent);
        final CheckersFilter filter = new CheckersFilter();
        searchText.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent e) {
                filter.setSearchText(searchText.getText());
                checkersTableViewer.refresh();
            }
        });
        checkersTableViewer.addFilter(filter);
    }

    /**
     * @param pParent
     *            Composite containing the Table Viewer
     */
    private void createViewer(Composite pParent) {
        checkersTableViewer = new TableViewer(pParent,
                        SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.BORDER);
        createColumns(pParent, checkersTableViewer);
        final Table table = checkersTableViewer.getTable();
        table.setHeaderVisible(true);
        table.setLinesVisible(true);

        checkersTableViewer.setContentProvider(new ArrayContentProvider());
        // get the content for the viewer, setInput will call getElements in the
        // contentProvider
        checkersTableViewer.setInput(inputs);
        // make the selection available to other views

        // define layout for the viewer
        final GridData gridData = new GridData();
        gridData.verticalAlignment = GridData.FILL;
        gridData.horizontalSpan = 2;
        gridData.grabExcessHorizontalSpace = true;
        gridData.grabExcessVerticalSpace = true;
        gridData.horizontalAlignment = GridData.FILL;
        checkersTableViewer.getControl().setLayoutData(gridData);
    }

    /**
     * @param pParent
     *            Composite containing the Table Viewer
     * @param pCheckersTableViewer
     *            TableViewer containing the columns.
     * 
     */
    protected void createColumns(Composite pParent, TableViewer pCheckersTableViewer) {

        enabledColumn = createEnabledViewerColumn(COLUMN_ENABLED_BOUND, COLUMN_ENABLED_INDEX);
        enabledColumn.setEditingSupport(new EnabledEditingSupport(pCheckersTableViewer, this));
        enabledColumn.setLabelProvider(new ColumnLabelProvider() {

            @Override
            public String getText(Object element) {
                return null;
            }

            @Override
            public Image getImage(Object element) {
                final Image image;
                final CheckerPreferencesContainer checker = (CheckerPreferencesContainer) element;
                if (checker.isChecked()) {
                    image = enabledImage;
                } else {
                    image = disabledImage;
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

    /**
     * @param title
     *            Column's title.
     * @param bound
     *            Column's bound
     * @param colNumber
     *            Column's index.
     * @return {@link TableViewerColumn} created.
     */
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

    /**
     * @param bound
     *            Column's bound.
     * @param colNumber
     *            Column's index.
     * @return A column with an Image to set checker enabled or disabled.
     */
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

    /**
     * Refresh the TableViewer and it's components.
     */
    public void refresh() {
        this.checkersTableViewer.getControl().redraw();
        this.enableAllListerner.handleEvent(new Event());
        this.checkersTableViewer.refresh();
    }

    /**
     * @return the infoImage
     */
    protected Image getInfoImage() {
        return infoImage;
    }

    /**
     * @param pInfoImage
     *            the infoImage to set
     */
    protected void setInfoImage(Image pInfoImage) {
        this.infoImage = pInfoImage;
    }

    /**
     * @return the warningImage
     */
    protected Image getWarningImage() {
        return warningImage;
    }

    /**
     * @param pWarningImage
     *            the warningImage to set
     */
    protected void setWarningImage(Image pWarningImage) {
        this.warningImage = pWarningImage;
    }

    /**
     * @return the errorImage
     */
    protected Image getErrorImage() {
        return errorImage;
    }

    /**
     * @param pErrorImage
     *            the errorImage to set
     */
    protected void setErrorImage(Image pErrorImage) {
        this.errorImage = pErrorImage;
    }

    /**
     * @return the enabledImage
     */
    protected Image getEnabledImage() {
        return enabledImage;
    }

    /**
     * @param pEnabledImage
     *            the enabledImage to set
     */
    protected void setEnabledImage(Image pEnabledImage) {
        this.enabledImage = pEnabledImage;
    }

    /**
     * @return the disabledImage
     */
    protected Image getDisabledImage() {
        return disabledImage;
    }

    /**
     * @param pDisabledImage
     *            the disabledImage to set
     */
    protected void setDisabledImage(Image pDisabledImage) {
        this.disabledImage = pDisabledImage;
    }

    /**
     * @return the enabledColumn
     */
    protected TableViewerColumn getEnabledColumn() {
        return enabledColumn;
    }

    /**
     * @param pEnabledColumn
     *            the enabledColumn to set
     */
    protected void setEnabledColumn(TableViewerColumn pEnabledColumn) {
        this.enabledColumn = pEnabledColumn;
    }

    /**
     * @return the allEnabledChecked
     */
    protected boolean isAllEnabledChecked() {
        return allEnabledChecked;
    }

    /**
     * @param pAllEnabledChecked
     *            the allEnabledChecked to set
     */
    protected void setAllEnabledChecked(boolean pAllEnabledChecked) {
        this.allEnabledChecked = pAllEnabledChecked;
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

    /**
     * @param isEnabled
     *            Set enabled or disabled all checker of the TableViewer.
     */
    public void setAllEnabledChecker(boolean isEnabled) {
        if (allEnabledChecked && !isEnabled) {
            allEnabledChecked = false;
            enabledColumn.getColumn().setImage(disabledImage);
        }
    }
}
