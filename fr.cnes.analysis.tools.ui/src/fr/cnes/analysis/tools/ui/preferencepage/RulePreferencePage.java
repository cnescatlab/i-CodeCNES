/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferencepage;

import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.InvalidRegistryObjectException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.ui.preferencepage.rules.RuleEditingSupport;
import fr.cnes.analysis.tools.ui.preferencepage.rules.RulePreferenceContainer;
import fr.cnes.analysis.tools.ui.preferencepage.rules.RulePreferenceLabelProvider;
import fr.cnes.analysis.tools.ui.utils.PreferencesUIUtils;

/**
 * This is the class that defines the structure for preference page concerning
 * rules.
 * 
 */
public class RulePreferencePage extends PreferencePage
        implements IWorkbenchPreferencePage, IExecutableExtension {

    /** Logger **/
    private static final Logger LOGGER = Logger.getLogger(RulePreferencePage.class.getName());

    /** Column width **/
    private static final int[] BOUNDS = { 20, 150, 80 };

    /** Column titles **/
    private static final String[] TITLES = { " ! ", "Rule", "Criticity" };

    /** Title of the page **/
    private static final String TITLE_PAGE = "Selected rule(s) to be analyzed : ";

    /** The extension corresponding to this preference page **/
    private IConfigurationElement extension;

    /** Table viewer for language preference page. **/
    private TableViewer viewer;

    /** List of rules contributing to this language **/
    private List<IConfigurationElement> contributions;

    /** Composite containing the preferences. **/
    private Composite mainComposite;

    /**
     * Container for preferences. A container is composed of the id of the
     * boolean preference, its name and a boolean which represents if the
     * preference is true or false.
     **/
    private transient final List<RulePreferenceContainer> ruleContainers = new LinkedList<RulePreferenceContainer>();

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    @Override
    public void init(final IWorkbench workbench) {
        LOGGER.finest("Begin init method");

        // Initialize preference store
        final IPreferenceStore store = PlatformUI.getPreferenceStore();
        setPreferenceStore(store);

        // Initialize evaluation contributions
        contributions = new LinkedList<IConfigurationElement>();

        for (final IConfigurationElement contribution : Platform.getExtensionRegistry()
                .getConfigurationElementsFor(
                        extension.getAttribute(PreferencesUIUtils.RULE_EXT_ID))) {
            contributions.add(contribution);

            getPreferenceStore()
                    .setDefault(contribution.getAttribute(PreferencesUIUtils.CONTRIB_ID), true);
            getPreferenceStore().setDefault(contribution.getAttribute(PreferencesUIUtils.CONTRIB_ID)
                    + PreferencesUIUtils.CRITICAL, "Error");
        }

        LOGGER.finest("End init method");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.PreferencePage#contributeButtons(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected void contributeButtons(final Composite parent) {
        LOGGER.finest("Begin contributeButtons method");

        // Select all button : used to check all preferences
        final Button selectAll = new Button(parent, SWT.PUSH | SWT.CENTER);
        selectAll.setText("Select All");
        selectAll.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent event) {
                setAllChecked(true);
            }
        });

        // Deselect all button : used to uncheck all preferences
        final Button deselectAll = new Button(parent, SWT.PUSH | SWT.CENTER);
        deselectAll.setText("Deselect All");
        deselectAll.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent event) {
                setAllChecked(false);
            }
        });

        // Add two columns to the parent's layout, one for each bouton
        ((GridLayout) parent.getLayout()).numColumns += 2;

        LOGGER.finest("End contributeButtons method");
    }

    /**
     * This methods set all containers boolean value to true.
     * 
     * @param checked
     *            true they are all checked, false otherwise
     */
    public void setAllChecked(final boolean checked) {
        LOGGER.finest("Begin setAllChecked method");

        for (final RulePreferenceContainer container : ruleContainers) {
            container.getRuleCheckButton().setSelection(checked);
            container.setChecked(checked);
        }
        mainComposite.redraw();
        viewer.refresh();

        LOGGER.finest("End setAllChecked method");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.
     * swt.widgets.Composite)
     */
    @Override
    protected Control createContents(final Composite parent) {

        mainComposite = new Composite(parent, SWT.LEFT);

        // Set the layout
        mainComposite.setLayout(new GridLayout());

        // Add a title
        final Label title = new Label(mainComposite, SWT.NONE);
        title.setText(TITLE_PAGE);

        // Create the check box
        createTableViewer(mainComposite);

        return mainComposite;
    }

    /**
     * Create the table viewer.
     * 
     * @param parent
     *            the composite used for the viewer
     */
    private void createTableViewer(final Composite parent) {

        try {

            // define style
            final int style = SWT.H_SCROLL | SWT.V_SCROLL | SWT.SINGLE | SWT.FULL_SELECTION
                    | SWT.BORDER;

            // initialize viewer
            viewer = new TableViewer(parent, style);

            // create columns
            final String[] titles = TITLES;
            final int[] bounds = BOUNDS;
            for (int i = 0; i < titles.length; i++) {
                createColumnViewer(bounds[i], titles[i], i);
            }

            // set content provider
            viewer.setContentProvider(new ArrayContentProvider());

            // Create the list of check box names and corresponding preferences
            ruleContainers.clear();

            // Create the list of check box names and corresponding preferences
            for (final IConfigurationElement contribution : contributions) {
                createContainer(contribution);
            }
            // set input
            viewer.setInput(ruleContainers);

            // set layout data
            final GridData gridData = new GridData();
            gridData.horizontalAlignment = GridData.FILL;
            gridData.verticalAlignment = GridData.FILL;
            gridData.grabExcessHorizontalSpace = true;
            gridData.grabExcessVerticalSpace = true;
            gridData.heightHint = 200;
            gridData.horizontalSpan = 2;
            gridData.verticalSpan = 20;

            final Table table = this.viewer.getTable();
            table.setLayoutData(gridData);

            table.setLinesVisible(true);
            table.setHeaderVisible(true);

            table.pack();

        } catch (final InvalidRegistryObjectException exception) {
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Internal Error", "Contact support service : \n" + exception.getMessage());
        }
    }

    /**
     * Create the column viewer for a determined column.
     * 
     * @param width
     *            width of the column
     * @param title
     *            title of the column
     * @param nbColumn
     *            which columns is concerned
     */
    private void createColumnViewer(final int width, final String title, final int nbColumn) {
        final TableViewerColumn columnViewer = new TableViewerColumn(this.viewer, SWT.NONE);
        final TableColumn column = columnViewer.getColumn();

        column.setWidth(width);
        column.setText(title);
        column.setResizable(true);
        column.setMoveable(true);

        columnViewer.setLabelProvider(new RulePreferenceLabelProvider(nbColumn, viewer));
        columnViewer.setEditingSupport(new RuleEditingSupport(viewer, nbColumn));
    }

    /**
     * Create container for a given contribution
     * 
     * @param contribution
     *            contribution
     */
    private void createContainer(final IConfigurationElement contribution) {
        final String contribId = contribution.getAttribute(PreferencesUIUtils.CONTRIB_ID);
        final String name = contribution.getAttribute(PreferencesUIUtils.CONTRIB_NAME);

        final RulePreferenceContainer container = new RulePreferenceContainer(contribId, name,
                getPreferenceStore().getString(contribId + PreferencesUIUtils.CRITICAL),
                getPreferenceStore().getBoolean(contribId));

        // Create new check button true default value
        final Button button = new Button(this.viewer.getTable(), SWT.CHECK | SWT.LEFT);
        button.setSelection(getPreferenceStore().getBoolean(contribId));
        button.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent event) {
                container.setChecked(!container.isChecked());
            }
        });

        container.setRuleCheckButton(button);
        ruleContainers.add(container);
    }

    @Override
    public void setInitializationData(IConfigurationElement config, String propertyName,
            Object data) throws CoreException {
        extension = PreferencesUIUtils.setInitializationData(config, propertyName, data);

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
     */
    @Override
    public void performDefaults() {
        LOGGER.finest("Begin performDefaults method");

        for (final RulePreferenceContainer container : ruleContainers) {
            LOGGER.finest("Set " + container.getRuleId() + " to default");
            container.setToDefault();
        }
        mainComposite.redraw();
        viewer.refresh();

        LOGGER.finest("End performDefaults method");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performApply()
     */
    @Override
    public void performApply() {
        LOGGER.finest("Begin performApply method");

        for (final RulePreferenceContainer container : ruleContainers) {
            container.storePreference();
        }

        LOGGER.finest("End performApply method");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performOk()
     */
    @Override
    public boolean performOk() {
        LOGGER.finest("Begin performOk method");

        performApply();

        LOGGER.finest("End performOk method");
        return super.performOk();
    }

}
