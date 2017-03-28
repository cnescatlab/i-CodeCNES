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
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.ui.preferencepage.metrics.MetricPreferenceContainer;
import fr.cnes.analysis.tools.ui.utils.PreferencesUIUtils;

/**
 * This is the class that defines the structure for preference page concerning
 * metrics.
 */
public class MetricPreferencePage extends PreferencePage
        implements IWorkbenchPreferencePage, IExecutableExtension {

    /** The logger **/
    public static final Logger LOGGER = Logger.getLogger(MetricPreferencePage.class.getName());

    /** Title page **/
    private static final String TITLE_PAGE = "Selected metric(s) to be analyzed: ";

    /** The extension corresponding to this preference page **/
    private IConfigurationElement extension;

    /** List of rules contributing to this language **/
    private List<IConfigurationElement> contributions;

    /** Composite containing the table. */
    private Composite tableComposite;

    /** Combo box relative to the critical level. */
    private Combo criticalLevelCombo;

    /** Composite containing the preferences. **/
    private Composite mainComposite;

    /**
     * Container for preferences. A container is composed of the id of the
     * boolean preference, its name and a boolean which represents if the
     * preference is true or false.
     **/
    private final transient List<MetricPreferenceContainer> metricContainers = new LinkedList<MetricPreferenceContainer>();

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
                .getConfigurationElementsFor(getMetricExtId())) {
            contributions.add(contribution);
            store.setDefault(contribution.getAttribute(PreferencesUIUtils.CONTRIB_ID), true);
        }
        store.setDefault(getMetricExtId() + PreferencesUIUtils.LEVEL, 2);

        LOGGER.finest("End init method");
    }

    @Override
    protected Control createContents(final Composite parent) {

        mainComposite = new Composite(parent, SWT.LEFT);

        // Set the layout
        mainComposite.setLayout(new GridLayout());

        // Add combobox for metrics
        createLevel(this.mainComposite);

        // Add a title
        final GridLayout gridLayout = new GridLayout(1, false);
        mainComposite.setLayout(gridLayout);
        final Label title = new Label(mainComposite, SWT.NONE);
        title.setText(TITLE_PAGE);

        // Create struture for metrics analysis
        tableComposite = new Composite(mainComposite, SWT.NONE);
        createMetricTable(tableComposite);

        return mainComposite;
    }

    /**
     * Create combobox level for metrics
     * 
     * @param parent
     *            Compisite parent.
     */
    protected void createLevel(Composite parent) {
        // parent.setLayout(new GridLayout(2,false));

        final Composite levelComp = new Composite(parent, SWT.NONE);
        final GridLayout lay = new GridLayout(2, false);
        lay.horizontalSpacing = 80;
        levelComp.setLayout(lay);

        final Label levelLabel = new Label(levelComp, SWT.NONE);
        levelLabel.setText("Development level:");

        criticalLevelCombo = new Combo(levelComp, SWT.READ_ONLY | SWT.RIGHT);
        /** Action when event combobox is changed **/
        criticalLevelCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                refreshTable();
            }
        });

        criticalLevelCombo.setItems(PreferencesUIUtils.LEVELS);
        criticalLevelCombo
                .select(getPreferenceStore().getInt(getMetricExtId() + PreferencesUIUtils.LEVEL));

    }

    /**
     * Create table for metrics disposal.
     * 
     * @param parent
     *            composite parent.
     */
    private void createMetricTable(Composite parent) {
        parent.setLayout(new GridLayout());

        final Composite metricComp = new Composite(parent, SWT.NONE);
        final GridLayout lay = new GridLayout(2, false);
        lay.horizontalSpacing = 70;
        metricComp.setLayout(lay);

        final GridData textGridData = new GridData(GridData.FILL_HORIZONTAL);
        textGridData.horizontalAlignment = GridData.HORIZONTAL_ALIGN_END;

        for (int j = 0; j < this.contributions.size(); j++) {
            final IConfigurationElement contribution = contributions.get(j);

            final String contribId = contribution.getAttribute(PreferencesUIUtils.CONTRIB_ID);
            final String name = contribution.getAttribute(PreferencesUIUtils.CONTRIB_NAME);

            // Create new check button true default value
            final Button button = new Button(metricComp, SWT.CHECK | SWT.LEFT);
            button.setText(contribution.getAttribute(PreferencesUIUtils.CONTRIB_NAME));
            button.setSelection(getPreferenceStore().getBoolean(contribId));

            final Text valuetext = new Text(metricComp, SWT.BORDER | SWT.RIGHT);
            textGridData.heightHint = 20;
            textGridData.widthHint = 35;
            valuetext.setLayoutData(textGridData);

            final MetricPreferenceContainer container = new MetricPreferenceContainer(contribId,
                    name, valuetext, button);

            metricContainers.add(container);

            refreshTable();

        }

    }

    /**
     * Refresh Main Composite
     */
    private void refreshTable() {
        for (MetricPreferenceContainer container : metricContainers) {
            final String id = container.getMetricId();
            final Float pValue = getPreferenceStore().getFloat(id + PreferencesUIUtils.VALUE
                    + PreferencesUIUtils.LEVELS[criticalLevelCombo.getSelectionIndex()]);
            container.setValue(pValue);
            container.setEnabled(criticalLevelCombo.getSelectionIndex() == 4);
        }

        mainComposite.layout(true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
     */
    @Override
    public void performDefaults() {
        LOGGER.finest("Begin performDefaults method");
        for (final MetricPreferenceContainer container : metricContainers) {
            LOGGER.finest("Set " + container.getMetricId() + " to default");
            container.setToDefault(
                    PreferencesUIUtils.LEVELS[criticalLevelCombo.getSelectionIndex()]);
        }

        // set default values relative to critical level
        getPreferenceStore().setToDefault(PreferencesUIUtils.LEVEL);
        criticalLevelCombo.select(getPreferenceStore().getDefaultInt(PreferencesUIUtils.LEVEL));

        // refresh
        refreshTable();

        LOGGER.finest("End performDefaults method");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * fr.cnes.analysis.tools.ui.preferencepage.AbstractPreference#performApply(
     * )
     */
    @Override
    public void performApply() {
        LOGGER.finest("Begin performApply method");

        for (final MetricPreferenceContainer container : metricContainers) {
            // store the check and value
            container.storePreference(
                    PreferencesUIUtils.LEVELS[criticalLevelCombo.getSelectionIndex()]);
        }

        // store value relative to critical level
        getPreferenceStore().setValue(PreferencesUIUtils.LEVEL,
                criticalLevelCombo.getSelectionIndex());

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

    /**
     * @param value
     *            the value.
     * @return Index of the value.
     */

    @Override
    public void setInitializationData(IConfigurationElement config, String propertyName,
            Object data) throws CoreException {
        extension = PreferencesUIUtils.setInitializationData(config, propertyName, data);

    }

    /**
     * Retrieve contribution id.
     * 
     * @return the contribution id.
     */
    public String getMetricExtId() {
        return extension.getAttribute(PreferencesUIUtils.METRIC_EXT_ID);
    }

}
