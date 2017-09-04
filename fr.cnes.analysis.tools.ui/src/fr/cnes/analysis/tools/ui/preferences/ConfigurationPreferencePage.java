/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferences;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.ExpandBar;
import org.eclipse.swt.widgets.ExpandItem;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import fr.cnes.analysis.tools.analyzer.exception.NullContributionException;
import fr.cnes.analysis.tools.ui.Activator;
import fr.cnes.analysis.tools.ui.configurations.ConfigurationContainer;
import fr.cnes.analysis.tools.ui.configurations.ConfigurationService;
import fr.cnes.analysis.tools.ui.images.ImageFactory;
import fr.cnes.analysis.tools.ui.logger.UILogger;
import fr.cnes.analysis.tools.ui.preferences.checkerstables.CheckersComposite;
import fr.cnes.analysis.tools.ui.preferences.checkerstables.MetricsComposite;

/**
 * i-Code CNES Preferences page.
 */
public class ConfigurationPreferencePage extends PreferencePage
                implements IWorkbenchPreferencePage {

    /** Composite containing the configuration preference page */
    private Composite composite;
    /** List of {@link CheckerPreferencesContainer} displayed on the page */
    private List<CheckerPreferencesContainer> preferences;
    /** Configuration identifier selected */
    private String configurationId;
    /** Combo box to set the configuration */
    private Combo configurationSelection;
    /** Table viewer containing the rules */
    private CheckersComposite rulesExpandBarContainer;
    /** Table viewer containing the metrics */
    private MetricsComposite metricsExpandBarContainer;

    @Override
    public void init(IWorkbench workbench) {
        final String method = "init";
        UILogger.entering(this.getClass().getName(), method, workbench);
        // Page description
        setImageDescriptor(ImageFactory.getDescriptor(ImageFactory.ERROR_BIG));
        setDescription("This preference page is dedicated to iCode analysis. On this page,"
                        + " you can enable/disable language and checker that should be "
                        + "run during the analysis.");

        // Associate preference store
        final IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        setPreferenceStore(store);
        UILogger.exiting(this.getClass().getName(), method);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.
     * swt.widgets.Composite)
     */
    @Override
    protected Control createContents(Composite parent) {
        final String method = "createContents";
        UILogger.entering(this.getClass().getName(), method, parent);
        final Label info = new Label(parent, SWT.NONE);
        info.setText("Do you wish to choose an existing configuration ?");
        configurationSelection = new Combo(parent, SWT.READ_ONLY);
        configurationSelection.add(UserPreferencesService.PREF_CONFIGURATION_CUSTOMVALUE);
        configurationId = UserPreferencesService.getConfigurationName();
        final List<ConfigurationContainer> configs = ConfigurationService.getConfigurations();
        for (ConfigurationContainer config : configs) {
            configurationSelection.add(config.getName());
        }
        if (configurationSelection.indexOf(UserPreferencesService.getConfigurationName()) != -1) {
            configurationSelection.select(configurationSelection
                            .indexOf(UserPreferencesService.getConfigurationName()));
        }

        composite = new Composite(parent, SWT.LEFT);

        // Sets the layout data for the top composite's
        // place in its parent's layout.

        // Sets the layout for the top composite's
        // children to populate.
        composite.setLayout(new FillLayout());
        composite.setBackground(parent.getBackground());

        try {
            preferences = UserPreferencesService.getCheckersPreferences();
            preferences.sort(new Comparator<CheckerPreferencesContainer>() {

                @Override
                public int compare(CheckerPreferencesContainer arg0,
                                CheckerPreferencesContainer arg1) {
                    return arg0.getName().compareTo(arg1.getName());
                }
            });
        } catch (NullContributionException | CoreException e) {
            MessageDialog.openError(getShell(), Activator.PLUGIN_ID, e.getMessage());
        }
        final ExpandBar expandBar = new ExpandBar(composite, SWT.V_SCROLL);
        expandBar.setToolTipText("Choose rules and languages that should be enabled.");
        final GridLayout layout = new GridLayout();
        layout.makeColumnsEqualWidth = true;
        final Color expandBarColor = new Color(parent.getBackground().getDevice(),
                        parent.getBackground().getRed() - 10,
                        parent.getBackground().getGreen() - 10,
                        parent.getBackground().getBlue() - 10);

        expandBar.setBackground(expandBarColor);

        final List<CheckerPreferencesContainer> metrics = new ArrayList<>();
        final List<CheckerPreferencesContainer> rules = new ArrayList<>();
        for (CheckerPreferencesContainer checker : preferences) {
            if (checker.isMetric()) {
                metrics.add(checker);
            } else {
                rules.add(checker);
            }
        }

        // Expand item for Rules :
        rulesExpandBarContainer = new CheckersComposite(expandBar, rules, SWT.EMBEDDED);
        rulesExpandBarContainer.setLayout(layout);

        final ExpandItem ruleExpandItem = new ExpandItem(expandBar, SWT.NONE, 0);
        ruleExpandItem.setText("Rules");
        ruleExpandItem.setImage(ImageFactory.getImage(ImageFactory.ERROR_SMALL));
        ruleExpandItem.setHeight(rulesExpandBarContainer.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
        ruleExpandItem.setControl(rulesExpandBarContainer);
        // We build the expandItem Metrics;
        metricsExpandBarContainer = new MetricsComposite(expandBar, metrics, SWT.EMBEDDED);
        metricsExpandBarContainer.setLayout(layout);

        final ExpandItem metricExpandItem = new ExpandItem(expandBar, SWT.NONE, 0);
        metricExpandItem.setText("Metric");
        metricExpandItem.setImage(ImageFactory.getImage(ImageFactory.ERROR_SMALL));
        metricExpandItem.setHeight(
                        metricsExpandBarContainer.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
        metricExpandItem.setControl(metricsExpandBarContainer);

        // Then the expandItem Rules
        // Color of the background of the expandbar taking the one of the main
        // window
        configurationSelection.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                final String methodName = "widgetSelected";
                UILogger.entering(this.getClass().getName(), methodName, e);
                configurationId = configurationSelection
                                .getItem(configurationSelection.getSelectionIndex());
                refresh();
                UILogger.exiting(this.getClass().getName(), methodName);
            }

        });
        parent.getParent().pack();
        parent.getParent().redraw();
        this.refresh();
        UILogger.exiting(this.getClass().getName(), method, composite);
        return composite;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performApply()
     */
    @Override
    public void performApply() {
        final String method = "performApply";
        UILogger.entering(this.getClass().getName(), method);
        if (configurationId.equals(UserPreferencesService.PREF_CONFIGURATION_CUSTOMVALUE)) {
            UserPreferencesService.setDefaultConfiguration();
            for (CheckerPreferencesContainer checker : metricsExpandBarContainer.getInputs()) {
                checker.savePreferences();
            }
            for (CheckerPreferencesContainer checker : rulesExpandBarContainer.getInputs()) {
                checker.savePreferences();
            }
        } else {
            for (CheckerPreferencesContainer checker : metricsExpandBarContainer.getInputs()) {
                checker.setToDefault();
            }
            for (CheckerPreferencesContainer checker : rulesExpandBarContainer.getInputs()) {
                checker.setToDefault();
            }
            try {
                UserPreferencesService.setConfiguration(configurationId);
            } catch (NullContributionException e) {
                MessageDialog.openError(getShell(), Activator.PLUGIN_ID, e.getMessage());
                UILogger.error(this.getClass().getName(), method, e);
            }
            for (CheckerPreferencesContainer checker : metricsExpandBarContainer.getInputs()) {
                checker.update();
            }
            for (CheckerPreferencesContainer checker : rulesExpandBarContainer.getInputs()) {
                checker.update();
            }
        }
        this.refresh();
        UILogger.exiting(this.getClass().getName(), method);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
     */
    @Override
    public void performDefaults() {
        final String method = "performDefaults";
        UILogger.entering(this.getClass().getName(), method);

        for (CheckerPreferencesContainer checker : metricsExpandBarContainer.getInputs()) {
            checker.setToDefault();
        }
        for (CheckerPreferencesContainer checker : rulesExpandBarContainer.getInputs()) {
            checker.setToDefault();
        }
        UserPreferencesService.setDefaultConfiguration();
        configurationSelection.select(configurationSelection
                        .indexOf(UserPreferencesService.getConfigurationName()));
        this.refresh();
        UILogger.exiting(this.getClass().getName(), method);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performOk()
     */
    @Override
    public boolean performOk() {
        final String method = "performOk";
        UILogger.entering(this.getClass().getName(), method);
        this.performApply();
        UILogger.exiting(this.getClass().getName(), method);
        return super.performOk();
    }

    /**
     * Redraw every elements of the view.
     */
    public void refresh() {
        final String method = "refresh";
        UILogger.entering(this.getClass().getName(), method);
        this.composite.getParent().getParent().redraw();
        this.composite.layout();
        this.composite.redraw();
        metricsExpandBarContainer.refresh();
        rulesExpandBarContainer.refresh();
        UILogger.exiting(this.getClass().getName(), method);
    }
}
