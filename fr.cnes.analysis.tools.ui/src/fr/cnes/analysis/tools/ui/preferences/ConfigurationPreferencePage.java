package fr.cnes.analysis.tools.ui.preferences;

import fr.cnes.analysis.tools.analyzer.exception.NullContributionException;
import fr.cnes.analysis.tools.ui.Activator;
import fr.cnes.analysis.tools.ui.configurations.ConfigurationContainer;
import fr.cnes.analysis.tools.ui.configurations.ConfigurationService;
import fr.cnes.analysis.tools.ui.images.ImageFactory;
import fr.cnes.analysis.tools.ui.preferences.checkerstables.CheckerMetricTableViewer;
import fr.cnes.analysis.tools.ui.preferences.checkerstables.CheckerTableViewer;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import org.eclipse.core.runtime.CoreException;
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
import org.eclipse.ui.plugin.AbstractUIPlugin;

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
    private CheckerTableViewer checkersTable;
    /** Table viewer containing the metrics */
    private CheckerMetricTableViewer checkersMetricTable;

    @Override
    public void init(IWorkbench workbench) {
        // Page description
        setImageDescriptor(AbstractUIPlugin.imageDescriptorFromPlugin(Activator.PLUGIN_ID,
                "./icons/logo-i-code-rouge-45x45.png"));
        setDescription("This preference page is dedicated to iCode analysis. On this page,"
                + " you can enable/disable language and checker that should be run during the "
                + "analysis.");

        // Associate preference store
        final IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        setPreferenceStore(store);
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
        final Label info = new Label(parent, SWT.NONE);
        info.setText("Do you wish to choose a contributor configuration ?");
        configurationSelection = new Combo(parent, SWT.READ_ONLY);
        configurationSelection.add(UserPreferencesService.PREF_CONFIGURATION_CUSTOMVALUE);
        configurationId = UserPreferencesService.getConfigurationName();
        final List<ConfigurationContainer> configs = ConfigurationService.getConfigurations();
        for (ConfigurationContainer config : configs) {
            configurationSelection.add(config.getName());
        }
        if (configurationSelection.indexOf(UserPreferencesService.getConfigurationName()) == -1) {
            // TODO throw ERROR
        } else {
            configurationSelection.select(
                    configurationSelection.indexOf(UserPreferencesService.getConfigurationName()));
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
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        final ExpandBar expandBar = new ExpandBar(composite, SWT.V_SCROLL);
        expandBar.setToolTipText("Choose rules and languages that should be enabled.");
        final GridLayout layout = new GridLayout();
        layout.makeColumnsEqualWidth = true;
        final Color expandBarColor = new Color(parent.getBackground().getDevice(),
                parent.getBackground().getRed() - 10, parent.getBackground().getGreen() - 10,
                parent.getBackground().getBlue() - 10);

        expandBar.setBackground(expandBarColor);

        List<CheckerPreferencesContainer> metrics = new ArrayList<>();
        List<CheckerPreferencesContainer> rules = new ArrayList<>();
        for (CheckerPreferencesContainer checker : preferences) {
            if (checker.isMetric()) {
                metrics.add(checker);
            } else {
                rules.add(checker);
            }
        }

        // Expand item for Rules :
        final Composite checkersExpandBarContainer = new Composite(expandBar, SWT.EMBEDDED);
        checkersExpandBarContainer.setLayout(layout);

        final ExpandItem ruleExpandItem = new ExpandItem(expandBar, SWT.NONE, 0);
        ruleExpandItem.setText("Rules");
        ruleExpandItem.setImage(ImageFactory.getImage(ImageFactory.ERROR_SMALL));
        checkersTable = new CheckerTableViewer(checkersExpandBarContainer, rules);
        ruleExpandItem
                .setHeight(checkersExpandBarContainer.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
        ruleExpandItem.setControl(checkersExpandBarContainer);
        // We build the expandItem Metrics;
        final Composite metricsExpandBarContainer = new Composite(expandBar, SWT.EMBEDDED);
        metricsExpandBarContainer.setLayout(layout);

        final ExpandItem metricExpandItem = new ExpandItem(expandBar, SWT.NONE, 0);
        metricExpandItem.setText("Metric");
        metricExpandItem.setImage(ImageFactory.getImage(ImageFactory.ERROR_SMALL));
        checkersMetricTable = new CheckerMetricTableViewer(metricsExpandBarContainer, metrics);
        metricExpandItem
                .setHeight(metricsExpandBarContainer.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
        metricExpandItem.setControl(metricsExpandBarContainer);

        // Then the expandItem Rules
        // Color of the background of the expandbar taking the one of the main
        // window
        configurationSelection.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                configurationId = configurationSelection
                        .getItem(configurationSelection.getSelectionIndex());
            }

        });
        parent.getParent().pack();
        parent.getParent().redraw();
        return composite;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performApply()
     */
    @Override
    public void performApply() {
        if (configurationId.equals(UserPreferencesService.PREF_CONFIGURATION_CUSTOMVALUE)) {
            UserPreferencesService.setDefaultConfiguration();
            for (CheckerPreferencesContainer checker : checkersMetricTable.getInputs()) {
                checker.savePreferences();
            }
            for (CheckerPreferencesContainer checker : checkersTable.getInputs()) {
                checker.savePreferences();
            }
        } else {

            try

            {
                UserPreferencesService.setConfiguration(configurationId);
            } catch (NullContributionException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        this.refresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
     */
    @Override
    public void performDefaults() {

        for (CheckerPreferencesContainer preference : preferences) {
            preference.setToDefault();
        }
        UserPreferencesService.setDefaultConfiguration();
        configurationSelection.select(
                configurationSelection.indexOf(UserPreferencesService.getConfigurationName()));
        this.refresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performOk()
     */
    @Override
    public boolean performOk() {
        this.performApply();
        return super.performOk();
    }

    /**
     * Redraw every elements of the view.
     */
    public void refresh() {
        checkersMetricTable.refresh();
        checkersTable.refresh();
        composite.redraw();

    }
}
