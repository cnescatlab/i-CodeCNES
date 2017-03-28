/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferencepage;

import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.ui.utils.PreferencesUIUtils;

/**
 * Main preference page. This preference page let the user to choose on which
 * language(s) the analysis might be done.
 */
public class MainPreferencePage extends PreferencePage implements IWorkbenchPreferencePage {
    /** Logger. **/
    private static final Logger LOGGER = Logger.getLogger(MainPreferencePage.class.getName());

    /** Composite containing the preferences. **/
    private Composite mainComposite;
    /**
     * Container for preferences. A container is composed of the id of the
     * boolean preference, its name and a boolean which represents if the
     * preference is true or false.
     **/
    private final transient List<LangagePreferenceContainer> containers = new LinkedList<LangagePreferenceContainer>();

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    @Override
    public void init(final IWorkbench workbench) {
        LOGGER.finest("Begin init method");

        // Page description
        setDescription("This preference page is dedicated to iCode analysis. On this page,"
                + " you can select on which language(s) rule / metric analysis will be done. \n\nPlease"
                + " select language(s) on which apply analysis :");

        // Associate preference store
        final IPreferenceStore store = PlatformUI.getPreferenceStore();
        setPreferenceStore(store);

        final IConfigurationElement[] contributions = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(PreferencesUIUtils.PREF_EXT_PT_ID);

        for (final IConfigurationElement contribution : contributions) {
            store.setDefault(contribution.getAttribute(PreferencesUIUtils.PARENT_ID), true);
        }

        LOGGER.finest("End init method");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createContents(final Composite parent) {
        LOGGER.finest("Begin createContents method");

        mainComposite = new Composite(parent, SWT.LEFT);

        // Sets the layout data for the top composite's
        // place in its parent's layout.
        mainComposite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        // Sets the layout for the top composite's
        // children to populate.
        mainComposite.setLayout(new GridLayout());

        for (final IConfigurationElement contribution : PreferencesUIUtils.getAllContributions()) {
            createContainer(contribution);
        }

        LOGGER.finest("End createContents method");
        return mainComposite;
    }

    /**
     * Create container.
     * 
     * @param contribution
     *            Contribution relative to the container.
     */
    protected void createContainer(final IConfigurationElement contribution) {
        final String contribId = contribution.getAttribute(PreferencesUIUtils.PARENT_ID);
        final String name = contribution.getAttribute(PreferencesUIUtils.CONTRIB_NAME);
        final LangagePreferenceContainer container = new LangagePreferenceContainer(contribId, name,
                getPreferenceStore().getBoolean(contribId));

        // Create new check button true default value
        final Button button = new Button(this.mainComposite, SWT.CHECK | SWT.LEFT);
        button.setText("Enable " + name + " analysis");
        button.setSelection(getPreferenceStore().getBoolean(contribId));
        button.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent event) {
                container.setChecked(!container.isChecked());
            }
        });

        container.setButton(button);
        containers.add(container);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
     */
    @Override
    public void performDefaults() {
        LOGGER.finest("Begin performDefaults method");

        for (final LangagePreferenceContainer container : containers) {
            LOGGER.finest("Set " + container.getPrefId() + " to default");
            container.setToDefault();
        }
        mainComposite.redraw();

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

        for (final LangagePreferenceContainer container : containers) {
            LOGGER.finest("Store" + container.getPrefId() + " value in the preference store.");
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

        this.performApply();

        LOGGER.finest("End performOk method");
        return super.performOk();
    }

}
