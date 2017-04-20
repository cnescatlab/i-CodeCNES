/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferencepage;

import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.InvalidRegistryObjectException;
import org.eclipse.jface.dialogs.MessageDialog;
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
 * This is the preference page for Fortran 77. This page only gives the choice
 * to allow rule and/or metric analysis.
 */
public class LanguagePreferencePage extends PreferencePage
        implements IWorkbenchPreferencePage, IExecutableExtension {

    /** Logger. **/
    private static final Logger LOGGER = Logger.getLogger(LanguagePreferencePage.class.getName());

    /** The extension corresponding to this preference page **/
    private IConfigurationElement extension;

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
     * org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org
     * .eclipse.core.runtime.IConfigurationElement, java.lang.String,
     * java.lang.Object)
     */
    @Override
    public void setInitializationData(final IConfigurationElement config, final String propertyName,
            final Object data) throws CoreException {
        setExtension(PreferencesUIUtils.setInitializationData(config, propertyName, data));
    }

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
        setDescription("This preference page concerns rule/metrics analysis."
                + "On this page, you can select whether rule and/or metric analysis is allowed."
                + "\n\nPlease select which analysis can be done :");

        // Associate preference store
        final IPreferenceStore store = PlatformUI.getPreferenceStore();
        setPreferenceStore(store);

        // default value
        store.setDefault(extension.getAttribute(PreferencesUIUtils.RULE_PAGE_ID), true);
        store.setDefault(extension.getAttribute(PreferencesUIUtils.METRIC_PAGE_ID), true);

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

        try {
            mainComposite = new Composite(parent, SWT.LEFT);

            // Sets the layout data for the top composite's
            // place in its parent's layout.
            mainComposite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

            // Sets the layout for the top composite's
            // children to populate.
            mainComposite.setLayout(new GridLayout());

            // Clear the container
            containers.clear();

            // Create new boolean field editor with true default value for
            // rule and metric
            createField(this.getExtension().getAttribute(PreferencesUIUtils.METRIC_PAGE_ID), false);
            createField(this.getExtension().getAttribute(PreferencesUIUtils.RULE_PAGE_ID), true);

        } catch (final InvalidRegistryObjectException exception) {
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Internal Error", "Contact support service : \n" + exception.getMessage());
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
        }

        LOGGER.finest("End createContents method");

        return mainComposite;
    }

    /**
     * This method creates a boolean field editor which is for rule or metric
     * permission.
     * 
     * @param evalId
     *            the id of rule / metric preference page corresponding (which
     *            will also be the ID of the preference in the store)
     * @param isRule
     *            true if the field is for rule's permission, false if it's
     *            dedicated to the metric's one
     */
    private void createField(final String evalId, final boolean isRule) {
        LOGGER.finest("Begin createField method");

        // Creating the label of the field
        String message = "";
        if (isRule) {
            message = "Enable rule analysis";
        } else {
            message = "Enable metric analysis";
        }

        final LangagePreferenceContainer container = new LangagePreferenceContainer(evalId, message,
                getPreferenceStore().getBoolean(evalId));

        // Create new check button true default value
        final Button button = new Button(mainComposite, SWT.CHECK | SWT.LEFT);
        button.setText(message);
        button.setSelection(getPreferenceStore().getBoolean(evalId));
        button.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent event) {
                container.setChecked(!container.isChecked());
            }
        });

        container.setButton(button);
        containers.add(container);

        LOGGER.finest("End createField method");
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

    /**
     * Getter for the extension.
     * 
     * @return this page's extension
     */
    public IConfigurationElement getExtension() {
        return this.extension;
    }

    /**
     * Setter for the extension.
     * 
     * @param pExtension
     *            the new extension
     */
    public void setExtension(final IConfigurationElement pExtension) {
        this.extension = pExtension;
    }

}
