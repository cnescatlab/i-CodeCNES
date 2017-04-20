/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.services;

import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.AbstractSourceProvider;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.services.IServiceLocator;

import fr.cnes.analysis.tools.ui.preferencepage.LangagePreferenceContainer;

/**
 * This class is the structure to provide a variable into plugin.xml to enable
 * menu analysis.
 * 
 * 
 */
public class LanguageSourceProvider extends AbstractSourceProvider implements IStartup {
    /** Extension point id **/
    private static final String SRC_EXT_PT_ID = "fr.cnes.analysis.tools.services";

    /** Static value for extension point attribute called id. **/
    private static final String CONTRIB_ID = "id";

    /** Defines if languages are enabled. **/
    private Map<String, Boolean> enabled;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.AbstractSourceProvider#initialize(org.eclipse.ui.services
     * .IServiceLocator)
     */
    @Override
    public void initialize(final IServiceLocator locator) {
        super.initialize(locator);
        this.enabled = new Hashtable<String, Boolean>();
        for (final IConfigurationElement contribution : Platform.getExtensionRegistry()
                .getConfigurationElementsFor(SRC_EXT_PT_ID)) {
            final String contribId = contribution.getAttribute(CONTRIB_ID);
            final boolean value = PlatformUI.getPreferenceStore().getBoolean(contribId);
            this.enabled.put(contribId, value);
            this.fireSourceChanged(ISources.WORKBENCH, contribId, value);
        }
    }

    /**
     * Getter for enabled.
     * 
     * @return true if language is enabled, false otherwise
     */
    public Map<String, Boolean> getEnabled() {
        return this.enabled;
    }

    /**
     * Setter for enabled.
     * 
     * @param pEnabled
     *            the hash table value to set
     */
    public void setEnabled(final Map<String, Boolean> pEnabled) {
        this.enabled = pEnabled;
    }

    /**
     * Setter for one element of enabled.
     * 
     * @param pEnabled
     *            the container which contains id and value to set
     */
    public void setEnabled(final LangagePreferenceContainer pEnabled) {
        // this.setEnabled(pEnabled.getPrefId(), pEnabled.isChecked());
    }

    /**
     * Setter for one element of enabled.
     * 
     * @param pId
     *            id of the source provider to set
     * @param pEnabled
     *            the value to set
     */
    public void setEnabled(final String pId, final boolean pEnabled) {
        final String key = pId;
        final boolean value = pEnabled;

        this.enabled.remove(key);
        this.enabled.put(key, value);
        this.fireSourceChanged(ISources.WORKBENCH, key, value);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.ISourceProvider#getProvidedSourceNames()
     */
    @Override
    public String[] getProvidedSourceNames() {
        final List<String> list = new LinkedList<String>();
        for (final String key : this.enabled.keySet()) {
            list.add(key);
        }
        return list.toArray(new String[list.size()]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.ISourceProvider#getCurrentState()
     */
    @Override
    public Map<String, Boolean> getCurrentState() {
        this.fireSourceChanged(ISources.WORKBENCH, this.enabled);
        return this.enabled;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IStartup#earlyStartup()
     */
    @Override
    public void earlyStartup() {
        this.enabled = new Hashtable<String, Boolean>();
        for (final IConfigurationElement contribution : Platform.getExtensionRegistry()
                .getConfigurationElementsFor(SRC_EXT_PT_ID)) {
            final String contribId = contribution.getAttribute(CONTRIB_ID);
            final boolean value = PlatformUI.getPreferenceStore().getBoolean(contribId);
            this.enabled.put(contribId, value);
            this.fireSourceChanged(ISources.WORKBENCH, contribId, value);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.ISourceProvider#dispose()
     */
    @Override
    public void dispose() {
        final Map<String, Boolean> map = new Hashtable<String, Boolean>();
        this.setEnabled(map);
    }
}
