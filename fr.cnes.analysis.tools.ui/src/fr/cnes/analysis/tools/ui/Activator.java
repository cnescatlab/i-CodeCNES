/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

/**
 * As this Activator has been modified, it has to be tested and software quality
 * has to be done. The only modification is done in start method, which loads
 * all preferences used in the project.
 * 
 */
public class Activator extends AbstractUIPlugin implements BundleActivator {
    /** Plug-in ID */
    public static final String PLUGIN_ID = "fr.cnes.analysis.tools.ui";
    /** The static bundle context **/
    private static BundleContext context;
    /** Plugin activator */
    private static Activator plugin;

    /**
     * Static method which returns the bundle context of this activator.
     * 
     * @return the bundle context
     **/
    public static BundleContext getContext() {
        return context;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext
     * )
     */
    @Override
    public void start(final BundleContext bundleContext) throws Exception {
        Activator.context = bundleContext;
        plugin = this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext
     * )
     */
    @Override
    public void stop(final BundleContext bundleContext) throws Exception {
        Activator.context = null;
    }

    /**
     * @return plugin activator.
     */
    public static Activator getDefault() {
        return plugin;
    }

}
