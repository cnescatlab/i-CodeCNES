/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.analysis.tools.analyzer;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

/**
 * Initialize bundle context
 */
public class Activator implements BundleActivator {

    /** Plug-in ID */
    public static final String PLUGIN_ID = "fr.cnes.analysis.tools.analyzer";

    /**
     * Bundle context
     */
    private static BundleContext context;

    /**
     * @return bundle context
     */
    public static BundleContext getContext() {
        return context;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.
     * BundleContext)
     */
    @Override
    public void start(BundleContext bundleContext) throws Exception {
        Activator.context = bundleContext;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
     */
    @Override
    public void stop(BundleContext bundleContext) throws Exception {
        Activator.context = null;
    }

}
