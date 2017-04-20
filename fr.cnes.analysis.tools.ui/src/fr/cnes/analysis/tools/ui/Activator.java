/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

import fr.cnes.analysis.tools.ui.utils.PreferencesUIUtils;

/**
 * As this Activator has been modified, it has to be tested and software quality
 * has to be done. The only modification is done in start method, which loads
 * all preferences used in the project.
 * 
 */
public class Activator implements BundleActivator {

	/** The static bundle context **/
	private static BundleContext context;

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
		// Initialize all preferences
		PreferencesUIUtils.initializePreferences();

		Activator.context = bundleContext;
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

}
