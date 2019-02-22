/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.shell.metrics;

import fr.cnes.analysis.tools.analyzer.datas.CheckersDefinition;

/**
 * Define checkers to be supported by this plugin.
 */
public class ShellMetricsDefinition extends CheckersDefinition {

    /**
     * Default constructor.
     */
    public ShellMetricsDefinition() {
        super();
    }

    /**
     * Override this method to inject checkers in containers field.
     */
    @Override
    public void define() {
        addFromResources("/shell-metrics.xml");
    }

}
