/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.shell.metrics;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class TestShellMetricsDefinition {

    @Test
    public void testDefinitionOfDefaultRules() {
        final ShellMetricsDefinition rulesDefinition = new ShellMetricsDefinition();

        rulesDefinition.define();

        Assertions.assertEquals(5, rulesDefinition.list().size());
    }

}
