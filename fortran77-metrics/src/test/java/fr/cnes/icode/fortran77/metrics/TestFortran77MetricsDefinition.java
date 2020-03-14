/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.fortran77.metrics;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class TestFortran77MetricsDefinition {

    @Test
    public void testDefinitionOfDefaultRules() {
        final Fortran77MetricsDefinition rulesDefinition = new Fortran77MetricsDefinition();

        rulesDefinition.define();

        Assertions.assertEquals(5, rulesDefinition.list().size());
    }

}
