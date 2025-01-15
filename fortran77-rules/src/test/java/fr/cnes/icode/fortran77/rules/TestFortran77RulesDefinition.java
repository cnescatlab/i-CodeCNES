/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.fortran77.rules;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class TestFortran77RulesDefinition {

    @Test
    public void testDefinitionOfDefaultRules() {
        final Fortran77RulesDefinition rulesDefinition = new Fortran77RulesDefinition();

        rulesDefinition.define();

        Assertions.assertEquals(65, rulesDefinition.list().size());
    }

}
