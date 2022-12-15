/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.fortran90.rules;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class TestFortran90RulesDefinition {

    @Test
    public void testDefinitionOfDefaultRules() {
        final Fortran90RulesDefinition rulesDefinition = new Fortran90RulesDefinition();

        rulesDefinition.define();

        Assertions.assertEquals(68, rulesDefinition.list().size());
    }

}
