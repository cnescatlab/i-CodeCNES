/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.fortran77.rules;

import fr.cnes.icode.data.CheckersDefinition;

/**
 * Define checkers to be supported by this plugin.
 */
public class Fortran77RulesDefinition extends CheckersDefinition {

    /**
     * Default constructor.
     */
    public Fortran77RulesDefinition() {
        super();
    }

    /**
     * Override this method to inject checkers in containers field.
     */
    @Override
    public void define() {
        addFromResources("/fortran77-rules.xml");
    }

}
