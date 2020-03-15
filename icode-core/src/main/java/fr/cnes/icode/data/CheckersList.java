/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.data;

import com.google.common.collect.Lists;
import fr.cnes.icode.services.checkers.CheckerContainer;

import java.util.List;

/**
 * Define a list checkers.
 */
public class CheckersList {

    /** List of checkers defined in a plugin. **/
    public List<CheckerContainer> containers;

    /**
     * Default constructor.
     */
    public CheckersList() {
        this.containers = Lists.newArrayList();
    }

}
