/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.datas;

import com.google.common.collect.Lists;
import fr.cnes.icode.services.checkers.CheckerContainer;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import java.util.List;

/**
 * Define a list checkers.
 */
@XmlRootElement(name="checkers")
public class CheckersList {

    /** List of checkers defined in a plugin. **/
    @XmlElement(name="check")
    public List<CheckerContainer> containers;

    /**
     * Default constructor.
     */
    public CheckersList() {
        this.containers = Lists.newArrayList();
    }

}
