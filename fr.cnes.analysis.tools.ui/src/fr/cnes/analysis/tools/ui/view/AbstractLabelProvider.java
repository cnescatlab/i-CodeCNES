/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view;

import org.eclipse.jface.viewers.ColumnLabelProvider;

public abstract class AbstractLabelProvider extends ColumnLabelProvider {

    /** An integer to determine which column has to be provide. **/
    private int type;

    /**
     * Constructor with integer parameter which represents the column created.
     * 
     * @param pType
     *            the column to create
     */
    public AbstractLabelProvider(final int pType) {
        super();
        this.type = pType;
    }

    /**
     * Getter for the type.
     * 
     * @return the type
     */
    public int getType() {
        return this.type;
    }

    /**
     * Setter for the type.
     * 
     * @param pType
     *            the type to set
     */
    public void setType(final int pType) {
        this.type = pType;
    }
}
