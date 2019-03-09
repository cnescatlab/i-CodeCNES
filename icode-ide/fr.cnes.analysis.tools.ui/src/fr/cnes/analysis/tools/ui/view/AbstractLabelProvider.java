/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view;

import org.eclipse.jface.viewers.ColumnLabelProvider;

import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;

/**
 * Label provider for columns of viewers
 *
 */
public abstract class AbstractLabelProvider extends ColumnLabelProvider {

    /** Class name */
    private static final String CLASS = AbstractLabelProvider.class.getName();
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
        final String method = "AbstractLabelProvider";
        ICodeLogger.entering(CLASS, method, Integer.valueOf(pType));
        this.type = pType;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Getter for the type.
     * 
     * @return the type
     */
    public int getType() {
        final String method = "getType";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, Integer.valueOf(this.type));
        return this.type;
    }

    /**
     * Setter for the type.
     * 
     * @param pType
     *            the type to set
     */
    public void setType(final int pType) {
        final String method = "setType";
        ICodeLogger.entering(CLASS, method, Integer.valueOf(pType));
        this.type = pType;
        ICodeLogger.exiting(CLASS, method);
    }
}
