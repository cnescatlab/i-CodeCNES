/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.exception;

/**
 * Exception throw when the package explorer selection is empty.
 */
public class EmptySelectionException extends Exception {

    /**
     * Serial Version UID.
     */
    private static final long serialVersionUID = 4395611590755520580L;

    /**
     * Default constructor.
     */
    public EmptySelectionException() {
        super();
    }

    /**
     * Constructor with a message as parameter.
     * 
     * @param message
     *            The exception message.
     */
    public EmptySelectionException(final String message) {
        super(message);
    }

}
