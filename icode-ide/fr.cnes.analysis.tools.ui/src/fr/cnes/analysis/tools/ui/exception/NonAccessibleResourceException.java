/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.exception;

/**
 * This exception is thrown whenever the resource is not accessible : a folder
 * does not exist, a project does not exist, a project is empty.
 */
public class NonAccessibleResourceException extends Exception {

    /**
     * Serial version UID.
     */
    private static final long serialVersionUID = 6576045126274438859L;

    /**
     * Constructor with message.
     * 
     * @param message
     *            the error message.
     */
    public NonAccessibleResourceException(final String message) {
        super(message);
    }

    /**
     * Constructor with message and original exception.
     * 
     * @param message
     *            the error message.
     * @param exception
     *            original exception.
     */
    public NonAccessibleResourceException(final String message, final Exception exception) {
        super(message, exception);
    }

}
