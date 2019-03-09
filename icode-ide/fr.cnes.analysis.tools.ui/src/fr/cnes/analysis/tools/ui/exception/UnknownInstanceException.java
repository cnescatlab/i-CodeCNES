/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.exception;

/**
 * Exception throw when the page load is empty.
 */
public class UnknownInstanceException extends Exception {

    /**
     * Serial version UID.
     */
    private static final long serialVersionUID = 431826525188171840L;

    /**
     * Default constructor.
     */
    public UnknownInstanceException() {
        super();
    }

    /**
     * Constructor with a message as parameter.
     * 
     * @param message
     *            The exception message.
     */
    public UnknownInstanceException(final String message) {
        super(message);
    }

}
