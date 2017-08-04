/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.exception;

/**
 * Exception throw when the page load is empty.
 */
public class EmptyNodeException extends Exception {

    /**
     * Serial version UID.
     */
    private static final long serialVersionUID = -2622872998164155942L;

    /**
     * Default constructor.
     */
    public EmptyNodeException() {
        super();
    }

    /**
     * Constructor with a message as parameter.
     * 
     * @param message
     *            The exception message.
     */
    public EmptyNodeException(final String message) {
        super(message);
    }

}
