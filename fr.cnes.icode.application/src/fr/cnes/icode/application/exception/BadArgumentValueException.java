/************************************************************************************************/
/* i-Code CNES is a static code analyzer. */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html */
/************************************************************************************************/
package fr.cnes.icode.application.exception;

/**
 * This exception should be thrown when no contributor could be reached while
 * requesting a contributor on an element inside.
 */
public class BadArgumentValueException extends Exception {
	
    /**
	 * Serial version UID.
	 */
	private static final long serialVersionUID = -6209005999854926693L;

    /**
     * Default constructor.
     */
    public BadArgumentValueException() {
        super();
    }

    /**
     * Constructor with a message as parameter.
     * 
     * @param message
     *            The exception message.
     */
    public BadArgumentValueException(final String message) {
        super(message);
    }
}
