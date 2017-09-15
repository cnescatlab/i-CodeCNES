/************************************************************************************************/
/* i-Code CNES is a static code analyzer. */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html */
/************************************************************************************************/
package fr.cnes.analysis.tools.export.exception;

/**
 * This exception should be thrown when no contributor could be reached while
 * requesting a contributor on an element inside.
 */
public class NoContributorMatchingException extends Exception {
    /**
     * Serial version UID.
     */
    private static final long serialVersionUID = -2478597633245453088L;

    /**
     * Default constructor.
     */
    public NoContributorMatchingException() {
        super();
    }

    /**
     * Constructor with a message as parameter.
     * 
     * @param message
     *            The exception message.
     */
    public NoContributorMatchingException(final String message) {
        super(message);
    }
}
