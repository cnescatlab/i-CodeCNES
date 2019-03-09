/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.exception;

/**
 * This exception is thrown whenever { IResource}'s type is not FILE, nor
 * FOLDER, nor PROJECT, nor ROOT (which are the only 4 options).
 * 
 */
public class UnknownResourceTypeException extends Exception {

    /**
     * Auto-generated serialVersionUID.
     */
    private static final long serialVersionUID = 4095575983823504009L;

    /**
     * Constructor with message.
     * 
     * @param message
     *            the error message.
     */
    public UnknownResourceTypeException(final String message) {
        super(message);
    }

}
