/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */ 
package fr.cnes.analysis.tools.analyzer.exception;

/**
 * Exception throw when the provider load is empty.
 */
public class NullContributionException extends Exception {

    /**
     * Serial version UID.
     */
    private static final long serialVersionUID = 4937959051256382779L;

    /**
     * Default constructor.
     */
    public NullContributionException() {
        super();
    }

    /**
     * Constructor with a message as parameter.
     * 
     * @param message
     *            The exception message.
     */
    public NullContributionException(final String message) {
        super(message);
    }

}
