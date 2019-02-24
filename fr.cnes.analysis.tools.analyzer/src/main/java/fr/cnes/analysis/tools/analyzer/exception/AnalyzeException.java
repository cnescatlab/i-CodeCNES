/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */ 
package fr.cnes.analysis.tools.analyzer.exception;

/**
 * Analyze exception is thrown to the GUI when a technical exception occurred.
 */
public class AnalyzeException extends Exception {
    
    /**
     * Serial Version UID.
     */
    private static final long serialVersionUID = -1061141881144637142L;
    
    /**
     * Constructor with original exception.
     * 
     * @param exception
     *            the original exception.
     */
    public AnalyzeException(final Exception exception) {
        super(exception);
    }
    
}
