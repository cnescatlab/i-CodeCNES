/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */ 
package fr.cnes.analysis.tools.analyzer.exception;

/**
 * This exception whenever an error occures during JFlex analysis.
 * 
 */
public class JFlexException extends Exception {

	/**
	 * Serial Version UID.
	 */
	private static final long serialVersionUID = 4198004753881804823L;

	/**
	 * Default constructor.
	 */
	public JFlexException() {
		super();
	}

	/**
	 * Constructor with original exception.
	 * 
	 * @param exception
	 *            the original exception.
	 */
	public JFlexException(final Exception exception) {
		super(exception);
	}
}