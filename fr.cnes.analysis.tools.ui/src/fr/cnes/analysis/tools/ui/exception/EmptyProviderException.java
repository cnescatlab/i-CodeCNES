/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.exception;

/**
 * Exception throw when the provider load is empty.
 */
public class EmptyProviderException extends Exception {
	/**
	 * Serial version UID.
	 */
	private static final long serialVersionUID = -1423752077318249456L;

	/**
	 * Default constructor.
	 */
	public EmptyProviderException() {
		super();
	}

	/**
	 * Constructor with a message as parameter.
	 * 
	 * @param message
	 *            The exception message.
	 */
	public EmptyProviderException(final String message) {
		super(message);
	}
}
