/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.exception;

/**
 * Exception throw when the resource is empty : a folder or a project has no
 * members.
 */
public class EmptyResourceException extends Exception {

	/**
	 * Serial Version UID.
	 */
	private static final long serialVersionUID = 6504785843304862870L;

	/**
	 * Default constructor.
	 */
	public EmptyResourceException() {
		super();
	}

	/**
	 * Constructor with a message as parameter.
	 * 
	 * @param message
	 *            The exception message.
	 */
	public EmptyResourceException(final String message) {
		super(message);
	}

}
