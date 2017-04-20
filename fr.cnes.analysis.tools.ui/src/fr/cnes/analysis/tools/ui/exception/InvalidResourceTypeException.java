/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.exception;

/**
 * This exception is thrown whenever {IResource}'s type is ROOT, while its
 * parent is a project or a folder.
 * 
 */
public class InvalidResourceTypeException extends Exception {

	/**
	 * Serial version UID.
	 */
	private static final long serialVersionUID = -8619336880299930548L;

	/**
	 * Constructor with message.
	 * 
	 * @param message
	 *            the error message.
	 */
	public InvalidResourceTypeException(final String message) {
		super(message);
	}
}
