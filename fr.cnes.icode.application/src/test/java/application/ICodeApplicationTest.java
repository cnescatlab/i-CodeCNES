/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package application;

import fr.cnes.icode.application.ICodeApplication;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.logging.Logger;

/**
 * This class provide a main command line application to
 * use i-Code in a console without GUI.
 * 
 * @author lequal
 */
public class ICodeApplicationTest {
	
	/**
	 * Logger for the current class @see /logging.properties for more information.
	 */
	private final static Logger LOGGER = Logger.getLogger(ICodeApplicationTest.class.getName());

	@Test
	public void main_test() {
		Assertions.assertTrue(true);
	}

}
