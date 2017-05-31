/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/

package fr.cnes.analysis.tools.shell.rules.COM.FLOW.FileExistence;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;

import org.eclipse.core.runtime.FileLocator;
import org.junit.Test;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.shell.rules.COMFLOWFileExistence;
import fr.cnes.analysis.tools.shell.rules.TestUtils;

/**
 * This class aims to test Tr.IfElse rule. There are 2 functions in this class.
 * The first one verifies that an error in a file is detected whenever there is
 * one, the other verifies that nothing is detected when there's no error.
 * 
 */
public class TestCOMFLOWFileExistence {

	public final static String ERROR_FILE = "error.sh";
	public final static String NO_ERROR_FILE = "noError.sh";
	public final static int[] LINES = { 8, 9, 11, 13, 15, 17, 18, 18, 20, 23, 26, 28 };
	public final static String[] LOCATIONS = { "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM",
			"MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM",
			"MAIN PROGRAM", "MAIN PROGRAM" };
	public final AbstractChecker rule = new COMFLOWFileExistence();

	/**
	 * This test verifies that an error can be detected.
	 */
	@Test
	public void testRunWithError() {

		try {
			// Initializing rule and getting error file.
			final File file = new File(FileLocator.resolve(getClass().getResource(ERROR_FILE)).getFile());

			// Defining file in the rule instantiation.
			this.rule.setContribution(TestUtils.getContribution("", ""));
			this.rule.setInputFile(file);

			// Running rule
			final List<CheckResult> list = this.rule.run();

			// We verify that there is an error.
			assertFalse("No error found.", list.isEmpty());

			// We verify that there is the right number of errors

			final int nb_CheckResults = list.size();
			assertEquals("Wrong number of CheckResults : ", LINES.length, nb_CheckResults);

			// We verify that the error detected is the right one. There is
			// only one case of error : a blank common (with no name) is
			// detected.
			final String fileName = list.get(0).getFile().getName();
			assertEquals("Wrong file name : ", ERROR_FILE, fileName);

			// We verify the values
			for (final CheckResult value : list) {
				final Integer index = list.indexOf(value);
				final String location = value.getLocation();
				assertTrue("CheckResult " + index.toString() + " has wrong location : " + location + " should contain "
						+ LOCATIONS[index], location.contains(LOCATIONS[index]));
				final int line = value.getLine();
				assertEquals("CheckResult " + index.toString() + " is in wrong line : ", LINES[index], line);
			}
		} catch (final FileNotFoundException e) {
			fail("Erreur d'analyse (FileNotFoundException)");
		} catch (final IOException e) {
			fail("Erreur d'analyse (IOException)");
		} catch (final JFlexException e) {
			fail("Erreur d'analyse (JFlexException)");
		}
	}

	/**
	 * This test verifies nothing is returned when there's no error.
	 */
	@Test
	public void testRunWithoutError() {
		try {
			// Initializing rule and getting error file.
			final File file = new File(FileLocator.resolve(getClass().getResource(NO_ERROR_FILE)).getFile());

			// Defining file in the rule instantiation.
			this.rule.setContribution(TestUtils.getContribution("", ""));
			this.rule.setInputFile(file);

			// Running rule
			final List<CheckResult> list = this.rule.run();

			// We verify that there is an error.

			assertTrue("Error(s) are detected : " + TestUtils.getCheckResults(list), list.isEmpty());

		} catch (final FileNotFoundException e) {
			fail("Erreur d'analyse (FileNotFoundException)");
		} catch (final IOException e) {
			fail("Erreur d'analyse (IOException)");
		} catch (final JFlexException e) {
			fail("Erreur d'analyse (JFlexException)");
		}
	}
}
