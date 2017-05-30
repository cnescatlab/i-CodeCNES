/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules.F90.DATA.Float;

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

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.fortran90.rules.F90DATAFloat;
import fr.cnes.analysis.tools.fortran90.rules.TestUtils;

/**
 * This class aims to test Don.Declaration rule. There are 2 functions in this
 * class. The first one verifies that an error in a file is detected whenever
 * there is one, the other verifies that nothing is detected when there's no
 * error.
 * 
 */
public class TestF90DATAFloat {

	/** File containing errors to analyze. **/
	public static final String ERROR_FILE = "error.f90";
	/** File without error to analyze. **/
	public static final String NO_ERROR_FILE = "noError.f90";
	/** Lines where error(s) is (are) found. **/
	public static final int[] LINES = { 20, 20 };
	/** Location(s) of the error(s). **/
	public static final String[] LOCATIONS = { "subroutine format_etoile", "subroutine format_etoile" };
	/** The rule to analyze. **/
	public final AbstractRule RULE = new F90DATAFloat();

	/**
	 * This test verifies that an error can be detected.
	 */
	@Test
	public final void testRunWithError() {

		try {
			// Initializing rule and getting error file.
			final File file = new File(FileLocator.resolve(getClass().getResource(ERROR_FILE)).getFile());

			// Defining file in the rule instantiation.
			this.RULE.setContribution(TestUtils.getContribution("", ""));
			this.RULE.setInputFile(file);

			// Running rule
			final List<CheckResult> list = this.RULE.run();

			// We verify that there is an error.
			assertFalse("No error found.", list.isEmpty());

			// We verify that there is the right number of errors

			final int nbCheckResults = list.size();
			assertEquals("Wrong number of CheckResults : ", LINES.length, nbCheckResults);

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
	public final void testRunWithoutError() {
		try {
			// Initializing rule and getting error file.
			final File file = new File(FileLocator.resolve(getClass().getResource(NO_ERROR_FILE)).getFile());

			// Defining file in the rule instantiation.
			this.RULE.setContribution(TestUtils.getContribution("", ""));
			this.RULE.setInputFile(file);

			// Running rule
			final List<CheckResult> list = this.RULE.run();

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
