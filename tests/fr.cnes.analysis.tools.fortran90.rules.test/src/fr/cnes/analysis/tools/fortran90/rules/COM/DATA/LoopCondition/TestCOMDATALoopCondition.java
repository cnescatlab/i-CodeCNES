/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules.COM.DATA.LoopCondition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.junit.Test;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.fortran90.rules.COMDATALoopCondition;
import fr.cnes.analysis.tools.fortran90.rules.TestUtils;

public class TestCOMDATALoopCondition {

	public final static String ERROR_FILE = "error.f90";
	public final static String NO_ERROR_FILE = "noError.f90";
	public final static int[] LINES = { 4, 5, 6, 12, 17, 23, 29 };
	public final static String[] LOCATIONS = { "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM",
			"MAIN PROGRAM", "MAIN PROGRAM", "FUNCTION TEST" };
	public final AbstractRule rule = new COMDATALoopCondition();

	/**
	 * This test verifies that an error can be detected.
	 */
	@Test
	public void testRunWithError() {

		try {
			// Initializing rule and getting error file.
			final IPath file = new Path(FileLocator.resolve(getClass().getResource(ERROR_FILE)).getFile());

			// Defining file in the rule instantiation.
			this.rule.setContribution(TestUtils.getContribution("", ""));
			this.rule.setInputFile(file);

			// Running rule
			final List<Violation> list = this.rule.run();

			// We verify that there is an error.
			assertFalse("No error found.", list.isEmpty());

			// We verify that there is the right number of errors

			final int nb_violations = list.size();
			assertEquals("Wrong number of violations : ", LINES.length, nb_violations);

			// We verify that the error detected is the right one. There is
			// only one case of error : a blank common (with no name) is
			// detected.
			final String fileName = list.get(0).getFilePath().toFile().getName();
			assertEquals("Wrong file name : ", ERROR_FILE, fileName);

			// We verify the values
			for (final Violation value : list) {
				final Integer index = list.indexOf(value);
				final String location = value.getLocation();
				assertTrue("Violation " + index.toString() + " has wrong location : " + location + " should contain "
						+ LOCATIONS[index], location.contains(LOCATIONS[index]));
				final int line = value.getLine();
				assertEquals("Violation " + index.toString() + " is in wrong line : ", LINES[index], line);
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
			final IPath file = new Path(FileLocator.resolve(getClass().getResource(NO_ERROR_FILE)).getFile());

			// Defining file in the rule instantiation.
			this.rule.setContribution(TestUtils.getContribution("", ""));
			this.rule.setInputFile(file);

			// Running rule
			final List<Violation> list = this.rule.run();

			// We verify that there is an error.

			assertTrue("Error(s) are detected : " + TestUtils.getViolations(list), list.isEmpty());

		} catch (final FileNotFoundException e) {
			fail("Erreur d'analyse (FileNotFoundException)");
		} catch (final IOException e) {
			fail("Erreur d'analyse (IOException)");
		} catch (final JFlexException e) {
			fail("Erreur d'analyse (JFlexException)");
		}
	}
}
