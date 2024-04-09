/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/

package fr.cnes.icode.test;

import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;
import fr.cnes.icode.exception.JFlexException;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.File;
import java.io.IOException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * This class aims to test all i-Code rules. There are 2 functions in this class.
 * The first one verifies that an error in a file is detected whenever there is
 * one, the other verifies that nothing is detected when there's no error.
 * <p>
 * These functions test all rules with values provided by parametrized test.
 * <p>
 * If you want to add tests to i-Code's checkers, you just need to implement this
 * class and override static Object[][] data() with your data set.
 */
public interface ICodeCheckerTester {

    /**
     * This List represent the data set and contains:
     * - path to an errored file
     * - path to a correct file
     * - array of line raising errors
     * - array of function raising errors
     * - class of the checker to apply to previous files
     *
     * @return List of test data.
     */
    static Object[][] data() {
        return new Object[0][];
    }

    /**
     * This test verifies that an error can be detected.
     *
     * @param errorFile   path to an errored file
     * @param noErrorFile path to a correct file
     * @param lines       array of line raising errors
     * @param locations   array of function raising errors
     * @param checker     class of the checker to apply to previous files
     */
    @ParameterizedTest(name = "Test {index} should found errors in {0}")
    @MethodSource("data")
    default void testRunWithError(final String errorFile, final String noErrorFile, final int[] lines,
                                  final String[] locations, final Class<?> checker) {

        // Container for checker instantiation.
        AbstractChecker rule;

        try {
            // Initializing rule and getting error file.
            final File file = new File(getClass().getResource(errorFile).getFile());

            // Instantiate checker.
            rule = (AbstractChecker) checker.newInstance();

            // Defining file in the rule instantiation.
            rule.setInputFile(file);
            // Defining id in the rule instantiation.
            rule.setId(checker.getName());

            // Running rule
            final List<CheckResult> list = rule.run();

            // We verify that there is an error.
            assertFalse(list.isEmpty());

            // We verify that there is the right number of errors
            final int nb_CheckResults = list.size();
            assertEquals(lines.length, nb_CheckResults);

            // We verify that the error detected is the right one. There is
            // only one case of error : a blank common (with no name) is
            // detected.
            final String fileName = list.get(0).getFile().getName();
            final String[] split = errorFile.split("/");
            assertEquals(split[split.length - 1], fileName);

            // We verify the values
            for (final CheckResult value : list) {
                final int index = list.indexOf(value);
                final String location = value.getLocation();
                assertEquals(locations[index], location);
                final int line = value.getLine();
                assertEquals(lines[index], line);
            }
        } catch (final JFlexException | IllegalAccessException | InstantiationException | IOException e) {
            fail(String.format("Analysis error (%s): %s", e.getClass().getSimpleName(), e.getMessage()));
        }
    }

    /**
     * This test verifies nothing is returned when there's no error.
     *
     * @param errorFile   path to an errored file
     * @param noErrorFile path to a correct file
     * @param lines       array of line raising errors
     * @param locations   array of function raising errors
     * @param checker     class of the checker to apply to previous files
     */
    @ParameterizedTest(name = "Test {index} should found no error in {1}")
    @MethodSource("data")
    default void testRunWithoutError(final String errorFile, final String noErrorFile, final int[] lines,
                                     final String[] locations, final Class<?> checker) {

        // Container for checker instantiation.
        AbstractChecker rule;

        try {
            // Initializing rule and getting error file.
            final File file = new File(getClass().getResource(noErrorFile).getFile());

            // Defining file in the rule instantiation.
            rule = (AbstractChecker) checker.newInstance();
            rule.setInputFile(file);

            // Running rule
            final List<CheckResult> list = rule.run();

            // We verify that there is an error.
            assertTrue(list.isEmpty());

        } catch (final JFlexException | IllegalAccessException | InstantiationException | IOException e) {
            fail(String.format("Analysis error (%s): %s", e.getClass().getSimpleName(), e.getMessage()));
        }
    }
}
