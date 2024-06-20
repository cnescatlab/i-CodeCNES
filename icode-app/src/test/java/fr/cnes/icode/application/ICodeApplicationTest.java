/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.application;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * This class provides a main command line application to
 * use i-Code in a console without GUI.
 */
public class ICodeApplicationTest {

    /**
     * LogTester to capture log during tests.
     */
    private LogTester logTester;

    @BeforeEach
    public void attachLogTester() {
        logTester = LogTester.capture(ICodeApplication.LOGGER, new DisplayFormatter());
    }

    @AfterEach
    public void detachLogTester() {
        logTester.free();
    }

    @Test
    public void testSupportedLanguages() {

        ICodeApplication.main(new String[]{"-l"});

        final String icodeLog = logTester.log();

        assertFalse(icodeLog.contains("+ C++"));
        assertFalse(icodeLog.contains("+ C"));
        assertFalse(icodeLog.contains("+ Python"));
        assertTrue(icodeLog.contains("+ Fortran 77"));
        assertTrue(icodeLog.contains("+ Fortran 90"));

    }

    @Test
    public void testGetVersion() {

        final ICodeApplication application = new ICodeApplication();

        application.runICode(new String[]{"-v"});

        final String icodeLog = logTester.log();
        assertEquals("null version null\n", icodeLog);

    }

    @Test
    public void testNonRegressionOnHelp() {

        final CommandLineManager cli = new CommandLineManager();

        final boolean result = cli.parse(new String[]{"-h"});

        assertFalse(result);
        assertTrue(cli.hasOption("h"));

    }

    @Test
    public void testNonRegressionOnVersion() {

        final CommandLineManager cli = new CommandLineManager();

        final boolean result = cli.parse(new String[]{"-v"});

        assertFalse(result);
        assertTrue(cli.hasOption("v"));

    }

    @Test
    public void testNonRegressionOnExportersListing() {

        final CommandLineManager cli = new CommandLineManager();

        final boolean result = cli.parse(new String[]{"-e"});

        assertTrue(result);
        assertTrue(cli.hasOption("e"));

    }

    @Test
    public void testNonRegressionOnRulesListing() {

        final CommandLineManager cli = new CommandLineManager();

        final boolean result = cli.parse(new String[]{"-r"});

        assertTrue(result);
        assertTrue(cli.hasOption("r"));

    }

    @Test
    public void testNonRegressionOnUnknownOption() {

        final CommandLineManager cli = new CommandLineManager();

        final boolean result = cli.parse(new String[]{"-hibou"});

        assertFalse(result);
        assertFalse(cli.hasOption("hibou"));

        final String icodeLog = logTester.log();
        assertTrue(icodeLog.contains("Unrecognized option: -hibou\n"));

    }

}
