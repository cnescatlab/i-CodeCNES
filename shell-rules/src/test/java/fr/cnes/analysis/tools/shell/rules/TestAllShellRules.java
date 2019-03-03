/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;

/**
 * This class aims to test all Shell rules. There are 2 functions in this class.
 * The first one verifies that an error in a file is detected whenever there is
 * one, the other verifies that nothing is detected when there's no error.
 *
 * These functions test all rules with values provided by parametrized test.
 */
@RunWith(Parameterized.class)
public class TestAllShellRules {

	@Parameters(name = "TEST {index}: {4}")
	public static Iterable<Object[]> data() {
		return Arrays.asList(new Object[][] {
                {"/COM/DATA/Initialisation/error.sh", "/COM/DATA/Initialisation/noError.sh", new int[]{ 9, 15, 20, 25, 27, 32, 37 }, new String[]{ "MAIN PROGRAM", "function1", "MAIN PROGRAM", "function2", "function2", "function3", "MAIN PROGRAM" }, COMDATAInitialisation.class},
                {"/COM/DATA/Invariant/error.sh", "/COM/DATA/Invariant/noError.sh", new int[]{ 9, 14, 21, 27, 13, 8, 9, 30}, new String[]{ "MAIN PROGRAM", "MAIN PROGRAM", "fonction", "fonction2", "MAIN PROGRAM", "MAIN PROGRAM" , "MAIN PROGRAM", "MAIN PROGRAM" }, COMDATAInvariant.class},
                {"/COM/DATA/LoopCondition/error.sh", "/COM/DATA/LoopCondition/noError.sh", new int[]{ 27, 44 }, new String[]{ "testFunction", "MAIN PROGRAM" }, COMDATALoopCondition.class},
                {"/COM/DATA/NotUsed/error.sh", "/COM/DATA/NotUsed/noError.sh", new int[]{ 12 }, new String[]{ "MAIN PROGRAM" }, COMDATANotUsed.class},
                {"/COM/DESIGN/ActiveWait/error.sh", "/COM/DESIGN/ActiveWait/noError.sh", new int[]{ 9, 21, 25 }, new String[]{ "MAIN PROGRAM", "readNum", "MAIN PROGRAM" }, COMDESIGNActiveWait.class},
                {"/COM/FLOW/Abort/error.sh", "/COM/FLOW/Abort/noError.sh", new int[]{ 13, 14, 22, 25 }, new String[]{ "MAIN PROGRAM", "MAIN PROGRAM", "testFunction" , "MAIN PROGRAM"  }, COMFLOWAbort.class},
                {"/COM/FLOW/BooleanExpression/error.sh", "/COM/FLOW/BooleanExpression/noError.sh", new int[]{ 12, 17, 27, 32, 34, 42, 47 }, new String[]{ "test_func", "test_func", "test_func2", "test_func2", "test_func2", "test_func2", "test_func2" }, COMFLOWBooleanExpression.class},
                {"/COM/FLOW/CaseSwitch/error.sh", "/COM/FLOW/CaseSwitch/noError.sh", new int[]{ 17, 33, 37, 52 }, new String[]{ "MAIN PROGRAM", "caseFunction", "caseFunction", "MAIN PROGRAM" }, COMFLOWCaseSwitch.class},
                {"/COM/FLOW/Exit/error.sh", "/COM/FLOW/Exit/noError.sh", new int[]{ 14, 18, 21, 33, 37, 40 }, new String[]{ "getopts_internal", "getopts_internal", "getopts_internal", "getopts_external", "getopts_external", "getopts_external" }, COMFLOWExit.class},
                {"/COM/FLOW/ExitLoop/error.sh", "/COM/FLOW/ExitLoop/noError.sh", new int[]{ 22, 25, 31 }, new String[]{ "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM" }, COMFLOWExitLoop.class},
                {"/COM/FLOW/FileExistence/error.sh", "/COM/FLOW/FileExistence/noError.sh", new int[]{ 8, 9, 11, 13, 15, 17, 18, 18, 20, 24, 27, 29 }, new String[]{ "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "my-function", "MAIN PROGRAM", "MAIN PROGRAM" }, COMFLOWFileExistence.class},
                {"/COM/FLOW/FilePath/error.sh", "/COM/FLOW/FilePath/noError.sh", new int[]{ 7, 8, 9, 11, 13, 15, 17, 18, 18, 20, 23, 24, 27, 29, 29}, new String[]{ "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "my-function", "my-function", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM"}, COMFLOWFilePath.class},
                {"/COM/FLOW/Recursion/error.sh", "/COM/FLOW/Recursion/noError.sh", new int[]{ 16, 40, 48, 49 }, new String[]{ "recursive_directe", "recursive_indirecte2" , "recursive_indirecte3" , "recursive_indirecte3" }, COMFLOWRecursion.class},
                {"/COM/INST/BoolNegation/error.sh", "/COM/INST/BoolNegation/noError.sh", new int[]{ 9, 13, 17, 22, 27, 33, 40, 46 }, new String[]{ "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "test1", "test", "MAIN PROGRAM" }, COMINSTBoolNegation.class},
                {"/COM/INST/Brace/error.sh", "/COM/INST/Brace/noError.sh", new int[]{ 14 }, new String[]{ "MAIN PROGRAM" }, COMINSTBrace.class},
                {"/COM/INST/CodeComment/error.sh", "/COM/INST/CodeComment/noError.sh", new int[]{ 18, 21, 22, 27, 28, 31, 35, 36, 37 }, new String[]{ "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM" }, COMINSTCodeComment.class},
                {"/COM/INST/Line/error.sh", "/COM/INST/Line/noError.sh", new int[]{ 10, 11, 24, 25 }, new String[]{ "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM" }, COMINSTLine.class},
                {"/COM/INST/LoopCondition/error.sh", "/COM/INST/LoopCondition/noError.sh", new int[]{ 9 }, new String[]{ "MAIN PROGRAM" }, COMINSTLoopCondition.class},
                {"/COM/NAME/Homonymy/error.sh", "/COM/NAME/Homonymy/noError.sh", new int[]{ 19, 27, 32, 41, 43, 51, 54, 60, 67, 69, 78, 79 }, new String[]{ "fonction_globale", "MAIN PROGRAM", "MAIN PROGRAM", "test2", "test2", "test3", "test3", "test4", "test4", "test4", "test5", "test5"}, COMNAMEHomonymy.class},
                {"/COM/PRES/Header/error.sh", "/COM/PRES/Header/noError.sh", new int[]{ 12, 25 }, new String[]{ "ma_fonction_affine", "affiche_resultat" }, COMPRESHeader.class},
                {"/COM/PRES/Indent/error.sh", "/COM/PRES/Indent/noError.sh", new int[]{ 18, 26, 45, 49, 59, 60, 64, 70, 78, 79, 92, 93, 94 }, new String[]{ "ma_fonction_affine", "ma_fonction_affine", "affiche_resultat", "affiche_resultat", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "ma_fonction_affine2", "ma_fonction_affine2", "ma_fonction_affine2" }, COMPRESIndent.class},
                {"/COM/PRES/LengthLine/error.sh", "/COM/PRES/LengthLine/noError.sh", new int[]{ 9, 18, 48, 55, 57, 75}, new String[]{ "MAIN PROGRAM", "ma_fonction_affine", "affiche_resultat", "test_length", "affiche_resultat", "MAIN PROGRAM"}, COMPRESLengthLine.class},
                {"/COM/TYPE/Expression/error.sh", "/COM/TYPE/Expression/noError.sh", new int[]{ 9, 13 }, new String[]{ "MAIN PROGRAM", "MAIN PROGRAM" }, COMTYPEExpression.class},
                {"/SH/DATA/IFS/error.sh", "/SH/DATA/IFS/noError.sh", new int[]{ 18 }, new String[]{ "MAIN PROGRAM" }, SHDATAIFS.class},
                {"/SH/DATA/Integer/error.sh", "/SH/DATA/Integer/noError.sh", new int[]{ 12 }, new String[]{ "MAIN PROGRAM" }, SHDATAInteger.class},
                {"/SH/DESIGN/Bash/error.sh", "/SH/DESIGN/Bash/noError.sh", new int[]{ 1 }, new String[]{ "MAIN PROGRAM" }, SHDESIGNBash.class},
                {"/SH/DESIGN/Options/error.sh", "/SH/DESIGN/Options/noError.sh", new int[]{ 20 }, new String[]{ "getopts_internal" }, SHDESIGNOptions.class},
                {"/SH/ERR/Help/error.sh", "/SH/ERR/Help/noError.sh", new int[]{ 40 }, new String[]{ "getopts_internal" }, SHERRHelp.class},
                {"/SH/ERR/Help/error2.sh", "/SH/ERR/Help/noError.sh", new int[]{ 43 }, new String[]{ "getopts_internal" }, SHERRHelp.class},
                {"/SH/ERR/NoPipe/error.sh", "/SH/ERR/NoPipe/noError.sh", new int[]{ 18, 18, 18, 18, 22, 22, 22, 22, 25, 29, 36 }, new String[]{ "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM" }, SHERRNoPipe.class},
                {"/SH/ERR/String/error.sh", "/SH/ERR/String/noError.sh", new int[]{ 17 }, new String[]{ "MAIN PROGRAM" }, SHERRString.class},
                {"/SH/FLOW/CheckArguments/error.sh", "/SH/FLOW/CheckArguments/noError.sh", new int[]{ 6 }, new String[]{ "ma_fonction_affine" }, SHFLOWCheckArguments.class},
                {"/SH/FLOW/CheckCodeReturn/error.sh", "/SH/FLOW/CheckCodeReturn/noError.sh", new int[]{ 29, 32, 34, 35, 40, 44, 46, 48}, new String[]{ "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "nettoyer_repertoire", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM" }, SHFLOWCheckCodeReturn.class},
                {"/SH/FLOW/CheckUser/error.sh", "/SH/FLOW/CheckUser/noError.sh", new int[]{ 6 }, new String[]{ "MAIN PROGRAM" }, SHFLOWCheckUser.class},
                {"/SH/INST/Basename/error.sh", "/SH/INST/Basename/noError.sh", new int[]{ 15 }, new String[]{ "MAIN PROGRAM" }, SHINSTBasename.class},
                {"/SH/INST/Continue/error.sh", "/SH/INST/Continue/noError.sh", new int[]{ 18 }, new String[]{ "MAIN PROGRAM" }, SHINSTContinue.class},
                {"/SH/INST/Find/error.sh", "/SH/INST/Find/noError.sh", new int[]{ 22, 24, 41 }, new String[]{ "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM" }, SHINSTFind.class},
                {"/SH/INST/GetOpts/error.sh", "/SH/INST/GetOpts/noError.sh", new int[]{ 22, 23, 26, 27, 28, 29, 0 }, new String[]{ "direct_calls", "direct_calls", "direct_calls", "direct_calls", "direct_calls", "direct_calls", "MAIN PROGRAM" }, SHINSTGetOpts.class},
                {"/SH/INST/Interpreter/error.sh", "/SH/INST/Interpreter/noError.sh", new int[]{ 1 }, new String[]{ "MAIN PROGRAM" }, SHINSTInterpreter.class},
                {"/SH/INST/Interpreter/error.sh", "/SH/INST/Interpreter/noError2.sh", new int[]{ 1 }, new String[]{ "MAIN PROGRAM" }, SHINSTInterpreter.class},
                {"/SH/INST/Keywords/error.sh", "/SH/INST/Keywords/noError.sh", new int[]{ 8, 10, 14, 15, 16, 16 }, new String[]{ "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM" }, SHINSTKeywords.class},
                {"/SH/INST/Logical/error.sh", "/SH/INST/Logical/noError.sh", new int[]{ 16 }, new String[]{ "MAIN PROGRAM" }, SHINSTLogical.class},
                {"/SH/INST/POSIX/error.sh", "/SH/INST/POSIX/noError.sh", new int[]{ 31, 38 }, new String[]{ "MAIN PROGRAM", "MAIN PROGRAM" }, SHINSTPOSIX.class},
                {"/SH/INST/SetShift/error.sh", "/SH/INST/SetShift/noError.sh", new int[]{ 16, 27, 27, 28, 41, 42, 46 }, new String[]{ "MAIN PROGRAM", "getopts_internal", "getopts_internal", "getopts_internal", "getopts_internal", "getopts_internal", "getopts_internal" }, SHINSTSetShift.class},
                {"/SH/INST/Variables/error.sh", "/SH/INST/Variables/noError.sh", new int[]{ 17, 21, 23, 23, 23 }, new String[]{ "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM" }, SHINSTVariables.class},
                {"/SH/IO/Redirect/error.sh", "/SH/IO/Redirect/noError.sh", new int[]{ 13, 14, 17, 21, 24 }, new String[]{ "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "my-function", "MAIN PROGRAM" }, SHIORedirect.class},
                {"/SH/MET/LimitAWK/error.sh", "/SH/MET/LimitAWK/noError.sh", new int[]{ 9 }, new String[]{ "MAIN PROGRAM" }, SHMETLimitAWK.class},
                {"/SH/MET/LimitSed/error.sh", "/SH/MET/LimitSed/noError.sh", new int[]{ 12, 20 }, new String[]{ "MAIN PROGRAM", "MAIN PROGRAM" }, SHMETLimitSed.class},
                {"/SH/MET/PipeLine/error.sh", "/SH/MET/PipeLine/noError.sh", new int[]{ 8, 16, 19 }, new String[]{ "MAIN PROGRAM", "test1", "test" }, SHMETPipeLine.class},
                {"/SH/REF/Export/error.sh", "/SH/REF/Export/noError.sh", new int[]{ 18, 27, 30 }, new String[]{ "MAIN PROGRAM", "testFunction","MAIN PROGRAM" }, SHREFExport.class},
                {"/SH/SYNC/Signals/error.sh", "/SH/SYNC/Signals/noError.sh", new int[]{ 57 }, new String[]{ "MAIN PROGRAM" }, SHSYNCSignals.class}
		});
	}

	@Parameter
	public String errorFile;
	@Parameter(1)
	public String noErrorFile;
	@Parameter(2)
	public int[] lines;
	@Parameter(3)
	public String[] locations;
	@Parameter(4)
    public Class<?> checker;
	public AbstractChecker rule;

	/**
	 * This test verifies that an error can be detected.
	 */
	@Test
	public void testRunWithError() {

		try {
			// Initializing rule and getting error file.
			final File file = new File(getClass().getResource(errorFile).getFile());

			// Instantiate checker.
            this.rule = (AbstractChecker) checker.newInstance();

			// Defining file in the rule instantiation.
			this.rule.setInputFile(file);
            // Defining id in the rule instantiation.
            this.rule.setId(checker.getName());

			// Running rule
			final List<CheckResult> list = this.rule.run();

			// We verify that there is an error.
			assertFalse("No error found.", list.isEmpty());

			// We verify that there is the right number of errors

			final int nb_CheckResults = list.size();
			assertEquals("Wrong number of CheckResults : ", lines.length, nb_CheckResults);

			// We verify that the error detected is the right one. There is
			// only one case of error : a blank common (with no name) is
			// detected.
			final String fileName = list.get(0).getFile().getName();
			final String[] split = errorFile.split("/");
			assertEquals("Wrong file name : ", split[split.length-1], fileName);

			// We verify the values
			for (final CheckResult value : list) {
				final int index = list.indexOf(value);
				final String location = value.getLocation();
				assertTrue("CheckResult " + Integer.toString(index) + " has wrong location : " + location + " should contain "
						+ locations[index], location.contains(locations[index]));
				final int line = value.getLine();
				assertEquals("CheckResult " + Integer.toString(index) + " is in wrong line : ", lines[index], line);
			}
		} catch (final JFlexException | IllegalAccessException | InstantiationException | IOException e) {
            fail("Analysis error (" + e.getClass().getName() + ")");
        }
    }

	/**
	 * This test verifies nothing is returned when there's no error.
	 */
	@Test
	public void testRunWithoutError() {
		try {
			// Initializing rule and getting error file.
			final File file = new File(getClass().getResource(noErrorFile).getFile());

			// Defining file in the rule instantiation.
            this.rule = (AbstractChecker) checker.newInstance();
			this.rule.setInputFile(file);

			// Running rule
			final List<CheckResult> list = this.rule.run();

			// We verify that there is an error.

			assertTrue("Error(s) are detected: " + TestUtils.getCheckResults(list), list.isEmpty());

		} catch (final JFlexException | IllegalAccessException | InstantiationException | IOException e) {
            fail("Analysis error (" + e.getClass().getName() + ")");
        }
	}
}
