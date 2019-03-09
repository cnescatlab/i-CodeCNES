/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

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
 * This class aims to test all Fortran 77 rules. There are 2 functions in this class.
 * The first one verifies that an error in a file is detected whenever there is
 * one, the other verifies that nothing is detected when there's no error.
 *
 * These functions test all rules with values provided by parametrized test.
 */
@RunWith(Parameterized.class)
public class TestAllFortran77Rules {

    @Parameters(name = "TEST {index}: {4}")
    public static Iterable<Object[]> data() {
        return Arrays.asList(new Object[][]{
                {"/COM/DATA/FloatCompare/error.f", "/COM/DATA/FloatCompare/noError.f", new int[]{5, 8, 9}, new String[]{"MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM"}, COMDATAFloatCompare.class},
                {"/COM/DATA/Initialisation/error.f", "/COM/DATA/Initialisation/noError.f", new int[]{11, 15, 16, 8}, new String[]{"PROGRAM TEST", "PROGRAM TEST", "PROGRAM TEST", "PROGRAM TEST"}, COMDATAInitialisation.class},
                {"/COM/DATA/Invariant/error.f", "/COM/DATA/Invariant/noError.f", new int[]{4, 6, 12, 16}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, COMDATAInvariant.class},
                {"/COM/DATA/LoopCondition/error.f", "/COM/DATA/LoopCondition/noError.f", new int[]{4, 5, 6, 12, 17, 23, 29}, new String[]{"MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "FUNCTION TEST"}, COMDATALoopCondition.class},
                {"/COM/DATA/NotUsed/error.f", "/COM/DATA/NotUsed/noError.f", new int[]{7, 12, 17, 17, 17, 18, 18, 18, 18}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, COMDATANotUsed.class},
                {"/COM/DESIGN/ActiveWait/error.f", "/COM/DESIGN/ActiveWait/noError.f", new int[]{38, 46, 50}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, COMDESIGNActiveWait.class},
                {"/COM/DESIGN/Alloc/error.f", "/COM/DESIGN/Alloc/noError.f", new int[]{12, 17}, new String[]{"function f1", "subroutine s2"}, COMDESIGNAlloc.class},
                {"/COM/FLOW/Abort/error.f", "/COM/FLOW/Abort/noError.f", new int[]{11}, new String[]{"PROGRAM ESSAI"}, COMFLOWAbort.class},
                {"/COM/FLOW/BooleanExpression/error.f", "/COM/FLOW/BooleanExpression/noError.f", new int[]{11, 15}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI"}, COMFLOWBooleanExpression.class},
                {"/COM/FLOW/CheckCodeReturn/error.f", "/COM/FLOW/CheckCodeReturn/noError.f", new int[]{7}, new String[]{"PROGRAM MAIN"}, COMFLOWCheckCodeReturn.class},
                {"/COM/FLOW/CheckUser/error.f", "/COM/FLOW/CheckUser/noError.f", new int[]{1}, new String[]{"PROGRAM MAIN"}, COMFLOWCheckUser.class},
                {"/COM/FLOW/Exit/error.f", "/COM/FLOW/Exit/noError.f", new int[]{12}, new String[]{"function f1"}, COMFLOWExit.class},
                {"/COM/FLOW/ExitLoop/error.f", "/COM/FLOW/ExitLoop/noError.f", new int[]{5, 9, 15, 17}, new String[]{"FUNCTION TEST", "FUNCTION TEST", "FUNCTION TEST", "FUNCTION TEST"}, COMFLOWExitLoop.class},
                {"/COM/FLOW/FileExistence/error.f", "/COM/FLOW/FileExistence/noError.f", new int[]{5, 7, 28}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, COMFLOWFileExistence.class},
                {"/COM/FLOW/FilePath/error.f", "/COM/FLOW/FilePath/noError.f", new int[]{12, 15, 29, 31}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, COMFLOWFilePath.class},
                {"/COM/INST/BoolNegation/error.f", "/COM/INST/BoolNegation/noError.f", new int[]{7, 9, 11, 13}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, COMINSTBoolNegation.class},
                {"/COM/INST/Brace/error.f", "/COM/INST/Brace/noError.f", new int[]{3, 5, 10, 11, 13}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, COMINSTBrace.class},
                {"/COM/INST/CodeComment/error.f", "/COM/INST/CodeComment/noError.f", new int[]{14, 17}, new String[]{"subroutine mpi_IO_e_us76", "subroutine mpi_IO_e_us76"}, COMINSTCodeComment.class},
                {"/COM/INST/GoTo/error.f", "/COM/INST/GoTo/noError.f", new int[]{5}, new String[]{"PROGRAM ESSAI"}, COMINSTGoTo.class},
                {"/COM/INST/LoopCondition/error.f", "/COM/INST/LoopCondition/noError.f", new int[]{5}, new String[]{"PROGRAM ESSAI"}, COMINSTLoopCondition.class},
                {"/COM/NAME/Homonymy/error.f", "/COM/NAME/Homonymy/noError.f", new int[]{7, 25, 29}, new String[]{"function f1", "subroutine s1", "function f3"}, COMNAMEHomonymy.class},
                {"/COM/PRES/Indent/error.f", "/COM/PRES/Indent/noError.f", new int[]{6}, new String[]{"PROGRAM ESSAI"}, COMPRESIndent.class},
                {"/COM/PRES/LengthLine/error.f", "/COM/PRES/LengthLine/noError.f", new int[]{7, 9, 18}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, COMPRESLengthLine.class},
                {"/COM/PROJECT/Header/error.f", "/COM/PROJECT/Header/noError.f", new int[]{0, 11}, new String[]{"No file header existing", "FUNCTION F"}, COMPROJECTHeader.class},
                {"/COM/TYPE/Expression/error.f", "/COM/TYPE/Expression/noError.f", new int[]{20, 21, 22, 23}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, COMTYPEExpression.class},
                {"/F77/BLOC/Common/error.f", "/F77/BLOC/Common/noError.f", new int[]{6, 7, 26}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "SUBROUTINE MY_SUB1"}, F77BLOCCommon.class},
                {"/F77/BLOC/Else/error.f", "/F77/BLOC/Else/noError.f", new int[]{32, 35}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI"}, F77BLOCElse.class},
                {"/F77/BLOC/Function/error.f", "/F77/BLOC/Function/noError.f", new int[]{31}, new String[]{"PROGRAM ESSAI"}, F77BLOCFunction.class},
                {"/F77/BLOC/Loop/error.f", "/F77/BLOC/Loop/noError.f", new int[]{23}, new String[]{"PROGRAM ESSAI"}, F77BLOCLoop.class},
                {"/F77/DATA/Array/error.f", "/F77/DATA/Array/noError.f", new int[]{68, 70}, new String[]{"SUBROUTINE MOYENNE", "SUBROUTINE MOYENNE"}, F77DATAArray.class},
                {"/F77/DATA/Common/error.f", "/F77/DATA/Common/noError.f", new int[]{5, 25}, new String[]{"MAIN PROGRAM", "SUBROUTINE MY_SUB1"}, F77DATACommon.class},
                {"/F77/DATA/Double/error.f", "/F77/DATA/Double/noError.f", new int[]{17}, new String[]{"SUBROUTINE"}, F77DATADouble.class},
                {"/F77/DATA/Initialiasation/error.f", "/F77/DATA/Initialiasation/noError.f", new int[]{12, 13, 14, 15, 18}, new String[]{"PROGRAM", "PROGRAM", "PROGRAM", "PROGRAM", "PROGRAM"}, F77DATAInitialisation.class},
                {"/F77/DATA/IO/error.f", "/F77/DATA/IO/noError.f", new int[]{17, 18}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI"}, F77REFIO.class},
                {"/F77/DATA/LoopDO/error.f", "/F77/DATA/LoopDO/noError.f", new int[]{11}, new String[]{"PROGRAM ESSAI"}, F77DATALoopDO.class},
                {"/F77/DATA/Parameter/error.f", "/F77/DATA/Parameter/noError.f", new int[]{8}, new String[]{"PROGRAM ESSAI"}, F77DATAParameter.class},
                {"/F77/INST/Assign/error.f", "/F77/INST/Assign/noError.f", new int[]{16, 29, 31, 33, 35}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, F77INSTAssign.class},
                {"/F77/INST/Dimension/error.f", "/F77/INST/Dimension/noError.f", new int[]{8}, new String[]{"PROGRAM ESSAI"}, F77INSTDimension.class},
                {"/F77/INST/Equivalence/error.f", "/F77/INST/Equivalence/noError.f", new int[]{5}, new String[]{"PROGRAM ESSAI"}, F77INSTEquivalence.class},
                {"/F77/INST/Function/error.f", "/F77/INST/Function/noError.f", new int[]{7}, new String[]{"FUNCTION"}, F77INSTFunction.class},
                {"/F77/INST/If/error.f", "/F77/INST/If/noError.f", new int[]{18}, new String[]{"PROGRAM ESSAI"}, F77INSTIf.class},
                {"/F77/INST/Include/error.f", "/F77/INST/Include/noError.f", new int[]{16}, new String[]{"PROGRAM ESSAI"}, F77INSTInclude.class},
                {"/F77/INST/Pause/error.f", "/F77/INST/Pause/noError.f", new int[]{38, 51}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI"}, F77INSTPause.class},
                {"/F77/INST/Return/error.f", "/F77/INST/Return/noError.f", new int[]{27}, new String[]{"SUBROUTINE carre_cube"}, F77INSTReturn.class},
                {"/F77/INST/Save/error.f", "/F77/INST/Save/noError.f", new int[]{31}, new String[]{"PROGRAM ESSAI"}, F77INSTSave.class},
                {"/F77/MET/Line/error.f", "/F77/MET/Line/noError.f", new int[]{18}, new String[]{"PROGRAM F77_Pr_LongLigne"}, F77METLine.class},
                {"/F77/NAME/GenericIntrinsic/error.f", "/F77/NAME/GenericIntrinsic/noError.f", new int[]{27}, new String[]{"PROGRAM ESSAI"}, F77NAMEGenericIntrinsic.class},
                {"/F77/NAME/Intrinsic/error.f", "/F77/NAME/Intrinsic/noError.f", new int[]{1}, new String[]{"FUNCTION"}, F77NAMEIntrinsic.class},
                {"/F77/NAME/KeyWords/error.f", "/F77/NAME/KeyWords/noError.f", new int[]{13, 14, 15, 16, 17}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, F77NAMEKeyWords.class},
                {"/F77/NAME/Label/error.f", "/F77/NAME/Label/noError.f", new int[]{46, 49, 52, 61}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, F77NAMELabel.class},
                {"/F77/PROTO/Declaration/error.f", "/F77/PROTO/Declaration/noError.f", new int[]{24, 29}, new String[]{"PROGRAM", "PROGRAM"}, F77PROTODeclaration.class},
                {"/F77/REF/IO/error.f", "/F77/REF/IO/noError.f", new int[]{17, 18}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI"}, F77REFIO.class},
                {"/F77/REF/Open/error.f", "/F77/REF/Open/noError.f", new int[]{1}, new String[]{"MAIN PROGRAM"}, F77REFOpen.class},
                {"/F77/REF/Parameter/error.f", "/F77/REF/Parameter/noError.f", new int[]{12, 12, 12, 12}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, F77REFParameter.class},
                {"/F77/TYPE/Basic/error.f", "/F77/TYPE/Basic/noError.f", new int[]{3, 4, 6, 7, 9}, new String[]{"PROGRAM", "PROGRAM", "PROGRAM", "PROGRAM", "PROGRAM"}, F77TYPEBasic.class},
                {"/F77/TYPE/Hollerith/error.f", "/F77/TYPE/Hollerith/noError.f", new int[]{4, 4, 5, 8}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, F77TYPEHollerith.class}
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
            assertEquals("Wrong file name : ", split[split.length - 1], fileName);

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
            fail(String.format("Analysis error (%s): %s", e.getClass().getSimpleName(), e.getMessage()));
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
            fail(String.format("Analysis error (%s): %s", e.getClass().getSimpleName(), e.getMessage()));
        }
    }
}
