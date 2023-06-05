/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/

package fr.cnes.icode.fortran77.rules;

import fr.cnes.icode.test.ICodeCheckerTester;

/**
 * This class aims to test all Fortran 77 rules. There are 2 functions in this class.
 * The first one verifies that an error in a file is detected whenever there is
 * one, the other verifies that nothing is detected when there's no error.
 * <p>
 * These functions test all rules with values provided by parametrized test.
 */
public class TestAllFortran77Rules implements ICodeCheckerTester {

    /**
     * This List represent the data set and contains:
     * - path to an errored file
     * - path to a correct file
     * - array of line raising errors
     * - array of function raising errors
     * - class of the checker to apply to previous files
     *
     * @return Array of test data.
     */
    public static Object[][] data() {
        return new Object[][]{
                {"/COM/DATA/FloatCompare/error.f", "/COM/DATA/FloatCompare/noError.f", new int[]{5, 8, 9}, new String[]{"MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM"}, COMDATAFloatCompare.class},
                {"/COM/DATA/Initialisation/error.f", "/COM/DATA/Initialisation/noError.f", new int[]{11, 15, 16, 8}, new String[]{"PROGRAM TEST", "PROGRAM TEST", "PROGRAM TEST", "PROGRAM TEST"}, COMDATAInitialisation.class},
                {"/COM/DATA/Invariant/error.f", "/COM/DATA/Invariant/noError.f", new int[]{4, 6, 12, 16}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, COMDATAInvariant.class},
                {"/COM/DATA/LoopCondition/error.f", "/COM/DATA/LoopCondition/noError.f", new int[]{4, 5, 6, 12, 17, 23, 29}, new String[]{"MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "FUNCTION TEST"}, COMDATALoopCondition.class},
                {"/COM/DATA/NotUsed/error.f", "/COM/DATA/NotUsed/noError.f", new int[]{7, 12, 17, 17, 17, 18, 18, 18, 18}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, COMDATANotUsed.class},
                {"/COM/DESIGN/ActiveWait/error.f", "/COM/DESIGN/ActiveWait/noError.f", new int[]{38, 46, 50}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, COMDESIGNActiveWait.class},
                {"/COM/DESIGN/Alloc/error.f", "/COM/DESIGN/Alloc/noError.f", new int[]{12, 17}, new String[]{"function f1", "subroutine s2"}, COMDESIGNAlloc.class},
                {"/COM/FLOW/Abort/error.f", "/COM/FLOW/Abort/noError.f", new int[]{11}, new String[]{"MAIN PROGRAM ESSAI"}, COMFLOWAbort.class},
                {"/COM/FLOW/BooleanExpression/error.f", "/COM/FLOW/BooleanExpression/noError.f", new int[]{11, 15}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI"}, COMFLOWBooleanExpression.class},
				{"/COM/FLOW/CheckArguments/error.f", "/COM/FLOW/CheckArguments/noError.f", new int[]{1}, new String[]{"SUBROUTINE C3BODY"}, COMFLOWCheckArguments.class},
                {"/COM/FLOW/CheckCodeReturn/error.f", "/COM/FLOW/CheckCodeReturn/noError.f", new int[]{7}, new String[]{"PROGRAM MAIN"}, COMFLOWCheckCodeReturn.class},
                {"/COM/FLOW/CheckUser/error.f", "/COM/FLOW/CheckUser/noError.f", new int[]{1}, new String[]{"PROGRAM MAIN"}, COMFLOWCheckUser.class},
                {"/COM/FLOW/Exit/error.f", "/COM/FLOW/Exit/noError.f", new int[]{12}, new String[]{"function f1"}, COMFLOWExit.class},
                {"/COM/FLOW/ExitLoop/error.f", "/COM/FLOW/ExitLoop/noError.f", new int[]{5, 9, 15, 17}, new String[]{"FUNCTION TEST", "FUNCTION TEST", "FUNCTION TEST", "FUNCTION TEST"}, COMFLOWExitLoop.class},
                {"/COM/FLOW/FileExistence/error.f", "/COM/FLOW/FileExistence/noError.f", new int[]{5, 7, 28}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, COMFLOWFileExistence.class},
                {"/COM/FLOW/FilePath/error.f", "/COM/FLOW/FilePath/noError.f", new int[]{12, 15, 29, 31}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, COMFLOWFilePath.class},
                {"/COM/INST/BoolNegation/error.f", "/COM/INST/BoolNegation/noError.f", new int[]{7, 9, 11, 13}, new String[]{"MAIN PROGRAM ESSAI", "MAIN PROGRAM ESSAI", "MAIN PROGRAM ESSAI", "MAIN PROGRAM ESSAI"}, COMINSTBoolNegation.class},
                {"/COM/INST/Brace/error.f", "/COM/INST/Brace/noError.f", new int[]{3, 5, 10, 11, 13}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, COMINSTBrace.class},
                {"/COM/INST/CodeComment/error.f", "/COM/INST/CodeComment/noError.f", new int[]{14, 17}, new String[]{"subroutine mpi_IO_e_us76", "subroutine mpi_IO_e_us76"}, COMINSTCodeComment.class},
                {"/COM/INST/GoTo/error.f", "/COM/INST/GoTo/noError.f", new int[]{5}, new String[]{"PROGRAM ESSAI"}, COMINSTGoTo.class},
                {"/COM/INST/LoopCondition/error.f", "/COM/INST/LoopCondition/noError.f", new int[]{5}, new String[]{"PROGRAM ESSAI"}, COMINSTLoopCondition.class},
				{"/COM/MET/ComplexitySimplified/error.f", "/COM/MET/ComplexitySimplified/noError.f", new int[]{1}, new String[]{"SUBROUTINE CALBED"}, COMMETComplexitySimplified.class},
				{"/COM/MET/LineOfCode/error.f", "/COM/MET/LineOfCode/noError.f", new int[]{170}, new String[]{"PROCEDURE ESSAI"}, COMMETLineOfCode.class},
				{"/COM/MET/RatioComment/error.f", "/COM/MET/RatioComment/noError.f", new int[]{15}, new String[]{"MAIN PROGRAM"}, COMMETRatioComment.class},
                {"/COM/NAME/Homonymy/error.f", "/COM/NAME/Homonymy/noError.f", new int[]{7, 25, 29}, new String[]{"function f1", "subroutine s1", "function f3"}, COMNAMEHomonymy.class},
				{"/COM/PRES/Data/error.f", "/COM/PRES/Data/noError.f", new int[]{6}, new String[]{"r"}, COMPRESData.class},
                {"/COM/PRES/FileLength/error.f", "/COM/PRES/FileLength/noError.f", new int[]{1052}, new String[]{"SUBROUTINE MY_SUB1"}, COMPRESFileLength.class},
                {"/COM/PRES/Indent/error.f", "/COM/PRES/Indent/noError.f", new int[]{6}, new String[]{"PROGRAM ESSAI"}, COMPRESIndent.class},
                {"/COM/PRES/LengthLine/error.f", "/COM/PRES/LengthLine/noError.f", new int[]{7, 9, 18}, new String[]{"MAIN PROGRAM ESSAI", "MAIN PROGRAM ESSAI", "MAIN PROGRAM ESSAI"}, COMPRESLengthLine.class},
                {"/COM/PROJECT/Header/error.f", "/COM/PROJECT/Header/noError.f", new int[]{0, 11}, new String[]{"No file header existing.", "FUNCTION F"}, COMPROJECTHeader.class},
                {"/COM/TYPE/Expression/error.f", "/COM/TYPE/Expression/noError.f", new int[]{20, 21, 22, 23}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, COMTYPEExpression.class},
                {"/F77/BLOC/Common/error.f", "/F77/BLOC/Common/noError.f", new int[]{6, 7, 26}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "SUBROUTINE MY_SUB1"}, F77BLOCCommon.class},
                {"/F77/BLOC/Else/error.f", "/F77/BLOC/Else/noError.f", new int[]{32, 35}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI"}, F77BLOCElse.class},
                {"/F77/BLOC/Function/error.f", "/F77/BLOC/Function/noError.f", new int[]{31}, new String[]{"PROGRAM ESSAI"}, F77BLOCFunction.class},
                {"/F77/BLOC/Loop/error.f", "/F77/BLOC/Loop/noError.f", new int[]{23}, new String[]{"PROGRAM ESSAI"}, F77BLOCLoop.class},
                {"/F77/DATA/Array/error.f", "/F77/DATA/Array/noError.f", new int[]{68, 70}, new String[]{"SUBROUTINE MOYENNE", "SUBROUTINE MOYENNE"}, F77DATAArray.class},
                {"/F77/DATA/Common/error.f", "/F77/DATA/Common/noError.f", new int[]{5, 25}, new String[]{"MAIN PROGRAM", "SUBROUTINE MY_SUB1"}, F77DATACommon.class},
                {"/F77/DATA/Double/error.f", "/F77/DATA/Double/noError.f", new int[]{17}, new String[]{"SUBROUTINE SUB1"}, F77DATADouble.class},
                {"/F77/DATA/Initialiasation/error.f", "/F77/DATA/Initialiasation/noError.f", new int[]{12, 13, 14, 15, 18}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, F77DATAInitialisation.class},
                {"/F77/DATA/IO/error.f", "/F77/DATA/IO/noError.f", new int[]{17, 18}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI"}, F77REFIO.class},
                {"/F77/DATA/LoopDO/error.f", "/F77/DATA/LoopDO/noError.f", new int[]{11}, new String[]{"PROGRAM ESSAI"}, F77DATALoopDO.class},
                {"/F77/DATA/Parameter/error.f", "/F77/DATA/Parameter/noError.f", new int[]{8}, new String[]{"PROGRAM ESSAI"}, F77DATAParameter.class},
                {"/F77/DESIGN/LogicUnit/error.f", "/F77/DESIGN/LogicUnit/noError.f", new int[]{21}, new String[]{"MODULE ESSAI2"}, F77DESIGNLogicUnit.class},
                {"/F77/FILE/Header/error.f", "/F77/FILE/Header/noError.f", new int[]{1}, new String[]{"MODULE"}, F77FILEHeader.class},
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
                {"/F77/PROTO/Declaration/error.f", "/F77/PROTO/Declaration/noError.f", new int[]{24, 29}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI"}, F77PROTODeclaration.class},
                {"/F77/REF/IO/error.f", "/F77/REF/IO/noError.f", new int[]{17, 18}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI"}, F77REFIO.class},
                {"/F77/REF/Open/error.f", "/F77/REF/Open/noError.f", new int[]{1}, new String[]{"MAIN PROGRAM"}, F77REFOpen.class},
                {"/F77/REF/Parameter/error.f", "/F77/REF/Parameter/noError.f", new int[]{12, 12, 12, 12}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, F77REFParameter.class},
                {"/F77/TYPE/Basic/error.f", "/F77/TYPE/Basic/noError.f", new int[]{3, 4, 6, 7, 9}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, F77TYPEBasic.class},
                {"/F77/TYPE/Hollerith/error.f", "/F77/TYPE/Hollerith/noError.f", new int[]{4, 4, 5, 8}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, F77TYPEHollerith.class}
        };
    }
}
