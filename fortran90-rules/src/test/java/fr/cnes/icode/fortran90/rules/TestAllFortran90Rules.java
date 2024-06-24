/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/

package fr.cnes.icode.fortran90.rules;

import fr.cnes.icode.test.ICodeCheckerTester;

/**
 * This class aims to test all Fortran 90 rules. There are 2 functions in this class.
 * The first one verifies that an error in a file is detected whenever there is
 * one, the other verifies that nothing is detected when there's no error.
 * <p>
 * These functions test all rules with values provided by parametrized test.
 */
public class TestAllFortran90Rules implements ICodeCheckerTester {

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
                {"/COM/DATA/FloatCompare/error.f90", "/COM/DATA/FloatCompare/noError.f90", new int[]{5, 8, 9}, new String[]{"MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM"}, COMDATAFloatCompare.class},
                {"/COM/DATA/Initialisation/error.f90", "/COM/DATA/Initialisation/noError.f90", new int[]{11, 15, 16, 8}, new String[]{"PROGRAM TEST", "PROGRAM TEST", "PROGRAM TEST", "PROGRAM TEST"}, COMDATAInitialisation.class},
                {"/COM/DATA/Invariant/error.f", "/COM/DATA/Invariant/noError.f", new int[]{4, 6, 12, 16}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, COMDATAInvariant.class},
                {"/COM/DATA/LoopCondition/error.f90", "/COM/DATA/LoopCondition/noError.f90", new int[]{4, 5, 6, 12, 17, 23, 29}, new String[]{"MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "FUNCTION TEST"}, COMDATALoopCondition.class},
                {"/COM/DATA/NotUsed/error.f", "/COM/DATA/NotUsed/noError.f", new int[]{7, 12}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI"}, COMDATANotUsed.class},
                {"/COM/DESIGN/ActiveWait/error.f", "/COM/DESIGN/ActiveWait/noError.f", new int[]{12}, new String[]{"PROGRAM ESSAI"}, COMDESIGNActiveWait.class},
                {"/COM/DESIGN/Alloc/error.f", "/COM/DESIGN/Alloc/noError.f", new int[]{12, 17}, new String[]{"procedure p1", "subroutine s2"}, COMDESIGNAlloc.class},
                {"/COM/FLOW/Abort/error.f", "/COM/FLOW/Abort/noError.f", new int[]{15}, new String[]{"PROGRAM ESSAI"}, COMFLOWAbort.class},
                {"/COM/FLOW/BooleanExpression/error.f", "/COM/FLOW/BooleanExpression/noError.f", new int[]{11}, new String[]{"PROGRAM ESSAI"}, COMFLOWBooleanExpression.class},
                {"/COM/FLOW/CaseSwitch/error.f", "/COM/FLOW/CaseSwitch/noError.f", new int[]{3}, new String[]{"PROGRAM ESSAI"}, COMFLOWCaseSwitch.class},
				{"/COM/FLOW/CheckArguments/error.f", "/COM/FLOW/CheckArguments/noError.f", new int[]{1}, new String[]{"SUBROUTINE C3BODY"}, COMFLOWCheckArguments.class},
                {"/COM/FLOW/CheckCodeReturn/error.f90", "/COM/FLOW/CheckCodeReturn/noError.f90", new int[]{7}, new String[]{"PROGRAM MAIN"}, COMFLOWCheckCodeReturn.class},
                {"/COM/FLOW/CheckUser/error.f90", "/COM/FLOW/CheckUser/noError.f90", new int[]{1}, new String[]{"PROGRAM MAIN"}, COMFLOWCheckUser.class},
                {"/COM/FLOW/Exit/error.f", "/COM/FLOW/Exit/noError.f", new int[]{12}, new String[]{"function f1"}, COMFLOWExit.class},
                {"/COM/FLOW/ExitLoop/error.f90", "/COM/FLOW/ExitLoop/noError.f90", new int[]{2, 6, 14, 16}, new String[]{"MAIN PROGRAM", "MAIN PROGRAM", "FUNCTION TEST", "FUNCTION TEST"}, COMFLOWExitLoop.class},
                {"/COM/FLOW/FileExistence/error.f", "/COM/FLOW/FileExistence/noError.f", new int[]{5, 7, 28}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, COMFLOWFileExistence.class},
                {"/COM/FLOW/FilePath/error.f", "/COM/FLOW/FilePath/noError.f", new int[]{12, 15}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI"}, COMFLOWFilePath.class},
                {"/COM/FLOW/Recursion/error.f90", "/COM/FLOW/Recursion/noError.f90", new int[]{1}, new String[]{"MAIN PROGRAM"}, COMFLOWRecursion.class},
                {"/COM/INST/BoolNegation/error.f", "/COM/INST/BoolNegation/noError.f", new int[]{7, 9, 11, 13}, new String[]{"MAIN PROGRAM ESSAI", "MAIN PROGRAM ESSAI", "MAIN PROGRAM ESSAI", "MAIN PROGRAM ESSAI"}, COMINSTBoolNegation.class},
                {"/COM/INST/Brace/error.f", "/COM/INST/Brace/noError.f", new int[]{5, 6, 7, 26, 27, 27}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, COMINSTBrace.class},
                {"/COM/INST/CodeComment/error.f", "/COM/INST/CodeComment/noError.f", new int[]{140, 142}, new String[]{"subroutine ostc_lecdon2", "subroutine ostc_lecdon2"}, COMINSTCodeComment.class},
                {"/COM/INST/GoTo/error.f", "/COM/INST/GoTo/noError.f", new int[]{5}, new String[]{"PROGRAM ESSAI"}, COMINSTGoTo.class},
                {"/COM/INST/Line/error.f", "/COM/INST/Line/noError.f", new int[]{2}, new String[]{"PROGRAM ESSAI"}, COMINSTLine.class},
                {"/COM/INST/LoopCondition/error.f", "/COM/INST/LoopCondition/noError.f", new int[]{5, 11}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI"}, COMINSTLoopCondition.class},
				{"/COM/MET/ComplexitySimplified/error.f", "/COM/MET/ComplexitySimplified/noError.f", new int[]{1}, new String[]{"SUBROUTINE CALBED"}, COMMETComplexitySimplified.class},
				{"/COM/MET/LineOfCode/error.f90", "/COM/MET/LineOfCode/noError.f90", new int[]{170}, new String[]{"PROCEDURE ESSAI"}, COMMETLineOfCode.class},
				{"/COM/MET/RatioComment/error.f", "/COM/MET/RatioComment/noError.f", new int[]{15}, new String[]{"MAIN PROGRAM"}, COMMETRatioComment.class},
                {"/COM/NAME/Homonymy/error.f", "/COM/NAME/Homonymy/noError.f", new int[]{24, 42, 46}, new String[]{"function f1", "subroutine s2", "function f3"}, COMNAMEHomonymy.class},
				{"/COM/PRES/Data/error.f90", "/COM/PRES/Data/noError.f90", new int[]{3}, new String[]{"daMatTr"}, COMPRESData.class},
                {"/COM/PRES/Indent/error.f", "/COM/PRES/Indent/noError.f", new int[]{5, 6}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI"}, COMPRESIndent.class},
                {"/COM/PRES/LengthLine/error.f", "/COM/PRES/LengthLine/noError.f", new int[]{7, 9, 18}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, COMPRESLengthLine.class},
                {"/COM/PRES/FileLength/error.f", "/COM/PRES/FileLength/noError.f", new int[]{1502}, new String[]{"subroutine ncdf_getvar_2D_FourByteInt"}, COMPRESFileLength.class},
                {"/COM/PROJECT/Header/error.f90", "/COM/PROJECT/Header/noError.f90", new int[]{0, 11}, new String[]{"No file header existing.", "FUNCTION F"}, COMPROJECTHeader.class},
                {"/COM/TYPE/Expression/error.f", "/COM/TYPE/Expression/noError.f", new int[]{20, 21, 23, 48}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "SUBROUTINE GS_calcul_ecart_echt"}, COMTYPEExpression.class},
                {"/F90/BLOC/File/error.f", "/F90/BLOC/File/noError.f", new int[]{20}, new String[]{"PROGRAM ESSAI"}, F90BLOCFile.class},
                {"/F90/DATA/Array/error.f", "/F90/DATA/Array/noError.f", new int[]{3, 21}, new String[]{"Subroutine somme", "Subroutine somme2"}, F90DATAArray.class},
                {"/F90/DATA/ArrayAccess/error.f", "/F90/DATA/ArrayAccess/noError.f", new int[]{10}, new String[]{"program ESSAI"}, F90DATAArrayAccess.class},
                {"/F90/DATA/Constant/error.f", "/F90/DATA/Constant/noError.f", new int[]{64, 86}, new String[]{"FUNCTION FUNC_1", "FUNCTION FUNC_2"}, F90DATAConstant.class},
                {"/F90/DATA/ConstantFloat/error.f", "/F90/DATA/ConstantFloat/noError.f", new int[]{27}, new String[]{"PROGRAM ESSAI"}, F90DATAConstantFloat.class},
                {"/F90/DATA/Declaration/error.f", "/F90/DATA/Declaration/noError.f", new int[]{38, 47, 48}, new String[]{"subroutine changer_coordonnees", "subroutine changer_coordonnees", "subroutine changer_coordonnees"}, F90DATADeclaration.class},
                {"/F90/DATA/Float/error.f90", "/F90/DATA/Float/noError.f90", new int[]{20, 20}, new String[]{"subroutine format_etoile", "subroutine format_etoile"}, F90DATAFloat.class},
                {"/F90/DATA/Parameter/error.f90", "/F90/DATA/Parameter/noError.f90", new int[]{13, 14, 15, 16, 17, 18, 56, 57, 58, 59, 60, 61}, new String[]{"MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, F90DATAParameter.class},
                {"/F90/DESIGN/Free/error.f", "/F90/DESIGN/Free/noError.f", new int[]{103}, new String[]{"SUBROUTINE Desallouer_Tableau"}, F90DESIGNFree.class},
                {"/F90/DESIGN/Include/error.f", "/F90/DESIGN/Include/noError.f", new int[]{1}, new String[]{"MAIN PROGRAM"}, F90DESIGNInclude.class},
                {"/F90/DESIGN/Interface/error.f90", "/F90/DESIGN/Interface/noError.f90", new int[]{50}, new String[]{"module PLUSIEURS_USAGES"}, F90DESIGNInterface.class},
                {"/F90/DESIGN/IO/error.f90", "/F90/DESIGN/IO/noError.f90", new int[]{12, 14, 16, 20}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, F90DESIGNIO.class},
                {"/F90/DESIGN/LogicUnit/error.f90", "/F90/DESIGN/LogicUnit/noError.f90", new int[]{21}, new String[]{"MODULE ESSAI2"}, F90DESIGNLogicUnit.class},
                {"/F90/DESIGN/Obsolete/error.f", "/F90/DESIGN/Obsolete/noError.f", new int[]{8, 78, 79, 92, 115, 123, 124, 176, 183, 191}, new String[]{"function RETOURNE", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "function RETOURNE", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, F90DESIGNObsolete.class},
                {"/F90/ERR/Allocate/error.f", "/F90/ERR/Allocate/noError.f", new int[]{26, 30, 59, 65}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, F90ERRAllocate.class},
                {"/F90/ERR/OpenRead/error.f", "/F90/ERR/OpenRead/noError.f", new int[]{27, 32, 39, 42}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, F90ERROpenRead.class},
                {"/F90/FILE/Header/error.f90", "/F90/FILE/Header/noError.f90", new int[]{1}, new String[]{"MODULE"}, F90FILEHeader.class},
                {"/F90/INST/Associated/error.f90", "/F90/INST/Associated/noError.f90", new int[]{44, 52}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI"}, F90INSTAssociated.class},
                {"/F90/INST/Entry/error.f", "/F90/INST/Entry/noError.f", new int[]{10, 16}, new String[]{"SUBROUTINE FIN", "SUBROUTINE FIN"}, F90INSTEntry.class},
                {"/F90/INST/Equivalence/error.f90", "/F90/INST/Equivalence/noError.f90", new int[]{10}, new String[]{"program ESSAI"}, F90INSTEquivalence.class},
                {"/F90/INST/If/error.f90", "/F90/INST/If/noError.f90", new int[]{34}, new String[]{"program ESSAI"}, F90INSTIf.class},
                {"/F90/INST/Intent/error.f90", "/F90/INST/Intent/noError.f90", new int[]{38, 39, 40}, new String[]{"subroutine Mouv", "subroutine Mouv", "subroutine Mouv"}, F90INSTIntent.class},
                {"/F90/INST/Nullify/error.f", "/F90/INST/Nullify/noError.f", new int[]{5}, new String[]{"program ESSAI"}, F90INSTNullify.class},
                {"/F90/INST/Only/error.f", "/F90/INST/Only/noError.f", new int[]{73}, new String[]{"program ESSAI"}, F90INSTOnly.class},
                {"/F90/INST/Operator/error.f", "/F90/INST/Operator/noError.f", new int[]{34, 49, 61, 70, 75, 84, 88, 100, 100, 107, 117}, new String[]{"program ESSAI", "program ESSAI", "program ESSAI", "program ESSAI", "program ESSAI", "program ESSAI", "program ESSAI", "program ESSAI", "program ESSAI", "program ESSAI", "program ESSAI"}, F90INSTOperator.class},
                {"/F90/INST/Pointer/error.f", "/F90/INST/Pointer/noError.f", new int[]{32, 33}, new String[]{"program ESSAI", "program ESSAI"}, F90INSTPointer.class},
                {"/F90/NAME/GenericIntrinsic/error.f90", "/F90/NAME/GenericIntrinsic/noError.f90", new int[]{30}, new String[]{"PROGRAM ESSAI"}, F90NAMEGenericIntrinsic.class},
                {"/F90/NAME/KeyWord/error.f90", "/F90/NAME/KeyWord/noError.f90", new int[]{41, 44, 50, 54, 56, 87}, new String[]{"PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "FUNCTION sin"}, F90NAMEKeyWords.class},
                {"/F90/PROTO/Overload/error.f90", "/F90/PROTO/Overload/noError.f90", new int[]{82}, new String[]{"function ajouter_vecteur_2"}, F90PROTOOverload.class},
                {"/F90/REF/Array/error.f90", "/F90/REF/Array/noError.f90", new int[]{23}, new String[]{"PROGRAM ESSAI"}, F90REFArray.class},
                {"/F90/REF/Interface/error.f90", "/F90/REF/Interface/noError.f90", new int[]{22}, new String[]{"subroutine pas_1"}, F90REFInterface.class},
                {"/F90/REF/Label/error.f90", "/F90/REF/Label/noError.f90", new int[]{15, 43, 61, 84, 145}, new String[]{"module ma_precision", "block data bloc_mesure", "subroutine celsius_to_fahrenheit", "function mon_getuid", "program essai"}, F90REFLabel.class},
                {"/F90/REF/Open/error.f", "/F90/REF/Open/noError.f", new int[]{28}, new String[]{"PROGRAM ESSAI"}, F90REFOpen.class},
                {"/F90/REF/Variable/error.f90", "/F90/REF/Variable/noError.f90", new int[]{19, 19}, new String[]{"subroutine incr", "subroutine incr"}, F90REFVariable.class},
                {"/F90/TYPE/Derivate/error.f90", "/F90/TYPE/Derivate/noError.f90", new int[]{11, 34}, new String[]{"program PERSONNEL", "subroutine AFFICHE"}, F90TYPEDerivate.class},
                {"/F90/TYPE/Integer/error.f90", "/F90/TYPE/Integer/noError.f90", new int[]{5, 13}, new String[]{"MODULE ma_precision", "MODULE ma_precision"}, F90TYPEInteger.class},
                {"/F90/TYPE/Real/error.f90", "/F90/TYPE/Real/noError.f90", new int[]{13, 14, 15, 16, 17, 18, 55, 56, 57, 58, 59, 60}, new String[]{"MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "MAIN PROGRAM", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI", "PROGRAM ESSAI"}, F90TYPEReal.class}
        };
    }
}
