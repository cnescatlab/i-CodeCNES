/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

package fr.cnes.analysis.tools.fortran90.rules;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import fr.cnes.analysis.tools.fortran90.rules.COM.DATA.FloatCompare.TestCOMDATAFloatCompare;
import fr.cnes.analysis.tools.fortran90.rules.COM.DATA.Initialisation.TestCOMDATAInitialisation;
import fr.cnes.analysis.tools.fortran90.rules.COM.DATA.Invariant.TestCOMDATAInvariant;
import fr.cnes.analysis.tools.fortran90.rules.COM.DATA.LoopCondition.TestCOMDATALoopCondition;
import fr.cnes.analysis.tools.fortran90.rules.COM.DATA.NotUsed.TestCOMDATANotUsed;
import fr.cnes.analysis.tools.fortran90.rules.COM.DESIGN.Alloc.TestCOMDESIGNAlloc;
import fr.cnes.analysis.tools.fortran90.rules.COM.FLOW.Abort.TestCOMFLOWAbort;
import fr.cnes.analysis.tools.fortran90.rules.COM.FLOW.BooleanExpression.TestCOMFLOWBooleanExpression;
import fr.cnes.analysis.tools.fortran90.rules.COM.FLOW.CaseSwitch.TestCOMFLOWCaseSwitch;
import fr.cnes.analysis.tools.fortran90.rules.COM.FLOW.CheckCodeReturn.TestCOMFLOWCheckCodeReturn;
import fr.cnes.analysis.tools.fortran90.rules.COM.FLOW.CheckUser.TestCOMFLOWCheckUser;
import fr.cnes.analysis.tools.fortran90.rules.COM.FLOW.Exit.TestCOMFLOWExit;
import fr.cnes.analysis.tools.fortran90.rules.COM.FLOW.ExitLoop.TestCOMFLOWExitLoop;
import fr.cnes.analysis.tools.fortran90.rules.COM.FLOW.FileExistence.TestCOMFLOWFileExistence;
import fr.cnes.analysis.tools.fortran90.rules.COM.FLOW.FilePath.TestCOMFLOWFilePath;
import fr.cnes.analysis.tools.fortran90.rules.COM.FLOW.Recursion.TestCOMFLOWRecursion;
import fr.cnes.analysis.tools.fortran90.rules.COM.INST.BoolNegation.TestCOMINSTBoolNegation;
import fr.cnes.analysis.tools.fortran90.rules.COM.INST.Brace.TestCOMINSTBrace;
import fr.cnes.analysis.tools.fortran90.rules.COM.INST.CodeComment.TestCOMINSTCodeComment;
import fr.cnes.analysis.tools.fortran90.rules.COM.INST.GoTo.TestCOMINSTGoTo;
import fr.cnes.analysis.tools.fortran90.rules.COM.INST.Line.TestCOMINSTLine;
import fr.cnes.analysis.tools.fortran90.rules.COM.INST.LoopCondition.TestCOMINSTLoopCondition;
import fr.cnes.analysis.tools.fortran90.rules.COM.NAME.Homonymy.TestCOMNAMEHomonymy;
import fr.cnes.analysis.tools.fortran90.rules.COM.PRES.Indent.TestCOMPRESIndent;
import fr.cnes.analysis.tools.fortran90.rules.COM.PRES.LengthLine.TestCOMPRESLengthLine;
import fr.cnes.analysis.tools.fortran90.rules.COM.PROJECT.Header.TestCOMPROJECTHeader;
import fr.cnes.analysis.tools.fortran90.rules.COM.TYPE.Expression.TestCOMTYPEExpression;
import fr.cnes.analysis.tools.fortran90.rules.F90.BLOC.File.TestF90BLOCFile;
import fr.cnes.analysis.tools.fortran90.rules.F90.DATA.Array.TestF90DATAArray;
import fr.cnes.analysis.tools.fortran90.rules.F90.DATA.ArrayAccess.TestF90DATAArrayAccess;
import fr.cnes.analysis.tools.fortran90.rules.F90.DATA.Constant.TestF90DATAConstant;
import fr.cnes.analysis.tools.fortran90.rules.F90.DATA.ConstantFloat.TestF90DATAConstantFloat;
import fr.cnes.analysis.tools.fortran90.rules.F90.DATA.Float.TestF90DATAFloat;
import fr.cnes.analysis.tools.fortran90.rules.F90.DATA.Parameter.TestF90DATAParameter;
import fr.cnes.analysis.tools.fortran90.rules.F90.DESIGN.Free.TestF90DESIGNFree;
import fr.cnes.analysis.tools.fortran90.rules.F90.DESIGN.IO.TestF90DESIGNIO;
import fr.cnes.analysis.tools.fortran90.rules.F90.DESIGN.Include.TestF90DESIGNInclude;
import fr.cnes.analysis.tools.fortran90.rules.F90.DESIGN.Interface.TestF90DESIGNInterface;
import fr.cnes.analysis.tools.fortran90.rules.F90.DESIGN.Obsolete.TestF90DESIGNObsolete;
import fr.cnes.analysis.tools.fortran90.rules.F90.ERR.Allocate.TestF90ERRAllocate;
import fr.cnes.analysis.tools.fortran90.rules.F90.ERR.OpenRead.TestF90ERROpenRead;
import fr.cnes.analysis.tools.fortran90.rules.F90.INST.Associated.TestF90INSTAssociated;
import fr.cnes.analysis.tools.fortran90.rules.F90.INST.Entry.TestF90INSTEntry;
import fr.cnes.analysis.tools.fortran90.rules.F90.INST.Equivalence.TestF90INSTEquivalence;
import fr.cnes.analysis.tools.fortran90.rules.F90.INST.If.TestF90INSTIf;
import fr.cnes.analysis.tools.fortran90.rules.F90.INST.Intent.TestF90INSTIntent;
import fr.cnes.analysis.tools.fortran90.rules.F90.INST.Nullify.TestF90INSTNullify;
import fr.cnes.analysis.tools.fortran90.rules.F90.INST.Only.TestF90INSTOnly;
import fr.cnes.analysis.tools.fortran90.rules.F90.INST.Operator.TestF90INSTOperator;
import fr.cnes.analysis.tools.fortran90.rules.F90.INST.Pointer.TestF90INSTPointer;
import fr.cnes.analysis.tools.fortran90.rules.F90.NAME.GenericIntrinsic.TestF90NAMEGenericIntrinsic;
import fr.cnes.analysis.tools.fortran90.rules.F90.NAME.KeyWord.TestF90NAMEKeyWords;
import fr.cnes.analysis.tools.fortran90.rules.F90.PROTO.Overload.TestF90PROTOOverload;
import fr.cnes.analysis.tools.fortran90.rules.F90.REF.Array.TestF90REFArray;
import fr.cnes.analysis.tools.fortran90.rules.F90.REF.Interface.TestF90REFInterface;
import fr.cnes.analysis.tools.fortran90.rules.F90.REF.Label.TestF90REFLabel;
import fr.cnes.analysis.tools.fortran90.rules.F90.REF.Open.TestF90REFOpen;
import fr.cnes.analysis.tools.fortran90.rules.F90.REF.Variable.TestF90REFVariable;
import fr.cnes.analysis.tools.fortran90.rules.F90.TYPE.Derivate.TestF90TYPEDerivate;
import fr.cnes.analysis.tools.fortran90.rules.F90.TYPE.Integer.TestF90TYPEInteger;
import fr.cnes.analysis.tools.fortran90.rules.F90.TYPE.Real.TestF90TYPEReal;

@RunWith(Suite.class)
@SuiteClasses({TestCOMDESIGNAlloc.class,
        TestCOMDATAFloatCompare.class, TestCOMDATAInitialisation.class,
        TestCOMDATAInvariant.class, TestCOMDATALoopCondition.class,
        TestCOMDATANotUsed.class, TestCOMFLOWAbort.class,
        TestCOMFLOWBooleanExpression.class,
        TestCOMFLOWCaseSwitch.class, TestCOMFLOWCheckCodeReturn.class,
        TestCOMFLOWCheckUser.class,
        TestCOMFLOWExit.class, TestCOMFLOWExitLoop.class,
        TestCOMFLOWFileExistence.class, TestCOMFLOWFilePath.class,
        TestCOMFLOWRecursion.class, TestCOMINSTBoolNegation.class,
        TestCOMINSTBrace.class, TestCOMINSTCodeComment.class,
        TestCOMINSTGoTo.class, TestCOMINSTLine.class,
        TestCOMINSTLoopCondition.class, TestCOMNAMEHomonymy.class,
        TestCOMPROJECTHeader.class,
        TestCOMPRESIndent.class, TestCOMPRESLengthLine.class,
        TestCOMTYPEExpression.class, TestF90DESIGNInclude.class,
        TestF90DESIGNIO.class, TestF90DESIGNFree.class,
        TestF90DESIGNInterface.class,TestF90DESIGNFree.class,
        TestF90DESIGNObsolete.class,
        TestF90BLOCFile.class, TestF90DATAArray.class,
        TestF90DATAArrayAccess.class, 
        TestF90DATAConstant.class, TestF90DATAConstantFloat.class,
        TestF90DATAFloat.class,
        TestF90DATAParameter.class, TestF90ERRAllocate.class,
        TestF90ERROpenRead.class, TestF90INSTAssociated.class,
        TestF90INSTEntry.class, TestF90INSTEquivalence.class,
        TestF90INSTIf.class, TestF90INSTIntent.class,
        TestF90INSTNullify.class, TestF90INSTOnly.class,
        TestF90INSTOperator.class, TestF90INSTPointer.class,
        TestF90NAMEGenericIntrinsic.class,
        TestF90NAMEKeyWords.class, TestF90PROTOOverload.class,
        TestF90REFArray.class, TestF90REFInterface.class, TestF90DESIGNIO.class,
        TestF90REFLabel.class, TestF90REFOpen.class, TestF90REFVariable.class,
        TestF90TYPEDerivate.class, TestF90TYPEInteger.class,
        TestF90TYPEReal.class})
public class AllTests {

}
