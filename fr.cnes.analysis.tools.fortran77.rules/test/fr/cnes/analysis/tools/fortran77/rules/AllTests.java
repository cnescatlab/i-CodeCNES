/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import fr.cnes.analysis.tools.fortran77.rules.COM.DATA.FloatCompare.TestCOMDATAFloatCompare;
import fr.cnes.analysis.tools.fortran77.rules.COM.DATA.Initialisation.TestCOMDATAInitialisation;
import fr.cnes.analysis.tools.fortran77.rules.COM.DATA.Invariant.TestCOMDATAInvariant;
import fr.cnes.analysis.tools.fortran77.rules.COM.DATA.LoopCondition.TestCOMDATALoopCondition;
import fr.cnes.analysis.tools.fortran77.rules.COM.DATA.NotUsed.TestCOMDATANotUsed;
import fr.cnes.analysis.tools.fortran77.rules.COM.DESIGN.ActiveWait.TestCOMDESIGNActiveWait;
import fr.cnes.analysis.tools.fortran77.rules.COM.DESIGN.Alloc.TestCOMDESIGNAlloc;
import fr.cnes.analysis.tools.fortran77.rules.COM.FLOW.Abort.TestCOMFLOWAbort;
import fr.cnes.analysis.tools.fortran77.rules.COM.FLOW.BooleanExpression.TestCOMFLOWBooleanExpression;
import fr.cnes.analysis.tools.fortran77.rules.COM.FLOW.CheckCodeReturn.TestCOMFLOWCheckCodeReturn;
import fr.cnes.analysis.tools.fortran77.rules.COM.FLOW.CheckUser.TestCOMFLOWCheckUser;
import fr.cnes.analysis.tools.fortran77.rules.COM.FLOW.Exit.TestCOMFLOWExit;
import fr.cnes.analysis.tools.fortran77.rules.COM.FLOW.ExitLoop.TestCOMFLOWExitLoop;
import fr.cnes.analysis.tools.fortran77.rules.COM.FLOW.FileExistence.TestCOMFLOWFileExistence;
import fr.cnes.analysis.tools.fortran77.rules.COM.FLOW.FilePath.TestCOMFLOWFilePath;
import fr.cnes.analysis.tools.fortran77.rules.COM.INST.BoolNegation.TestCOMINSTBoolNegation;
import fr.cnes.analysis.tools.fortran77.rules.COM.INST.Brace.TestCOMINSTBrace;
import fr.cnes.analysis.tools.fortran77.rules.COM.INST.CodeComment.TestCOMINSTCodeComment;
import fr.cnes.analysis.tools.fortran77.rules.COM.INST.GoTo.TestCOMINSTGoTo;
import fr.cnes.analysis.tools.fortran77.rules.COM.INST.LoopCondition.TestCOMINSTLoopCondition;
import fr.cnes.analysis.tools.fortran77.rules.COM.NAME.Homonymy.TestCOMNAMEHomonymy;
import fr.cnes.analysis.tools.fortran77.rules.COM.PRES.Indent.TestCOMPRESIndent;
import fr.cnes.analysis.tools.fortran77.rules.COM.PRES.LengthLine.TestCOMPRESLengthLine;
import fr.cnes.analysis.tools.fortran77.rules.COM.PROJECT.Header.TestCOMPROJECTHeader;
import fr.cnes.analysis.tools.fortran77.rules.COM.TYPE.Expression.TestCOMTYPEExpression;
import fr.cnes.analysis.tools.fortran77.rules.F77.BLOC.Common.TestF77BLOCCommon;
import fr.cnes.analysis.tools.fortran77.rules.F77.BLOC.Else.TestF77BLOCElse;
import fr.cnes.analysis.tools.fortran77.rules.F77.BLOC.Function.TestF77BLOCFunction;
import fr.cnes.analysis.tools.fortran77.rules.F77.BLOC.Loop.TestF77BLOCLoop;
import fr.cnes.analysis.tools.fortran77.rules.F77.DATA.Array.TestF77DATAArray;
import fr.cnes.analysis.tools.fortran77.rules.F77.DATA.Common.TestF77DATACommon;
import fr.cnes.analysis.tools.fortran77.rules.F77.DATA.Double.TestF77DATADouble;
import fr.cnes.analysis.tools.fortran77.rules.F77.DATA.IO.TestF77DATAIO;
import fr.cnes.analysis.tools.fortran77.rules.F77.DATA.Initialiasation.TestF77DATAInitialisation;
import fr.cnes.analysis.tools.fortran77.rules.F77.DATA.LoopDO.TestF77DATALoopDO;
import fr.cnes.analysis.tools.fortran77.rules.F77.DATA.Parameter.TestF77DATAParameter;
import fr.cnes.analysis.tools.fortran77.rules.F77.INST.Assign.TestF77INSTAssign;
import fr.cnes.analysis.tools.fortran77.rules.F77.INST.Dimension.TestF77INSTDimension;
import fr.cnes.analysis.tools.fortran77.rules.F77.INST.Equivalence.TestF77INSTEquivalence;
import fr.cnes.analysis.tools.fortran77.rules.F77.INST.Function.TestF77INSTFunction;
import fr.cnes.analysis.tools.fortran77.rules.F77.INST.If.TestF77INSTIf;
import fr.cnes.analysis.tools.fortran77.rules.F77.INST.Include.TestF77INSTInclude;
import fr.cnes.analysis.tools.fortran77.rules.F77.INST.Pause.TestF77INSTPause;
import fr.cnes.analysis.tools.fortran77.rules.F77.INST.Return.TestF77INSTReturn;
import fr.cnes.analysis.tools.fortran77.rules.F77.INST.Save.TestF77INSTSave;
import fr.cnes.analysis.tools.fortran77.rules.F77.MET.Line.TestF77METLine;
import fr.cnes.analysis.tools.fortran77.rules.F77.NAME.GenericIntrinsic.TestF77NAMEGenericIntrinsic;
import fr.cnes.analysis.tools.fortran77.rules.F77.NAME.Intrinsic.TestF77NAMEIntrinsic;
import fr.cnes.analysis.tools.fortran77.rules.F77.NAME.KeyWords.TestF77NAMEKeyWords;
import fr.cnes.analysis.tools.fortran77.rules.F77.NAME.Label.TestF77NAMELabel;
import fr.cnes.analysis.tools.fortran77.rules.F77.PROTO.Declaration.TestF77PROTODeclaration;
import fr.cnes.analysis.tools.fortran77.rules.F77.REF.IO.TestF77REFIO;
import fr.cnes.analysis.tools.fortran77.rules.F77.REF.Parameter.TestF77REFParameter;
import fr.cnes.analysis.tools.fortran77.rules.F77.TYPE.Basic.TestF77TYPEBasic;
import fr.cnes.analysis.tools.fortran77.rules.F77.TYPE.Hollerith.TestF77TYPEHollerith;

@RunWith(Suite.class)
@SuiteClasses({ TestCOMDESIGNAlloc.class, TestCOMDATAFloatCompare.class,
        TestCOMDATAInitialisation.class, TestCOMDATAInvariant.class, TestCOMDATALoopCondition.class,
        TestCOMDATANotUsed.class, TestCOMFLOWAbort.class, TestCOMDESIGNActiveWait.class,
        TestCOMFLOWBooleanExpression.class, TestCOMFLOWCheckCodeReturn.class,
        TestCOMFLOWCheckUser.class, TestCOMFLOWExit.class, TestCOMFLOWExitLoop.class,
        TestCOMFLOWFileExistence.class, TestCOMFLOWFilePath.class, TestCOMINSTBoolNegation.class,
        TestCOMINSTBrace.class, TestCOMINSTCodeComment.class, TestCOMINSTGoTo.class,
        TestCOMINSTLoopCondition.class, TestCOMNAMEHomonymy.class, TestCOMPROJECTHeader.class,
        TestCOMPRESIndent.class, TestCOMPRESLengthLine.class, TestCOMTYPEExpression.class,
        TestF77BLOCCommon.class, TestF77BLOCElse.class, TestF77BLOCFunction.class,
        TestF77BLOCLoop.class, TestF77DATAArray.class, TestF77DATACommon.class,
        TestF77DATADouble.class, TestF77DATAIO.class, TestF77DATAInitialisation.class,
        TestF77DATALoopDO.class, TestF77DATAParameter.class, TestF77INSTAssign.class,
        TestF77INSTDimension.class, TestF77INSTEquivalence.class, TestF77INSTFunction.class,
        TestF77INSTIf.class, TestF77INSTInclude.class, TestF77INSTPause.class,
        TestF77INSTReturn.class, TestF77INSTSave.class, TestF77METLine.class,
        TestF77NAMEGenericIntrinsic.class, TestF77NAMEIntrinsic.class, TestF77NAMEKeyWords.class,
        TestF77NAMELabel.class, TestF77PROTODeclaration.class, TestF77REFIO.class,
        TestF77REFParameter.class, TestF77TYPEBasic.class, TestF77TYPEHollerith.class, })
public class AllTests {

}
