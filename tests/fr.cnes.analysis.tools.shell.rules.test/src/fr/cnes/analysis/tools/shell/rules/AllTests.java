/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import fr.cnes.analysis.tools.shell.rules.COM.DATA.Initialisation.TestCOMDATAInitialisation;
import fr.cnes.analysis.tools.shell.rules.COM.DATA.Invariant.TestCOMDATAInvariant;
import fr.cnes.analysis.tools.shell.rules.COM.DATA.LoopCondition.TestCOMDATALoopCondition;
import fr.cnes.analysis.tools.shell.rules.COM.DATA.NotUsed.TestCOMDATANotUsed;
import fr.cnes.analysis.tools.shell.rules.COM.DESIGN.ActiveWait.TestCOMDESIGNActiveWait;
import fr.cnes.analysis.tools.shell.rules.COM.FLOW.Abort.TestCOMFLOWAbort;
import fr.cnes.analysis.tools.shell.rules.COM.FLOW.BooleanExpression.TestCOMFLOWBooleanExpression;
import fr.cnes.analysis.tools.shell.rules.COM.FLOW.CaseSwitch.TestCOMFLOWCaseSwitch;
import fr.cnes.analysis.tools.shell.rules.COM.FLOW.Exit.TestCOMFLOWExit;
import fr.cnes.analysis.tools.shell.rules.COM.FLOW.ExitLoop.TestCOMFLOWExitLoop;
import fr.cnes.analysis.tools.shell.rules.COM.FLOW.FileExistence.TestCOMFLOWFileExistence;
import fr.cnes.analysis.tools.shell.rules.COM.FLOW.FilePath.TestCOMFLOWFilePath;
import fr.cnes.analysis.tools.shell.rules.COM.FLOW.Recursion.TestCOMFLOWRecursion;
import fr.cnes.analysis.tools.shell.rules.COM.INST.BoolNegation.TestCOMINSTBoolNegation;
import fr.cnes.analysis.tools.shell.rules.COM.INST.Brace.TestCOMINSTBrace;
import fr.cnes.analysis.tools.shell.rules.COM.INST.CodeComment.TestCOMINSTCodeComment;
import fr.cnes.analysis.tools.shell.rules.COM.INST.Line.TestCOMINSTLine;
import fr.cnes.analysis.tools.shell.rules.COM.INST.LoopCondition.TestCOMINSTLoopCondition;
import fr.cnes.analysis.tools.shell.rules.COM.NAME.Homonymy.TestCOMNAMEHomonymy;
import fr.cnes.analysis.tools.shell.rules.COM.PRES.Header.TestCOMPRESHeader;
import fr.cnes.analysis.tools.shell.rules.COM.PRES.Indent.TestCOMPRESIndent;
import fr.cnes.analysis.tools.shell.rules.COM.PRES.LengthLine.TestCOMPRESLengthLine;
import fr.cnes.analysis.tools.shell.rules.SH.DATA.IFS.TestSHDATAIFS;
import fr.cnes.analysis.tools.shell.rules.SH.DATA.Integer.TestSHDATAInteger;
import fr.cnes.analysis.tools.shell.rules.SH.DESIGN.Bash.TestSHDESIGNBash;
import fr.cnes.analysis.tools.shell.rules.SH.DESIGN.Options.TestSHDESIGNOptions;
import fr.cnes.analysis.tools.shell.rules.SH.ERR.Help.TestSHERRHelp;
import fr.cnes.analysis.tools.shell.rules.SH.ERR.NoPipe.TestSHERRNoPipe;
import fr.cnes.analysis.tools.shell.rules.SH.ERR.String.TestSHERRString;
import fr.cnes.analysis.tools.shell.rules.SH.FLOW.CheckArguments.TestSHFLOWCheckArguments;
import fr.cnes.analysis.tools.shell.rules.SH.FLOW.CheckCodeReturn.TestSHFLOWCheckCodeReturn;
import fr.cnes.analysis.tools.shell.rules.SH.FLOW.CheckUser.TestSHFLOWCheckUser;
import fr.cnes.analysis.tools.shell.rules.SH.INST.Basename.TestSHINSTBasename;
import fr.cnes.analysis.tools.shell.rules.SH.INST.Continue.TestSHINSTContinue;
import fr.cnes.analysis.tools.shell.rules.SH.INST.Find.TestSHINSTFind;
import fr.cnes.analysis.tools.shell.rules.SH.INST.GetOpts.TestSHINSTGetOpts;
import fr.cnes.analysis.tools.shell.rules.SH.INST.Interpreter.TestSHINSTInterpreter;
import fr.cnes.analysis.tools.shell.rules.SH.INST.Logical.TestSHINSTLogical;
import fr.cnes.analysis.tools.shell.rules.SH.INST.POSIX.TestSHINSTPOSIX;
import fr.cnes.analysis.tools.shell.rules.SH.INST.SetShift.TestSHINSTSetShift;
import fr.cnes.analysis.tools.shell.rules.SH.INST.Variables.TestSHINSTVariables;
import fr.cnes.analysis.tools.shell.rules.SH.IO.Redirect.TestSHIORedirect;
import fr.cnes.analysis.tools.shell.rules.SH.MET.LimitAWK.TestSHMETLimitAWK;
import fr.cnes.analysis.tools.shell.rules.SH.MET.LimitSed.TestSHMETLimitSed;
import fr.cnes.analysis.tools.shell.rules.SH.MET.PipeLine.TestSHMETPipeLine;
import fr.cnes.analysis.tools.shell.rules.SH.REF.Export.TestSHREFExport;
import fr.cnes.analysis.tools.shell.rules.SH.SYNC.Signals.TestSHSYNCSignals;

@RunWith(Suite.class)
@SuiteClasses({ TestCOMDATAInitialisation.class, TestCOMDATAInvariant.class, TestCOMDATALoopCondition.class,
		TestCOMDATANotUsed.class, TestCOMDESIGNActiveWait.class, TestCOMFLOWAbort.class,
		TestCOMFLOWBooleanExpression.class, TestCOMFLOWCaseSwitch.class, TestCOMFLOWExit.class,
		TestCOMFLOWExitLoop.class, TestCOMFLOWFileExistence.class, TestCOMFLOWFilePath.class,
		TestCOMFLOWRecursion.class, TestCOMINSTBoolNegation.class, TestCOMINSTBrace.class, TestCOMINSTCodeComment.class,
		TestCOMINSTLine.class, TestCOMINSTLoopCondition.class, TestCOMNAMEHomonymy.class, TestCOMPRESHeader.class,
		TestCOMPRESIndent.class, TestCOMPRESLengthLine.class, TestSHDATAIFS.class, TestSHDATAInteger.class,
		TestSHDESIGNBash.class, TestSHERRHelp.class, TestSHERRNoPipe.class, TestSHERRString.class, TestSHDATAIFS.class,
		TestSHDESIGNOptions.class, TestSHFLOWCheckArguments.class, TestSHFLOWCheckCodeReturn.class,
		TestSHFLOWCheckUser.class, TestSHINSTBasename.class, TestSHINSTContinue.class, TestSHINSTGetOpts.class,
		TestSHINSTFind.class, TestSHINSTInterpreter.class, TestSHINSTLogical.class, TestSHINSTPOSIX.class,
		TestSHINSTSetShift.class, TestSHINSTVariables.class, TestSHIORedirect.class, TestSHMETLimitAWK.class,
		TestSHMETLimitSed.class, TestSHMETPipeLine.class, TestSHREFExport.class, TestSHSYNCSignals.class, })
public class AllTests {

}
