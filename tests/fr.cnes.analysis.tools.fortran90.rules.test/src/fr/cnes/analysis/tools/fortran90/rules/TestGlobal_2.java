/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/

package fr.cnes.analysis.tools.fortran90.rules;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.junit.Test;

import fr.cnes.analysis.tools.analyzer.RuleAnalysisJob;
import fr.cnes.analysis.tools.analyzer.datas.Violation;

public class TestGlobal_2 {
	/** This list contains all the violations when the analyse is executed **/
	public static List<Violation> list = new LinkedList<Violation>();
	List<File> listFiles = new LinkedList<File>();

	/**********************/
	/** PARAMS TO DEFINE **/
	/**********************/

	/** Folder where the function find files to execute the analyze **/
	final String resources = "/resources/f90_2";
	/** Extension of the files to be analyzed **/
	final String extension = "f90";
	/** Id to execute the analysis **/
	final String ruleExtensionId = "fr.cnes.analysis.tools.fortran90.analyzer.rule";

	/**
	 * Function test to validate global test. This function do - get all files
	 * in /resources folder - run analysis in this files - export results to
	 * globalTest.txt file - compare file with the last one
	 * 
	 * NOTE: to run this test in jUnit: Run As > jUnit PlugIn in test
	 */
	@Test
	public void runGlobalTest() {
		try {
			/** File where to save the result of the execution **/
			File fileResult = new File("resources/globalTest_2.txt");
			// fileResult.deleteOnExit();
			/**
			 * File to compare with fileResult. This file is the verified
			 * version of the execution
			 **/
			File fileResultValidated = new File("resources/globalTestValidated_2.txt");

			/** Get resources file **/
			String resourcesPath = System.getProperty("user.dir") + resources;
			File folder = new File(resourcesPath);
			/** Save files into a list **/
			getFilesIntoFolder(folder);

			/** Create the analysis job **/
			final RuleAnalysisJob analysis = new RuleAnalysisJob(ruleExtensionId, listFiles);

			/** Add change listener to check when the job is done **/
			analysis.addJobChangeListener(new JobChangeAdapter() {
				@Override
				public void done(final IJobChangeEvent event) {
					/** Running rule **/
					TestGlobal_2.list = analysis.getViolations();
				}
			});

			/** Set job priority and run analysis **/
			analysis.setUser(true);
			analysis.setPriority(Job.DECORATE);
			analysis.schedule();
			analysis.join();

			/** Export values into file **/
			createExportFile(fileResult);

			/** Compare new test with last one **/
			assertTrue(compareFiles(fileResult, fileResultValidated));

		} catch (InterruptedException e) {
			fail("Erreur d'analyse (InterruptedException)");
		}

	}

	private void getFilesIntoFolder(File folder) {
		if (folder.isDirectory()) {
			File[] listOfFiles = folder.listFiles();
			/** For each file, check the extension and save IPath into List **/
			for (File file : listOfFiles) {
				if (file.isDirectory())
					getFilesIntoFolder(file);
				else {
					int i = file.getAbsolutePath().lastIndexOf(".");
					if (file.getAbsolutePath().substring(i + 1).toLowerCase().equals(extension)) {
						
						listFiles.add(file);
					}
				}
			}
		}

	}

	/**
	 * Function to create the error file. The file has the following: RuleName
	 * FileName NumError
	 */
	private void createExportFile(File fileResult) {
		try {
			/** Create result file **/
			BufferedWriter output = new BufferedWriter(new FileWriter(fileResult));

			/** If the list is bigger than one **/
			if (list.size() > 1) {
				String rule = list.get(0).getRuleName();
				String file = new Path(list.get(0).getFile().getAbsolutePath()).lastSegment();
				int errors = 1;
				/** Iterate over the elements **/
				for (int i = 1; i < list.size(); i++) {
					Violation violation = list.get(i);
					/** If the is more errors in the same rule **/
					if (violation.getRuleName().equals(rule)) {
						/**
						 * If there is more errors in the same file -> increase
						 * error
						 **/
						if (new Path(violation.getFile().getAbsolutePath()).lastSegment().equals(file)) {
							errors++;
						}
						/** If the filename has change -> print error **/
						else {
							output.write(rule + " " + file + " " + errors + "\n");
							file = new Path(violation.getFile().getAbsolutePath()).lastSegment();
							errors = 1;
						}
					}
					/** If rule has change -> print error **/
					else {
						output.write(rule + " " + file + " " + errors + "\n");
						rule = violation.getRuleName();
						file = new Path(violation.getFile().getAbsolutePath()).lastSegment();
						errors = 1;
					}
				}
			}
			/** Only one error -> print directly **/
			else if (list.size() > 0) {
				output.write(list.get(0).getRuleName() + " " + new Path(list.get(0).getFile().getAbsolutePath()).lastSegment() + " 1\n");
			}
			/** After run for all files: close file writer **/
			output.close();
		} catch (IOException e) {
			fail("Erreur d'analyse (IOException)");
		}
	}

	/**
	 * Comparison between files
	 * 
	 * @return equals indicate the equality of the files
	 */
	private boolean compareFiles(File fileResult, File fileResultValidated) {
		boolean equals = true;
		try {
			/** Create reader for both files **/
			FileReader fR1 = new FileReader(fileResult);
			FileReader fR2 = new FileReader(fileResultValidated);
			BufferedReader reader1 = new BufferedReader(fR1);
			BufferedReader reader2 = new BufferedReader(fR2);

			/** String for compare **/
			String line1 = null;
			String line2 = null;

			/** Loop over the lines to find differences **/
			while (((line1 = reader1.readLine()) != null) && ((line2 = reader2.readLine()) != null)) {
				if (!line1.equalsIgnoreCase(line2)) {
					equals = false;
					break;
				}
			}
			reader1.close();
			reader2.close();

		} catch (FileNotFoundException e) {
			fail("Erreur d'analyse (FileNotFoundException)");
		} catch (IOException e) {
			fail("Erreur d'analyse (IOException)");
		}
		/** Return value **/
		return equals;
	}

	// @Test
	// public void runGlobalTest() {
	//
	// try {
	// /** Create result file**/
	// File fileResult = new File("resources/globalTest.txt");
	// BufferedWriter output = new BufferedWriter(new FileWriter(fileResult));
	//
	// /** Iterate over the files **/
	// for(String fileStr : files) {
	// file = new Path( this.getClass().getClassLoader()
	// .getResource(fileStr).getPath());
	// /** Write file name **/
	// output.write( fileStr + "\n");
	//
	// // COM.DATA.ALLOC
	// rule = new COMDATAAlloc();
	// executeRule();
	// output.write("- COM.DATA.Alloc:\t\t" + list.size() + "\n");
	//
	// // COM.DATA.FLOATCOMPARE
	// rule = new COMDATAFloatCompare();
	// executeRule();
	// output.write("- COM.DATA.FloatCompare:\t\t" + list.size() + "\n");
	//
	// // COM.DATA.INITIALISATION
	// rule = new COMDATAInitialisation();
	// executeRule();
	// output.write("- COM.DATA.Initialisation:\t\t" + list.size() + "\n");
	//
	// // COM.DATA.Invariant
	// rule = new COMDATAInvariant();
	// executeRule();
	// output.write("- COM.DATA.Invariant:\t\t" + list.size() + "\n");
	//
	// // COM.DATA.LoopCondition
	// rule = new COMDATALoopCondition();
	// executeRule();
	// output.write("- COM.DATA.LoopCondition:\t\t" + list.size() + "\n");
	//
	// // COM.DATA.NotUsed
	// rule = new COMDATANotUsed();
	// executeRule();
	// output.write("- COM.DATA.NotUsed:\t\t" + list.size() + "\n");
	//
	// // COM.FLOW.Abort
	// rule = new COMFLOWAbort();
	// executeRule();
	// output.write("- COM.FLOW.Abort:\t\t" + list.size() + "\n");
	//
	// // COM.FLOW.ActiveWait
	// rule = new COMFLOWActiveWait();
	// executeRule();
	// output.write("- COM.FLOW.ActiveWait:\t\t" + list.size() + "\n");
	//
	// // COM.FLOW.BooleanExpression
	// rule = new COMFLOWBooleanExpression();
	// executeRule();
	// output.write("- COM.FLOW.BooleanExpression:\t\t" + list.size() + "\n");
	//
	// // COM.FLOW.CheckCoedReturn
	// rule = new COMFLOWCheckCodeReturn();
	// executeRule();
	// output.write("- COM.FLOW.CheckCodeReturn:\t\t" + list.size() + "\n");
	//
	// // COM.FLOW.Exit
	// rule = new COMFLOWExit();
	// executeRule();
	// output.write("- COM.FLOW.Exit:\t\t" + list.size() + "\n");
	//
	// // COM.FLOW.ExitLoop
	// rule = new COMFLOWExitLoop();
	// executeRule();
	// output.write("- COM.FLOW.ExitLoop:\t\t" + list.size() + "\n");
	//
	// // COM.FLOW.FileExistence
	// rule = new COMFLOWFileExistence();
	// executeRule();
	// output.write("- COM.FLOW.FileExistence:\t\t" + list.size() + "\n");
	//
	// // COM.FLOW.FilePath
	// rule = new COMFLOWFilePath();
	// executeRule();
	// output.write("- COM.FLOW.FilePath:\t\t" + list.size() + "\n");
	//
	// // COM.FLOW.Recursion
	// rule = new COMFLOWRecursion();
	// executeRule();
	// output.write("- COM.FLOW.Recursion:\t\t" + list.size() + "\n");
	//
	// // COM.INST.BoolNegation
	// rule = new COMINSTBoolNegation();
	// executeRule();
	// output.write("- COM.INST.BoolNegation:\t\t" + list.size() + "\n");
	//
	// // COM.INST.Brace
	// rule = new COMINSTBrace();
	// executeRule();
	// output.write("- COM.INST.Brace:\t\t" + list.size() + "\n");
	//
	// // COM.INST.CodeComment
	// rule = new COMINSTCodeComment();
	// executeRule();
	// output.write("- COM.INST.CodeComment:\t\t" + list.size() + "\n");
	//
	// // COM.INST.GoTo
	// rule = new COMINSTGoTo();
	// executeRule();
	// output.write("- COM.INST.GoTo:\t\t" + list.size() + "\n");
	//
	// // COM.INST.LoopCondition
	// rule = new COMINSTLoopCondition();
	// executeRule();
	// output.write("- COM.INST.LoopCondition:\t\t" + list.size() + "\n");
	//
	// // COM.NAME.Homonomy
	// rule = new COMNAMEHomonymy();
	// executeRule();
	// output.write("- COM.NAME.Homonomy:\t\t" + list.size() + "\n");
	//
	// // COM.PRES.DATA
	// rule = new COMPRESData();
	// executeRule();
	// output.write("- COM.PRES.Data:\t\t" + list.size() + "\n");
	//
	// // COM.PRES.Header
	// rule = new COMPRESHeader();
	// executeRule();
	// output.write("- COM.PRES.Header:\t\t" + list.size() + "\n");
	//
	// // COM.PRES.Indent
	// rule = new COMPRESIndent();
	// executeRule();
	// output.write("- COM.PRES.Indent:\t\t" + list.size() + "\n");
	//
	// // COM.PRES.lengthLine
	// rule = new COMPRESLengthLine();
	// executeRule();
	// output.write("- COM.PRES.LengthLine:\t\t" + list.size() + "\n");
	//
	// // COM.TYPE.Expression
	// rule = new COMTYPEExpression();
	// executeRule();
	// output.write("- COM.TYPE.Expression:\t\t" + list.size() + "\n");
	//
	// // F77.BLOC.COMMON
	// rule = new F77BLOCCommon();
	// executeRule();
	// output.write("- F77.BLOC.Common:\t\t" + list.size() + "\n");
	//
	// // F77.BLOC.ELSE
	// rule = new F77BLOCElse();
	// executeRule();
	// output.write("- F77.BLOC.Else:\t\t" + list.size() + "\n");
	//
	// // F77.BLOC.Function
	// rule = new F77BLOCFunction();
	// executeRule();
	// output.write("- F77.BLOC.Function:\t\t" + list.size() + "\n");
	//
	// // F77.BLOC.Loop
	// rule = new F77BLOCLoop();
	// executeRule();
	// output.write("- F77.BLOC.Loop:\t\t" + list.size() + "\n");
	//
	// // F77.DATA.Array
	// rule = new F77DATAArray();
	// executeRule();
	// output.write("- F77.DATA.Array:\t\t" + list.size() + "\n");
	//
	// // F77.DATA.Common
	// rule = new F77DATACommon();
	// executeRule();
	// output.write("- F77.DATA.Common:\t\t" + list.size() + "\n");
	//
	// // F77.DATA.Double
	// rule = new F77DATADouble();
	// executeRule();
	// output.write("- F77.DATA.Double:\t\t" + list.size() + "\n");
	//
	// // F77.DATA.Initialisation
	// rule = new F77DATAInitialization();
	// executeRule();
	// output.write("- F77.DATA.Initialisation:\t\t" + list.size() + "\n");
	//
	// // F77.DATA.IO
	// rule = new F77DATAIO();
	// executeRule();
	// output.write("- F77.DATA.IO:\t\t" + list.size() + "\n");
	//
	// // F77.DATA.LoopDo
	// rule = new F77DATALoopDO();
	// executeRule();
	// output.write("- F77.DATA.LoopDo:\t\t" + list.size() + "\n");
	//
	// // F77.DATA.Parameter
	// rule = new F77DATAParameter();
	// executeRule();
	// output.write("- F77.DATA.Parameter:\t\t" + list.size() + "\n");
	//
	// // F77.DATA.Type
	// rule = new F77DATAType();
	// executeRule();
	// output.write("- F77.DATA.Type:\t\t" + list.size() + "\n");
	//
	// // F77.ERR.OpenRead
	// rule = new F77ERROpenRead();
	// executeRule();
	// output.write("- F77.ERR.OpenRead:\t\t" + list.size() + "\n");
	//
	// // F77.INST.Assign
	// rule = new F77INSTAssign();
	// executeRule();
	// output.write("- F77.INST.Assign:\t\t" + list.size() + "\n");
	//
	// // F77.INST.Dimension
	// rule = new F77INSTDimension();
	// executeRule();
	// output.write("- F77.INST.Dimension:\t\t" + list.size() + "\n");
	//
	// // F77.INST.Equivalence
	// rule = new F77INSTEquivalence();
	// executeRule();
	// output.write("- F77.INST.Equivalence:\t\t" + list.size() + "\n");
	//
	// // F77.INST.Function
	// rule = new F77INSTFunction();
	// executeRule();
	// output.write("- F77.INST.Funcion:\t\t" + list.size() + "\n");
	//
	// // F77.INST.If
	// rule = new F77INSTIf();
	// executeRule();
	// output.write("- F77.INST.If:\t\t" + list.size() + "\n");
	//
	// // F77.INST.Include
	// rule = new F77INSTInclude();
	// executeRule();
	// output.write("- F77.INST.Include:\t\t" + list.size() + "\n");
	//
	// // F77.INST.Pause
	// rule = new F77INSTPause();
	// executeRule();
	// output.write("- F77.INST.Pause:\t\t" + list.size() + "\n");
	//
	// // F77.INST.Return
	// rule = new F77INSTReturn();
	// executeRule();
	// output.write("- F77.INST.Return:\t\t" + list.size() + "\n");
	//
	// // F77.INST.Save
	// rule = new F77INSTSave();
	// executeRule();
	// output.write("- F77.INST.Save:\t\t" + list.size() + "\n");
	//
	// // F77.MET.Line
	// rule = new F77METLine();
	// executeRule();
	// output.write("- F77.MET.Line:\t\t" + list.size() + "\n");
	//
	// // F77.NAME.GenericIntrinsic
	// rule = new F77NAMEGenericIntrinsic();
	// executeRule();
	// output.write("- F77.NAME.GenericIntrinsic:\t\t" + list.size() + "\n");
	//
	// // F77.NAME.Intrinsic
	// rule = new F77NAMEIntrinsic();
	// executeRule();
	// output.write("- F77.NAME.Intrinsic:\t\t" + list.size() + "\n");
	//
	// // F77.NAME.KeyWords
	// rule = new F77NAMEKeyWords();
	// executeRule();
	// output.write("- F77.NAME.KeyWords:\t\t" + list.size() + "\n");
	//
	// // F77.NAME.Label
	// rule = new F77NAMELabel();
	// executeRule();
	// output.write("- F77.NAME.Label:\t\t" + list.size() + "\n");
	//
	// // F77.PROTO:Declaration
	// rule = new F77PROTODeclaration();
	// executeRule();
	// output.write("- F77.PROTO.Declaration:\t\t" + list.size() + "\n");
	//
	// // F77.REF.IO
	// rule = new F77REFIO();
	// executeRule();
	// output.write("- F77.REF.IO:\t\t" + list.size() + "\n");
	//
	// // F77.REF.Open
	// rule = new F77REFOpen();
	// executeRule();
	// output.write("- F77.REF.Open:\t\t" + list.size() + "\n");
	//
	// // F77.REF.Parameter
	// rule = new F77REFParameter();
	// executeRule();
	// output.write("- F77.REF.Parameter:\t\t" + list.size() + "\n");
	//
	// // F77.TYPE.Basic
	// rule = new F77TYPEBasic();
	// executeRule();
	// output.write("- F77.TYPE.Basic:\t\t" + list.size() + "\n");
	//
	// // F77.TYPE.Hollerith
	// rule = new F77TYPEHollerith();
	// executeRule();
	// output.write("- F77.TYPE.Hollerith:\t\t" + list.size() + "\n");
	//
	//
	// }
	// /** After run for all files: close file writer**/
	// output.close();
	// } catch ( IOException e ) {
	// e.printStackTrace();
	// }
	//
	//
	//
	//
	// }
	//
	// private void executeRule() {
	// try {
	// rule.setContribution(TestUtils.getContribution("", ""));
	// rule.setInputFile(file);
	// list = rule.run();
	// } catch (FileNotFoundException e) {
	// e.printStackTrace();
	// } catch (IOException e) {
	// e.printStackTrace();
	// } catch (JFlexException e) {
	// e.printStackTrace();
	// }
	// }
}
