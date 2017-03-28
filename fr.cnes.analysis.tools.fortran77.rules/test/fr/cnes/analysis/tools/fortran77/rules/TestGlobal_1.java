/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

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

public class TestGlobal_1 {
    /** This list contains all the violations when the analyse is executed **/
    public static List<Violation> list = new LinkedList<Violation>();
    List<IPath> listFiles = new LinkedList<IPath>();

    /**********************/
    /** PARAMS TO DEFINE **/
    /**********************/

    /** Folder where the function find files to execute the analyze **/
    final String resources = "/resources/f77_1";
    /** Extension of the files to be analyzed **/
    final String extension = "f";
    /** Id to execute the analysis **/
    final String ruleExtensionId = "fr.cnes.analysis.tools.fortran77.analyzer.rule";

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
            File fileResult = new File("resources/globalTest_1.txt");
            // fileResult.deleteOnExit();
            /**
             * File to compare with fileResult. This file is the verified
             * version of the execution
             **/
            File fileResultValidated = new File("resources/globalTestValidated_1.txt");

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
                    TestGlobal_1.list = analysis.getViolations();
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
                    if (file.getAbsolutePath().substring(i + 1).equals(extension)) {
                        IPath ipath = new Path(file.getPath());
                        listFiles.add(ipath);
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
                String file = list.get(0).getFilePath().lastSegment();
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
                        if (violation.getFilePath().lastSegment().equals(file)) {
                            errors++;
                        }
                        /** If the filename has change -> print error **/
                        else {
                            output.write(rule + " " + file + " " + errors + "\n");
                            file = violation.getFilePath().lastSegment();
                            errors = 1;
                        }
                    }
                    /** If rule has change -> print error **/
                    else {
                        output.write(rule + " " + file + " " + errors + "\n");
                        rule = violation.getRuleName();
                        file = violation.getFilePath().lastSegment();
                        errors = 1;
                    }
                }
            }
            /** Only one error -> print directly **/
            else if (list.size() > 0) {
                output.write(list.get(0).getRuleName() + " "
                        + list.get(0).getFilePath().lastSegment() + " 1\n");
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
            while (((line1 = reader1.readLine()) != null)
                    && ((line2 = reader2.readLine()) != null)) {
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

}
