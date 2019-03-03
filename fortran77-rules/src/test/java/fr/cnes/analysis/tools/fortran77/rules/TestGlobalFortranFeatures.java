/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import fr.cnes.analysis.tools.analyzer.Analyzer;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.io.*;
import java.util.*;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

/**
 * This class is intended to run test on all the files contained in the package
 * f77_1, f77_2, f77_3 and f77_4 of resources.
 */
@RunWith(Parameterized.class)
public class TestGlobalFortranFeatures {

    @Parameterized.Parameters(name = "TEST GLOBAL {index}: {4}")
    public static Iterable<Object[]> data() {
        return Arrays.asList(new Object[][]{
                {"f77_1", "/globalTestValidated_1.txt"},
                {"f77_2", "/globalTestValidated_2.txt"},
                {"f77_3", "/globalTestValidated_3.txt"},
                {"f77_4", "/globalTestValidated_4.txt"}
        });
    }

    /** This list contains all the violations when the analysis is executed **/
    private List<CheckResult> list = new LinkedList<>();
    /** File to analyze */
    private Set<File> listFiles = new HashSet<>();

    /**********************/
    /** PARAMS TO DEFINE **/
    /**********************/

    /** Folder where the function find files to execute the analyze **/
    @Parameterized.Parameter
    public String resources;
    /** Folder where the function find files to execute the analyze **/
    @Parameterized.Parameter(1)
    public String validatedResultFilename;
    /** Extension of the files to be analyzed **/
    private final String extension = "f";
    /** Id to execute the analysis **/
    private final String languageId = "fr.cnes.analysis.tools.languages.f77";
    /** Id of metrics of Fortran 77 that are excluded from the analysis */
    private final String[] excludedIds = new String[] {
        "fr.cnes.analysis.tools.fortran77.metrics.F77METComplexitySimplified",
        "fr.cnes.analysis.tools.fortran77.metrics.F77METNesting",
        "fr.cnes.analysis.tools.fortran77.metrics.F77METLineOfCode",
        "fr.cnes.analysis.tools.fortran77.metrics.F77METRatioComment"
    };

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
            /* File where to save the result of the execution */
            final File fileResult = new File(String.format("target%s_result.txt", resources));
            fileResult.deleteOnExit();
            /*
             * File to compare with fileResult. This file is the verified
             * version of the execution
             */
            final File fileResultValidated = new File(this.getClass().getResource(validatedResultFilename).getFile());

            /* Get resources file */
            final String resourcesPath = System.getProperty("user.dir") + resources;
            final File folder = new File(resourcesPath);
            /* Save files into a list */
            getFilesIntoFolder(folder);

            /* Create the analysis job */
            final Analyzer analysis = new Analyzer();
            final List<String> extensionIds = new ArrayList<>();
            extensionIds.add(languageId);

            this.list = analysis.stableCheck(listFiles, extensionIds, Arrays.asList(excludedIds));

            /* Export values into file */
            createExportFile(fileResult);

            /* Compare new test with last one */
            assertTrue(compareFiles(fileResult, fileResultValidated));

        } catch (final JFlexException exception) {
            fail("Analysis error (JFlexException)");
            exception.printStackTrace();
        }

    }

    /**
     * Get all files in the given directory.
     *
     * @param folder to run analysis on.
     */
    private void getFilesIntoFolder(final File folder) {
        if (folder.isDirectory()) {
            final File[] listOfFiles = folder.listFiles();
            /* For each file, check the extension and save IPath into List */
            for (final File file : listOfFiles) {
                if (file.isDirectory()) {
                    getFilesIntoFolder(file);
                } else {
                    final int i = file.getAbsolutePath().lastIndexOf(".");
                    if (file.getAbsolutePath().substring(i + 1).equals(extension)) {
                        listFiles.add(file);
                    }
                }
            }
        }

    }

    /**
     * Function to create the error file. The file has the following: RuleName
     * FileName NumError
     * 
     * @param fileResult The file to export result in.
     */
    private void createExportFile(final File fileResult) {
        /* Create result file */
        try (final BufferedWriter output = new BufferedWriter(new FileWriter(fileResult))){
            /* If the list is bigger than one */
            if (list.size() > 1) {
                String rule = list.get(0).getName();
                String file = list.get(0).getFile().getAbsolutePath();
                int errors = 1;
                /* Iterate over the elements */
                for (int i = 1; i < list.size(); i++) {
                    final CheckResult violation = list.get(i);
                    /* If the is more errors in the same rule */
                    if (violation.getName().equals(rule)) {
                        /*
                         * If there is more errors in the same file -> increase
                         * error
                         */
                        if (violation.getFile().getAbsolutePath().equals(file)) {
                            errors++;
                        }
                        /* If the filename has change -> print error */
                        else {
                            output.write(rule + " " + file + " " + errors + "\n");
                            file = violation.getFile().getAbsolutePath();
                            errors = 1;
                        }
                    }
                    /* If rule has change -> print error */
                    else {
                        output.write(rule + " " + file + " " + errors + "\n");
                        rule = violation.getName();
                        file = violation.getFile().getAbsolutePath();
                        errors = 1;
                    }
                }
            }
            /* Only one error -> print directly */
            else if (list.size() > 0) {
                output.write(list.get(0).getName() + " "
                                + list.get(0).getFile().getAbsolutePath()
                                + " 1\n");
            }
            /* After run for all files: close file writer */
            output.close();
        } catch (IOException exception) {
            fail("Analysis error (IOException)");
        }
    }

    /**
     * Comparison between files
     * 
     * @param fileResult
     *            File generated by the analysis.
     * @param fileResultValidated
     *            file to compare the analysis with.
     * @return equals indicate the equality of the files
     */
    private boolean compareFiles(final File fileResult, final File fileResultValidated) {
        boolean equals = true;
        try (
                final BufferedReader testReader = new BufferedReader(new FileReader(fileResult));
                final BufferedReader validatedReader = new BufferedReader(new FileReader(fileResultValidated))) {
            /* String for compare */
            String testedLine;
            String validatedLine;

            /* Loop over the lines to find differences */
            testedLine = testReader.readLine();
            validatedLine = validatedReader.readLine();
            while ((testReader.readLine() != null) && ((validatedReader.readLine()) != null)
                            && equals) {
                if (!testedLine.equalsIgnoreCase(validatedLine)) {
                    equals = false;
                }
                testedLine = testReader.readLine();
                validatedLine = validatedReader.readLine();
            }
            /*
             * Both documents should be at their last line at the end of the
             * test.
             */
            final boolean bothDocumentEndingNull = (testedLine == null && validatedLine == null);
            final boolean bothDocumentEndingWithSameLine = (testedLine != null
                            && validatedLine != null)
                            && !testedLine.equalsIgnoreCase(validatedLine);
            if (bothDocumentEndingNull || bothDocumentEndingWithSameLine) {
                equals = false;
            }
        } catch (final IOException exception) {
            fail("Analysis error ("+exception.getClass().getName()+")");
        }
        /* Return value */
        return equals;
    }

}
