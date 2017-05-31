/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/

package fr.cnes.analysis.tools.shell.metrics.MET.RatioComment;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.shell.metrics.SHMETRatioComment;
import fr.cnes.analysis.tools.shell.metrics.TestUtils;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;
import org.eclipse.core.runtime.FileLocator;
import org.junit.Test;

/**
 * This class aims to test Don.Declaration rule. There are 2 functions in this
 * class. The first one verifies that an error in a file is detected whenever
 * there is one, the other verifies that nothing is detected when there's no
 * error.
 * 
 */
public class TestSHMETRatioComment {

    /**
     * This test verifies that an error can be detected.
     */
    @Test
    public void testRunWithError() {

        try {
            // Initializing rule and getting error file.
            final AbstractChecker metric = new SHMETRatioComment();
            final String fileName = "file.sh";
            final File file = new File(
                    FileLocator.resolve(this.getClass().getResource(fileName)).getFile());

            // Defining file in the rule instantiation.
            metric.setContribution(TestUtils.getContribution("", ""));
            metric.setInputFile(file);

            // We verify that the metric value detected is the right one.
            // Get the list and verify each value
            final List<CheckResult> checkResults = metric.run();
            CheckResult fileValue = null;
            for (CheckResult check : checkResults) {
                if (check.getLocation().equals("FILE")) {
                    fileValue = check;
                    checkResults.remove(checkResults.indexOf(check));
                }
            }
            if (fileValue == null) {
                fail("Erreur : Aucun résultat sur le fichier trouvé.");
            } else {

                assertTrue(fileValue.getFile().getName().equals(fileName));
                assertTrue(fileValue.getValue() > 0.34 && fileValue.getValue() > 0.35);

                // Value 1
                final List<CheckResult> functionValues = checkResults;

                CheckResult metricValue = functionValues.get(0);
                assertTrue(metricValue.getLocation().equals("help"));
                assertTrue(metricValue.getValue() == 0.5);

                // Value 2
                metricValue = functionValues.get(1);
                assertTrue(metricValue.getLocation().equals("search_base_dir"));
                assertTrue(metricValue.getValue() == 0.1875);

                // Value 3
                metricValue = functionValues.get(2);
                assertTrue(metricValue.getLocation().equals("verify_process"));
                assertTrue(metricValue.getValue() > 0.33 && metricValue.getValue() < 0.34);

                // Value 4
                metricValue = functionValues.get(3);
                assertTrue(metricValue.getLocation().equals("MAIN PROGRAM"));
                assertTrue(metricValue.getValue() > 0.074 && metricValue.getValue() < 0.075);
            }
        } catch (final FileNotFoundException e) {
            fail("Erreur d'analyse (FileNotFoundException)");
        } catch (final IOException e) {
            fail("Erreur d'analyse (IOException)");
        } catch (final JFlexException e) {
            fail("Erreur d'analyse (JFlexException)");
        }
    }
}
