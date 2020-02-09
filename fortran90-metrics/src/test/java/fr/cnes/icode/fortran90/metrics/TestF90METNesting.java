/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/

package fr.cnes.icode.fortran90.metrics;

import fr.cnes.icode.data.AbstractChecker;
import fr.cnes.icode.data.CheckResult;
import fr.cnes.icode.exception.JFlexException;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import static org.junit.jupiter.api.Assertions.*;

/**
 * This class aims to test Don.Declaration rule. There are 2 functions in this
 * class. The first one verifies that an error in a file is detected whenever
 * there is one, the other verifies that nothing is detected when there's no
 * error.
 */
public class TestF90METNesting {

    /**
     * This test verifies that an error can be detected.
     */
    @Test
    public void testRunWithError() {

        try {
            // Initializing rule and getting error file.
            final AbstractChecker metric = new F90METNesting();
            final String fileName = "type_var2d_pdf2d.f90";
            final File file = new File(this.getClass().getResource(String.format("/%s", fileName)).getFile());

            // Defining file in the rule instantiation.
            metric.setInputFile(file);

            // We verify that the metric value detected is the right one.
            // Get the list and verify each value
            final List<CheckResult> checkResults = metric.run();
            CheckResult fileValue = null;
            for (CheckResult check : checkResults) {
                if (check.getLocation() == null || check.getLocation().isEmpty()) {
                    fileValue = check;
                    checkResults.remove(check);
                }
            }
            if (fileValue == null) {
                fail("Error: No issue found in the file.");
            } else {
                assertTrue(fileValue.getValue().isNaN());
                Map<String, Float> exceptedValues = new TreeMap<>();
                exceptedValues.put("function  interpolate_var2d_pdf2d_dp", (float) 0.0);
                exceptedValues.put("function  interpolate_var2d_pdf2d_sp", (float) 0.0);
                exceptedValues.put("function  set_var2d_pdf2d_dp", (float) 2.0);
                exceptedValues.put("function  set_var2d_pdf2d_sp", (float) 2.0);
                exceptedValues.put("module  procedure", (float) 0.0);
                exceptedValues.put("module  type_var2d_pdf2d", (float) 0.0);
                exceptedValues.put("subroutine  sample_var2d_pdf2d_dp", (float) 1.0);
                exceptedValues.put("subroutine  sample_var2d_pdf2d_sp", (float) 1.0);

                for (CheckResult metricValue : checkResults) {
                    assertTrue(exceptedValues.containsKey(metricValue.getLocation()));
                    assertEquals(Math.round(metricValue.getValue()), Math.round(exceptedValues.get(metricValue.getLocation())));
                }
                assertEquals(checkResults.size(), exceptedValues.size());
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
