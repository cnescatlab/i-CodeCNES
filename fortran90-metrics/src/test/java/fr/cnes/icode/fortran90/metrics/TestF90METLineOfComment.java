/************************************************************************************************/
/** i-Code CNES is a static code analyzer.                                                       */
/** This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/** http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/
package fr.cnes.icode.fortran90.metrics;

import fr.cnes.icode.datas.AbstractChecker;
import fr.cnes.icode.datas.CheckResult;
import fr.cnes.icode.exception.JFlexException;
import org.junit.Test;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

/**
 * This class aims to test Don.Declaration rule. There are 2 functions in this
 * class. The first one verifies that an error in a file is detected whenever
 * there is one, the other verifies that nothing is detected when there's no
 * error.
 */
public class TestF90METLineOfComment {

    /**
     * This test verifies that an error can be detected.
     */
    @Test
    public void testRunWithError() {

        try {
            // Initializing rule and getting error file.
            final AbstractChecker metric = new F90METLineOfComment();
            final String fileName = "type_var2d_pdf2d.f90";
            final File file = new File(this.getClass().getResource(String.format("/%s", fileName)).getFile());

            // Defining file in the rule instantiation.
            metric.setInputFile(file);

            // File Value
            final List<CheckResult> checkResults = metric.run();
            CheckResult fileValue = null;
            for (CheckResult check : checkResults) {
                if (check.getLocation()==null || check.getLocation().isEmpty()) {
                    fileValue = check;
                    checkResults.remove(checkResults.indexOf(check));
                }
            }

            if (fileValue == null) {
                fail("Error: No issue found in the file.");
            } else {


                // Value 1
                assertTrue("Test except a file value of ["+145.0 +"] while metric computed ["+Math.round(fileValue.getValue())+"].", fileValue.getValue().equals((float)145.0));

                final List<CheckResult> functionValues = checkResults;
                Map<String, Float> exceptedValues = new TreeMap<>();
                exceptedValues.put("function interpolate_var2d_pdf2d_dp",(float)22.0);
                exceptedValues.put("function interpolate_var2d_pdf2d_sp",(float)22.0);
                exceptedValues.put("function set_var2d_pdf2d_dp",(float)18.0);
                exceptedValues.put("function set_var2d_pdf2d_sp",(float)18.0);
                exceptedValues.put("interface interpolate_var2d_pdf2d",(float)0.0);
                exceptedValues.put("interface sample_var2d_pdf2d",(float)0.0);
                exceptedValues.put("interface set_var2d_pdf2d",(float)0.0);
                exceptedValues.put("module type_var2d_pdf2d",(float)4.0);
                exceptedValues.put("subroutine sample_var2d_pdf2d_dp",(float)17.0);
                exceptedValues.put("subroutine sample_var2d_pdf2d_sp",(float)17.0);

                for(CheckResult metricValue : functionValues){
                	assertTrue("Test do not excepts function : "+metricValue.getLocation()+".",exceptedValues.containsKey(metricValue.getLocation()));
                	assertTrue("Test excepts value of ["+Math.round(exceptedValues.get(metricValue.getLocation()))+"] while metric computed ["+Math.round(metricValue.getValue())+"] for the function "+metricValue.getLocation()+".",Math.round(metricValue.getValue()) == Math.round(exceptedValues.get(metricValue.getLocation())));
                }
                assertTrue("Test excepts "+exceptedValues.size()+" functions computed for the file while the metric computed ["+functionValues.size()+"].",functionValues.size() == exceptedValues.size());
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
