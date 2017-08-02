/************************************************************************************************/
/** i-Code CNES is a static code analyzer.                                                       */
/** This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/** http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/
package fr.cnes.analysis.tools.fortran90.metrics.ComplexitySimplified;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.fortran90.metrics.F90METComplexitySimplified;
import fr.cnes.analysis.tools.fortran90.metrics.TestUtils;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.eclipse.core.runtime.FileLocator;
import org.junit.Test;

/**
 * This class aims to test Don.Declaration rule. There are 2 functions in this
 * class. The first one verifies that an error in a file is detected whenever
 * there is one, the other verifies that nothing is detected when there's no
 * error.
 * 
 */
public class TestF90METComplexitySimplified {

    /**
     * This test verifies that an error can be detected.
     */
    @Test
    public void testRunWithError() {

        try {
            // Initializing rule and getting error file.
            final AbstractChecker metric = new F90METComplexitySimplified();
            final String fileName = "type_var2d_pdf2d.f90";
            final File file = new File(
                    FileLocator.resolve(this.getClass().getResource(fileName)).getFile());

            // Defining file in the rule instantiation.
            metric.setContribution(TestUtils.getContribution("", ""));
            metric.setInputFile(file);

            // File Value
            final List<CheckResult> checkResults = metric.run();
            CheckResult fileValue = null;
            for (CheckResult check : checkResults) {
                if (check.getLocation() == null || check.getLocation().isEmpty()) {
                    fileValue = check;
                    checkResults.remove(checkResults.indexOf(check));
                }
            }

            Float exceptedFileValue = Float.NaN;
        	assertTrue("Test except a file value of ["+exceptedFileValue +"] while metric computed ["+Math.round(fileValue.getValue())+"].", fileValue.getValue().equals(exceptedFileValue));
            final List<CheckResult> functionValues = checkResults;
            Map<String, Float> exceptedValues = new TreeMap<>();
            exceptedValues.put("function  interpolate_var2d_pdf2d_dp",(float)1.0);
            exceptedValues.put("function  interpolate_var2d_pdf2d_sp",(float)1.0);
            exceptedValues.put("function  set_var2d_pdf2d_dp",(float)7.0);
            exceptedValues.put("function  set_var2d_pdf2d_sp",(float)7.0);
            exceptedValues.put("module  procedure",(float)1.0);
            exceptedValues.put("module  type_var2d_pdf2d",(float)1.0);
            exceptedValues.put("subroutine  sample_var2d_pdf2d_dp",(float)2.0);
            exceptedValues.put("subroutine  sample_var2d_pdf2d_sp",(float)2.0);

            for(CheckResult metricValue : functionValues){
            	assertTrue("Test do not excepts function : "+metricValue.getLocation()+".",exceptedValues.containsKey(metricValue.getLocation()));
            	assertTrue("Test excepts value of ["+Math.round(exceptedValues.get(metricValue.getLocation()))+"] while metric computed ["+Math.round(metricValue.getValue())+"] for the function "+metricValue.getLocation()+".",Math.round(metricValue.getValue()) == Math.round(exceptedValues.get(metricValue.getLocation())));
            }
            assertTrue("Test excepts "+exceptedValues.size()+" functions computed for the file while the metric computed ["+functionValues.size()+"].",functionValues.size() == exceptedValues.size());

        } catch (final FileNotFoundException e) {
            fail("Erreur d'analyse (FileNotFoundException)");
        } catch (final IOException e) {
            fail("Erreur d'analyse (IOException)");
        } catch (final JFlexException e) {
            fail("Erreur d'analyse (JFlexException)");
        }
    }
}
