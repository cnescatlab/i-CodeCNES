/************************************************************************************************/
/** i-Code CNES is a static code analyzer.                                                       */
/** This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/** http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/
package fr.cnes.analysis.tools.fortran77.metrics.Nesting;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.eclipse.core.runtime.FileLocator;
import org.junit.Test;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.fortran77.metrics.F77METNesting;
import fr.cnes.analysis.tools.fortran77.metrics.TestUtils;

/**
 * This class aims to test Don.Declaration rule. There are 2 functions in this
 * class. The first one verifies that an error in a file is detected whenever
 * there is one, the other verifies that nothing is detected when there's no
 * error.
 * 
 */
public class TestF77METNesting {

    /**
     * This test verifies that an error can be detected.
     */
    @Test
    public void testRunWithError() {

        try {
            // Initializing rule and getting error file.
            final AbstractChecker metric = new F77METNesting();
            final String fileName = "dblat2.f";
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
                if (check.getLocation()==null || check.getLocation().isEmpty()) {
                    fileValue = check;
                    checkResults.remove(checkResults.indexOf(check));
                }
            }
            if (fileValue == null) {
                fail("Erreur : Aucun résultat sur le fichier trouvé.");
            } else {
            	Float exceptedFileValue = Float.NaN;
            	assertTrue("Test except a file value of ["+exceptedFileValue +"] while metric computed ["+Math.round(fileValue.getValue())+"].", fileValue.getValue().equals(exceptedFileValue));
                final List<CheckResult> functionValues = checkResults;
                Map<String, Float> exceptedValues = new TreeMap<>();
                exceptedValues.put("FUNCTION  DBEG",(float)1.0);
                exceptedValues.put("FUNCTION  DDIFF",(float)0.0);
                exceptedValues.put("FUNCTION  LDE",(float)2.0);
                exceptedValues.put("FUNCTION  LDERES",(float)4.0);
                exceptedValues.put("PROGRAM  DBLAT2",(float)2.0);
                exceptedValues.put("SUBROUTINE  CHKXER",(float)1.0);
                exceptedValues.put("SUBROUTINE  DCHK1",(float)10.0);
                exceptedValues.put("SUBROUTINE  DCHK2",(float)9.0);
                exceptedValues.put("SUBROUTINE  DCHK3",(float)9.0);
                exceptedValues.put("SUBROUTINE  DCHK4",(float)8.0);
                exceptedValues.put("SUBROUTINE  DCHK5",(float)8.0);
                exceptedValues.put("SUBROUTINE  DCHK6",(float)9.0);
                exceptedValues.put("SUBROUTINE  DCHKE",(float)1.0);
                exceptedValues.put("SUBROUTINE  DMAKE",(float)5.0);
                exceptedValues.put("SUBROUTINE  DMVCH",(float)3.0);
                exceptedValues.put("SUBROUTINE  XERBLA",(float)2.0);

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
