/************************************************************************************************/
/** i-Code CNES is a static code analyzer.                                                       */
/** This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/** http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/
package fr.cnes.analysis.tools.fortran77.metrics.ComplexitySimplified;

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
import fr.cnes.analysis.tools.fortran77.metrics.F77METComplexitySimplified;
import fr.cnes.analysis.tools.fortran77.metrics.TestUtils;

/**
 * This class aims to test Don.Declaration rule. There are 2 functions in this
 * class. The first one verifies that an error in a file is detected whenever
 * there is one, the other verifies that nothing is detected when there's no
 * error.
 */
public class TestF77METComplexitySimplified {

    /**
     * This test verifies that an error can be detected.
     */
    @Test
    public void testRunWithError() {

        try {
            // Initializing rule and getting error file.
            final AbstractChecker metric = new F77METComplexitySimplified();
            final String fileName = "dblat2.f";
            final File file = new File(
                    FileLocator.resolve(this.getClass().getResource(fileName)).getFile());

            // Defining file in the rule instantiation.
            metric.setContribution(TestUtils.getContribution("", ""));
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
                fail("Erreur : Aucun résultat sur le fichier trouvé.");
            } else {
                // CheckResult fileValue;
                for (CheckResult check : checkResults) {
                    if (check.getLocation()==null || check.getLocation().isEmpty()) {
                        fileValue = check;
                        checkResults.remove(checkResults.indexOf(check));
                    }
                }
                Float exceptedFileValue = Float.NaN;
            	assertTrue("Test except a file value of ["+exceptedFileValue +"] while metric computed ["+Math.round(fileValue.getValue())+"].", fileValue.getValue().equals(exceptedFileValue));
                final List<CheckResult> functionValues = checkResults;
                Map<String, Float> exceptedValues = new TreeMap<>();
                exceptedValues.put("FUNCTION  DBEG",(float)3.0);
                exceptedValues.put("FUNCTION  DDIFF",(float)1.0);
                exceptedValues.put("FUNCTION  LDE",(float)3.0);
                exceptedValues.put("FUNCTION  LDERES",(float)12.0);
                exceptedValues.put("PROGRAM  DBLAT2",(float)27.0);
                exceptedValues.put("SUBROUTINE  CHKXER",(float)2.0);
                exceptedValues.put("SUBROUTINE  DCHK1",(float)42.0);
                exceptedValues.put("SUBROUTINE  DCHK2",(float)46.0);
                exceptedValues.put("SUBROUTINE  DCHK3",(float)59.0);
                exceptedValues.put("SUBROUTINE  DCHK4",(float)30.0);
                exceptedValues.put("SUBROUTINE  DCHK5",(float)38.0);
                exceptedValues.put("SUBROUTINE  DCHK6",(float)44.0);
                exceptedValues.put("SUBROUTINE  DCHKE",(float)2.0);
                exceptedValues.put("SUBROUTINE  DMAKE",(float)41.0);
                exceptedValues.put("SUBROUTINE  DMVCH",(float)13.0);
                exceptedValues.put("SUBROUTINE  XERBLA",(float)4.0);

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
