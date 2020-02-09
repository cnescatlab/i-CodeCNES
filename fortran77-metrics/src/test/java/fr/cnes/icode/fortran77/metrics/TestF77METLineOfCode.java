/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/

package fr.cnes.icode.fortran77.metrics;

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
 * 
 */
public class TestF77METLineOfCode {

    /**
     * This test verifies that an error can be detected.
     */
    @Test
    public void testRunWithError() {

        try {
            // Initializing rule and getting error file.
            final AbstractChecker metric = new F77METLineOfCode();
            final String fileName = "dblat2.f";
            final File file = new File(this.getClass().getResource(String.format("/%s", fileName)).getFile());

            // Defining file in the rule instantiation.
            metric.setInputFile(file);

            // File Value
            final List<CheckResult> checkResults = metric.run();

            CheckResult fileValue = null;
            for (CheckResult check : checkResults) {
                if (check.getLocation()==null || check.getLocation().isEmpty()) {
                    fileValue = check;
                    checkResults.remove(check);
                }
            }
            if (fileValue == null) {
                fail("Error: No issue found in the file.");
            } else {
            	Float exceptedFileValue = (float) 2332.0;
                assertEquals(fileValue.getValue(), exceptedFileValue);
                Map<String, Float> exceptedValues = new TreeMap<>();
                exceptedValues.put("FUNCTION  DBEG",(float)21.0);
                exceptedValues.put("FUNCTION  DDIFF",(float)5.0);
                exceptedValues.put("FUNCTION  LDE",(float)14.0);
                exceptedValues.put("FUNCTION  LDERES",(float)40.0);
                exceptedValues.put("PROGRAM  DBLAT2",(float)254.0);
                exceptedValues.put("SUBROUTINE  CHKXER",(float)13.0);
                exceptedValues.put("SUBROUTINE  DCHK1",(float)255.0);
                exceptedValues.put("SUBROUTINE  DCHK2",(float)261.0);
                exceptedValues.put("SUBROUTINE  DCHK3",(float)278.0);
                exceptedValues.put("SUBROUTINE  DCHK4",(float)185.0);
                exceptedValues.put("SUBROUTINE  DCHK5",(float)207.0);
                exceptedValues.put("SUBROUTINE  DCHK6",(float)239.0);
                exceptedValues.put("SUBROUTINE  DCHKE",(float)299.0);
                exceptedValues.put("SUBROUTINE  DMAKE",(float)146.0);
                exceptedValues.put("SUBROUTINE  DMVCH",(float)86.0);
                exceptedValues.put("SUBROUTINE  XERBLA",(float)29.0);

                for(CheckResult metricValue : checkResults){
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
