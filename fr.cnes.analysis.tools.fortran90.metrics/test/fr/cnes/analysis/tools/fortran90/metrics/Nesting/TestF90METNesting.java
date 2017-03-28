/************************************************************************************************/
/** i-Code CNES is a static code analyzer.                                                       */
/** This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/** http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/
package fr.cnes.analysis.tools.fortran90.metrics.Nesting;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.junit.Test;

import fr.cnes.analysis.tools.analyzer.datas.AbstractMetric;
import fr.cnes.analysis.tools.analyzer.datas.FileValue;
import fr.cnes.analysis.tools.analyzer.datas.FunctionValue;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.fortran90.metrics.F90METNesting;
import fr.cnes.analysis.tools.fortran90.metrics.TestUtils;

/**
 * This class aims to test Don.Declaration rule. There are 2 functions in this
 * class. The first one verifies that an error in a file is detected whenever
 * there is one, the other verifies that nothing is detected when there's no
 * error.
 * 
 */
public class TestF90METNesting {

    /**
     * This test verifies that an error can be detected.
     */
    @Test
    public void testRunWithError() {

	try {
	    // Initializing rule and getting error file.
	    final AbstractMetric metric = new F90METNesting();
	    final String fileName = "file.f";
	    final IPath file = new Path(this.getClass().getResource(fileName).getPath());

	    // Defining file in the rule instantiation.
	    metric.setContribution(TestUtils.getContribution("", ""));
	    metric.setInputFile(file);

	    // We verify that the metric value detected is the right one.
	    // Get the list and verify each value
	    final FileValue fileValue = metric.run();
	    assertTrue(fileValue.getFilePath().toFile().getName().equals(fileName));
	    assertTrue(fileValue.getValue().isNaN());

	    // Value 1
	    final List<FunctionValue> functionValues = fileValue.getFunctionValues();

	    FunctionValue metricValue = functionValues.get(0);
	    assertTrue(metricValue.getLocation().equals("subroutine  osci_recherche_deb_plan_grp"));
	    assertTrue(metricValue.getValue() == 2.0);

	    // Value 2
	    metricValue = functionValues.get(1);
	    assertTrue(metricValue.getLocation().equals("subroutine  ostc_lecdon"));
	    assertTrue(metricValue.getValue() == 2.0);

	} catch (final FileNotFoundException e) {
	    fail("Erreur d'analyse (FileNotFoundException)");
	} catch (final IOException e) {
	    fail("Erreur d'analyse (IOException)");
	} catch (final JFlexException e) {
	    fail("Erreur d'analyse (JFlexException)");
	}
    }
}
