/************************************************************************************************/
/** i-Code CNES is a static code analyzer.                                                       */
/** This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/** http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/
package fr.cnes.analysis.tools.fortran77.metrics.RatioComment;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.junit.Test;

import fr.cnes.analysis.tools.analyzer.datas.AbstractMetric;
import fr.cnes.analysis.tools.analyzer.datas.FileValue;
import fr.cnes.analysis.tools.analyzer.datas.FunctionValue;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.fortran77.metrics.F77METRatioComment;
import fr.cnes.analysis.tools.fortran77.metrics.TestUtils;

/**
 * This class aims to test Don.Declaration rule. There are 2 functions in this
 * class. The first one verifies that an error in a file is detected whenever
 * there is one, the other verifies that nothing is detected when there's no
 * error.
 * 
 */
public class TestF77METRatioComment {

    /**
     * This test verifies that an error can be detected.
     */
    @Test
    public void testRunWithError() {

	try {
	    // Initializing rule and getting error file.
	    final AbstractMetric metric = new F77METRatioComment();
	    final String fileName = "file.f";
	    final IPath file = new Path(FileLocator.resolve(this.getClass().getResource(fileName)).getFile());

	    // Defining file in the rule instantiation.
	    metric.setContribution(TestUtils.getContribution("", ""));
	    metric.setInputFile(file);

	    // File Value
	    final FileValue fileValue = metric.run();
	    assertTrue(fileValue.getFilePath().toFile().getName().equals(fileName));
	    // assertTrue(fileValue.getValue() > 52.1
	    // && fileValue.getValue() < 52.2);

	    // Value 1
	    final List<FunctionValue> functionValues = fileValue.getFunctionValues();

	    FunctionValue metricValue = functionValues.get(0);
	    assertTrue(metricValue.getLocation().equals("subroutine osci_recherche_deb_plan_grp"));
	    assertTrue(metricValue.getValue() > 62.54 && metricValue.getValue() < 62.55);

	    // Value 2
	    metricValue = functionValues.get(1);
	    assertTrue(metricValue.getLocation().equals("subroutine ostc_lecdon"));
	    assertTrue(metricValue.getValue() > 48.80 && metricValue.getValue() < 48.81);

	} catch (final FileNotFoundException e) {
	    fail("Erreur d'analyse (FileNotFoundException)");
	} catch (final IOException e) {
	    fail("Erreur d'analyse (IOException)");
	} catch (final JFlexException e) {
	    fail("Erreur d'analyse (JFlexException)");
	}
    }
}
