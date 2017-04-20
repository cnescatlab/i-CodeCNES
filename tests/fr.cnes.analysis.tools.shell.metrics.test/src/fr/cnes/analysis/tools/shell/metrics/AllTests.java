/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

package fr.cnes.analysis.tools.shell.metrics;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import fr.cnes.analysis.tools.shell.metrics.MET.ComplexitySimplified.TestSHMETComplexitySimplified;
import fr.cnes.analysis.tools.shell.metrics.MET.LineOfCode.TestSHMETLineOfCode;
import fr.cnes.analysis.tools.shell.metrics.MET.Nesting.TestSHMETNesting;
import fr.cnes.analysis.tools.shell.metrics.MET.RatioComment.TestSHMETRatioComment;



@RunWith(Suite.class)
@SuiteClasses({ 
    TestSHMETComplexitySimplified.class,
    TestSHMETNesting.class,
    TestSHMETLineOfCode.class,
    TestSHMETRatioComment.class})
public class AllTests {
}
