package fr.cnes.analysis.tools.fortran77.metrics;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import fr.cnes.analysis.tools.fortran77.metrics.ComplexitySimplified.TestF77METComplexitySimplified;
import fr.cnes.analysis.tools.fortran77.metrics.LineOfCode.TestF77METLineOfCode;
import fr.cnes.analysis.tools.fortran77.metrics.LineOfComment.TestF77METLineOfComment;
import fr.cnes.analysis.tools.fortran77.metrics.Nesting.TestF77METNesting;
import fr.cnes.analysis.tools.fortran77.metrics.RatioComment.TestF77METRatioComment;

@RunWith(Suite.class)
@SuiteClasses({TestF77METNesting.class,
        TestF77METComplexitySimplified.class, 
        TestF77METLineOfCode.class,
        TestF77METLineOfComment.class,
        TestF77METRatioComment.class})
public class AllTests {

}
