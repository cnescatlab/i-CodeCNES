package fr.cnes.analysis.tools.fortran90.metrics;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import fr.cnes.analysis.tools.fortran90.metrics.ComplexitySimplified.TestF90METComplexitySimplified;
import fr.cnes.analysis.tools.fortran90.metrics.LineOfCode.TestF90METLineOfCode;
import fr.cnes.analysis.tools.fortran90.metrics.LineOfComment.TestF90METLineOfComment;
import fr.cnes.analysis.tools.fortran90.metrics.Nesting.TestF90METNesting;
import fr.cnes.analysis.tools.fortran90.metrics.RatioComment.TestF90METRatioComment;

@RunWith(Suite.class)
@SuiteClasses({TestF90METNesting.class, 
        TestF90METComplexitySimplified.class, 
        TestF90METLineOfCode.class,
        TestF90METLineOfComment.class,
        TestF90METRatioComment.class})
public class AllTests {

}
