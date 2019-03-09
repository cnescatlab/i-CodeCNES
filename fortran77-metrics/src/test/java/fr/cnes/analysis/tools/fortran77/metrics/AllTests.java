package fr.cnes.analysis.tools.fortran77.metrics;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

@RunWith(Suite.class)
@SuiteClasses({TestF77METNesting.class,
        TestF77METComplexitySimplified.class, 
        TestF77METLineOfCode.class,
        TestF77METLineOfComment.class,
        TestF77METRatioComment.class})
public class AllTests {

}
