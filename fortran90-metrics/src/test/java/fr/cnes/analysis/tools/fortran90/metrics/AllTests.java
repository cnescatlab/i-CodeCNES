package fr.cnes.analysis.tools.fortran90.metrics;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

@RunWith(Suite.class)
@SuiteClasses({TestF90METNesting.class, 
        TestF90METComplexitySimplified.class, 
        TestF90METLineOfCode.class,
        TestF90METLineOfComment.class,
        TestF90METRatioComment.class})
public class AllTests {

}
