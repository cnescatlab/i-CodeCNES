/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.preferencepage.metrics;

/**
 * Metric default value, levels,...
 *
 */
public final class MetricValues {

    /** Metrics names. */
    private static String[] METRICS = { "METComplexitySimplified", "METNesting", "METLineOfCode",
            "METRatioComment" };

    /** Comparison symbol. */
    private static String[] SYMBOL = { "le", "le", "le", "ge" };

    /** Metrics default values. */
    private static Float[][] VALUES = { { (float) 10.0, (float) 5.0, (float) 60, (float) 30.0 },
            { (float) 10.0, (float) 5.0, (float) 60.0, (float) 30.0 },
            { (float) 12.0, (float) 6.0, (float) 80.0, (float) 20.0 },
            { (float) 20.0, (float) 7.0, (float) 100.0, (float) 20.0 } };

    /**
     * Private constructor.
     */
    private MetricValues() {
        super();
    }

    /**
     * @return the mETRICS
     */
    public static String[] getMETRICS() {
        return METRICS;
    }

    /**
     * @return the sYMBOL
     */
    public static String[] getSYMBOL() {
        return SYMBOL;
    }

    /**
     * @return the vALUES
     */
    public static Float[][] getVALUES() {
        return VALUES;
    }

}
