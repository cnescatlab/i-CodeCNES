/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.utils;

/**
 * This class is used to create final static attributes to handle the different
 * types of export
 * 
 * 
 */
public class TypeExportUtils {

    /**
     * File extension for a XML export
     */
    public final static String TYPE_EXPORT1 = "xml";

    /**
     * File extension for a CSV export
     */
    public final static String TYPE_EXPORT2 = "csv";

    /**
     * File name for the metric analysis results
     */
    public final static String METRIC_ANALYSIS_RESULT_FILENAME = "metricAnalysisResult.xml";

    /**
     * File name for the violation analysis results
     */
    public final static String VIOLATION_ANALYSIS_RESULT_FILENAME = "violationAnalysisResult.xml";

    /**
     * File name for the temporary file of violation analysis results
     */
    public final static String TEMP_VIOLATION_FILENAME = "tempViolation.xml";

    /**
     * File name for the temporary file of metric analysis results
     */
    public final static String TEMP_METRIC_FILENAME = "tempMetric.xml";
}
