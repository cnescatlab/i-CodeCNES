/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */ 
package fr.cnes.analysis.tools.analyzer.datas;

import java.io.IOException;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

/**
 * Abstract class implementing the generic application of a metric over a file.
 * For each metric and file, an instance is made.
 * 
 */
public abstract class AbstractMetric extends AbstractEvaluation {

    /**
     * Run analysis for considering file and metric.
     * 
     * @return list of
     *         {@link fr.cnes.analysis.tools.analyzer.metrics.FileValue} found
     *         during analysis
     * @throws IOException
     *             IO problem occurred
     * @throws JFlexException
     *             JFlex error during analysis
     */
    public abstract FileValue run() throws IOException, JFlexException;
}
