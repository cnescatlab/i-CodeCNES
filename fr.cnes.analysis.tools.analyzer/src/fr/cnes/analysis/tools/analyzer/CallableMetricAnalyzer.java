/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.analysis.tools.analyzer;

import fr.cnes.analysis.tools.analyzer.datas.AbstractMetric;
import fr.cnes.analysis.tools.analyzer.datas.FileValue;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.logging.Logger;

/**
 * This class is responsible of computing a metric on a file and to return it's
 * results as a Thread by implementing {@link Callable} interface.
 * 
 * @since 3.0
 */
public class CallableMetricAnalyzer implements Callable<List<FileValue>> {

    /** Logger */
    private static final Logger LOGGER = Logger.getLogger(CallableMetricAnalyzer.class.getName());

    /** The metric to compute */
    private AbstractMetric metric;
    /** The file to analyze */
    private File file;

    /**
     * Constructor for {@link CallableMetricAnalyzer}.
     * 
     * @param pMetric
     *            to compute
     * @param pInputFile
     *            to analyze
     */
    public CallableMetricAnalyzer(AbstractMetric pMetric, File pInputFile) {
        this.metric = pMetric;
        this.file = pInputFile;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.concurrent.Callable#call()
     */
    @Override
    public List<FileValue> call() throws IOException, JFlexException {
        LOGGER.entering(this.getClass().getName(), "call");
        final List<FileValue> results = new ArrayList<>();
        metric.setInputFile(file);
        results.add(metric.run());
        return results;
    }
}
