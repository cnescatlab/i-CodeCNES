/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.metrics;

/**
 * Interface which represents any level of a metric description. It could
 * describe the metric, a file or a value. This interface is mainly used to
 * display results in the MetricsView.
 * 
 */
public interface IMetricDescriptor {

    /**
     * Returns the name of this descriptor.
     * 
     * @return the name
     */
    String getName();

    /**
     * Returns the value of this descriptor.
     * 
     * @return the value
     */
    Float getValue();

    /**
     * Returns the mean value calculated for this descriptor.
     * 
     * @return the mean
     */
    Float getMean();

    /**
     * Returns the minimum value calculated for this descriptor.
     * 
     * @return the minimum
     */
    Float getMinimum();

    /**
     * Returns the maximum value calculated for this descriptor.
     * 
     * @return the maximum
     */
    Float getMaximum();

    /**
     * Returns the resource causing the minimum value calculated for this
     * descriptor.
     * 
     * @return the minCause
     */
    String getMinCause();

    /**
     * Returns the resource causing the maximum value calculated for this
     * descriptor.
     * 
     * @return the maxCause
     */
    String getMaxCause();

    /**
     * Returns true if the value is between thresholds defined in preferences,
     * false otherwise.
     * 
     * @return whether the value is good
     */
    boolean hasRightValue();
}
