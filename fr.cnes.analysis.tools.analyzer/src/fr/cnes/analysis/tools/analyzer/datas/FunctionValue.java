/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */ 
package fr.cnes.analysis.tools.analyzer.datas;

/**
 * Each instance of this class represents the value of a rule / metric, for a
 * part of a file (function, subprogram, etc.).
 * 
 */
public class FunctionValue {
    /**
     * Value's or violation's location, such as Program, Method, Function, etc.
     **/
    private String location;
    /** Value of the metric in the current location or rule's violation's line. **/
    private Float value;
    /** Line of the location */
    private Integer line;

    /**
     * Constructor made with every attribute, exempt for the ID. An empty
     * instantiation is not allowed.
     * 
     * @param pLocation
     *            value's location
     * @param pValue
     *            metric's value or rule's violation's line
     * @param pLine 
     *            location's line    
     */
    public FunctionValue(final String pLocation, final Float pValue, final Integer pLine) {
        this.location = pLocation;
        this.value = pValue;
        this.line = pLine;
    }

    public int getLine() {
		return line;
	}

	public void setLine(int line) {
		this.line = line;
	}

	/**
     * Getter for location.
     * 
     * @return location of metric's value / rule's violation
     */
    public final String getLocation() {
        return this.location;
    }

    /**
     * Getter for metric's value or rule's violation's line.
     * 
     * @return metric's value or rule's violation's line
     */
    public final Float getValue() {
        return this.value;
    }

    /**
     * Setter for the location.
     * 
     * @param pLocation
     *            the location to set
     */
    public void setLocation(final String pLocation) {
        this.location = pLocation;
    }

    /**
     * Setter for value or line.
     * 
     * @param pValue
     *            the value or line to set
     */
    public void setValue(final Float pValue) {
        this.value = pValue;
    }
    
    

    /*
     * (non-Javadoc)
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(final Object object) {
        boolean isEqual;
        if (object instanceof FunctionValue) {
            isEqual =
                    this.location.equals(((FunctionValue) object)
                            .getLocation());
        } else {
            isEqual = false;
        }
        return isEqual;
    }

    /*
     * (non-Javadoc)
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        assert false : "hashCode not designed";
        return this.value.intValue();
    }
}
