/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.IPath;

/**
 * Descriptor for a Function that is intended to be shown.</br>
 * This descriptor would return it's {@link #location} and the number of
 * violations that it contains while using {@link #getName()} and
 * {@link #getValue()} when instanced in a {@link IFileRuleDescriptor}.</br>
 * 
 * Descriptors of this file are {@link RuleDescriptor} for each rule violated in
 * the function described by this class.
 * 
 * @see IFileRuleDescriptor
 * @see FileRuleDescriptor
 * @see RuleDescriptor
 * @version 2.1
 * @since 2.0
 */
public class FunctionDescriptor implements IFileRuleDescriptor, Cloneable {

    /** Function containing the violation. **/
    private String location;
    /** Line of the violation. **/
    private Integer value;
    /** File name */
    private IPath filePath;
    /** List of all rules violated in the file */
    private List<RuleDescriptor> descriptors;

    /**
     * Empty constructor.
     */
    public FunctionDescriptor() {
        this.descriptors = new LinkedList<>();
        this.location = "";
        this.value = -1;
    }

    /**
     * @param pLocation
     * @param pValue
     * @param pFilePath
     */
    public FunctionDescriptor(final String pLocation, final Integer pValue, final IPath pFilePath) {
        super();
        this.descriptors = new LinkedList<>();
        this.location = pLocation;
        this.value = pValue;
        this.filePath = pFilePath;
    }

    @Override
    public String getName() {
        return this.location;
    }

    @Override
    public Integer getValue() {
        Integer sum = 0;
        for (RuleDescriptor r : this.descriptors) {
            sum += r.getValue();
        }
        return sum;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((location == null) ? 0 : location.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        FunctionDescriptor other = (FunctionDescriptor) obj;
        if (location == null) {
            if (other.location != null)
                return false;
        } else if (!location.equals(other.location))
            return false;
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public FunctionDescriptor clone() throws CloneNotSupportedException {
        final FunctionDescriptor clone = (FunctionDescriptor) super.clone();
        clone.setDescriptors(new LinkedList<RuleDescriptor>(this.descriptors));
        clone.setFilePath(this.filePath);
        clone.setLocation(this.location);
        clone.setValue(this.value);
        return clone;
    }

    /**
     * @return the location
     */
    public String getLocation() {
        return location;
    }

    /**
     * @param pLocation
     *            the location to set
     */
    public void setLocation(String pLocation) {
        this.location = pLocation;
    }

    /**
     * @return the filePath
     */
    public IPath getFilePath() {
        return filePath;
    }

    /**
     * @param pFilePath
     *            the filePath to set
     */
    public void setFilePath(IPath pFilePath) {
        this.filePath = pFilePath;
    }

    /**
     * @return the descriptors
     */
    public List<RuleDescriptor> getDescriptors() {
        return descriptors;
    }

    /**
     * @param pDescriptors
     *            the descriptors to set
     */
    public void setDescriptors(List<RuleDescriptor> pDescriptors) {
        this.descriptors = pDescriptors;
    }

    /**
     * @param pValue
     *            the value to set
     */
    public void setValue(Integer pValue) {
        this.value = pValue;
    }

}
