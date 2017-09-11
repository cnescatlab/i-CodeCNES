/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.IPath;

import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;

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
    /** Class name **/
    private static final String CLASS = FunctionDescriptor.class.getName();

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
        final String method = "FunctionDescriptor";
        ICodeLogger.entering(CLASS, method);
        this.descriptors = new LinkedList<>();
        this.location = "";
        this.value = Integer.valueOf(-1);
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @param pLocation
     *            Function's name.
     * @param pValue
     *            Value computed for the function.
     * @param pFilePath
     *            Function's file.
     */
    public FunctionDescriptor(final String pLocation, final Integer pValue, final IPath pFilePath) {
        super();
        final String method = "FunctionDescriptor";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pLocation, pValue, pFilePath
        });
        this.descriptors = new LinkedList<>();
        this.location = pLocation;
        this.value = pValue;
        this.filePath = pFilePath;
        ICodeLogger.exiting(CLASS, method);
    }

    @Override
    public String getName() {
        final String method = "getName";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, location);
        return this.location;
    }

    @Override
    public Integer getValue() {
        final String method = "getValue";
        ICodeLogger.entering(CLASS, method);
        int sum = 0;
        for (RuleDescriptor r : this.descriptors) {
            sum += r.getValue().intValue();
        }
        ICodeLogger.exiting(CLASS, method, Integer.valueOf(sum));
        return Integer.valueOf(sum);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final String method = "hashCode";
        ICodeLogger.entering(CLASS, method);
        final int prime = 31;
        int result = 1;
        result = prime * result + ((location == null) ? 0 : location.hashCode());
        ICodeLogger.exiting(CLASS, method, Integer.valueOf(result));
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(final Object obj) {
        final String method = "equals";
        ICodeLogger.entering(CLASS, method, obj);
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final FunctionDescriptor other = (FunctionDescriptor) obj;
        if (location == null) {
            if (other.location != null) {
                return false;
            }
        } else if (!location.equals(other.location)) {
            return false;
        }
        ICodeLogger.exiting(CLASS, method, Boolean.TRUE);
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public FunctionDescriptor clone() throws CloneNotSupportedException {
        final String method = "clone";
        ICodeLogger.entering(CLASS, method);
        final FunctionDescriptor clone = (FunctionDescriptor) super.clone();
        clone.setDescriptors(new LinkedList<RuleDescriptor>(this.descriptors));
        clone.setFilePath(this.filePath);
        clone.setLocation(this.location);
        clone.setValue(this.value);
        ICodeLogger.exiting(CLASS, method, clone);
        return clone;
    }

    /**
     * @return the location
     */
    public String getLocation() {
        final String method = "getLocation";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, location);
        return location;
    }

    /**
     * @param pLocation
     *            the location to set
     */
    public void setLocation(final String pLocation) {
        final String method = "setLocation";
        ICodeLogger.entering(CLASS, method, pLocation);
        this.location = pLocation;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the filePath
     */
    public IPath getFilePath() {
        final String method = "getFilePath";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, filePath);
        return filePath;
    }

    /**
     * @param pFilePath
     *            the filePath to set
     */
    public void setFilePath(final IPath pFilePath) {
        final String method = "setFilePath";
        ICodeLogger.entering(CLASS, method, pFilePath);
        this.filePath = pFilePath;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the descriptors
     */
    public List<RuleDescriptor> getDescriptors() {
        final String method = "getDescriptors";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, descriptors);
        return descriptors;
    }

    /**
     * @param pDescriptors
     *            the descriptors to set
     */
    public void setDescriptors(List<RuleDescriptor> pDescriptors) {
        final String method = "setDescriptors";
        ICodeLogger.entering(CLASS, method, pDescriptors);
        this.descriptors = pDescriptors;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @param pValue
     *            the value to set
     */
    public void setValue(Integer pValue) {
        final String method = "setValue";
        ICodeLogger.entering(CLASS, method, pValue);
        this.value = pValue;
        ICodeLogger.exiting(CLASS, method);
    }

}
