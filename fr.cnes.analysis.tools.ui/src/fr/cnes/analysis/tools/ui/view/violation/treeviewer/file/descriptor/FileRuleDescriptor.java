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
 * Descriptor for a File that is intended to be shown.</br>
 * This descriptor would return it's {@link #filePath} and the number of
 * violations that it contains when using {@link #getName()} and
 * {@link #getValue()} when instanced in a {@link IFileRuleDescriptor}.</br>
 * 
 * Descriptors of this file are {@link FunctionDescriptor} for all violations
 * contained in a function of the file described by the class.
 * 
 * @see IFileRuleDescriptor
 * @version 2.1
 * @since 2.0
 */
public class FileRuleDescriptor implements IFileRuleDescriptor, Cloneable {

    /** File's path. **/
    private IPath filePath;
    /** List of violations in the file. **/
    private List<FunctionDescriptor> descriptors;

    /**
     * Empty constructor.
     */
    public FileRuleDescriptor() {
        this.descriptors = new LinkedList<FunctionDescriptor>();
    }

    /**
     * Constructor with the file's path.
     * 
     * @param pFilePath
     *            the file's path.
     */
    public FileRuleDescriptor(final IPath pFilePath) {
        this.filePath = pFilePath;
        this.descriptors = new LinkedList<FunctionDescriptor>();
    }

    /**
     * Getter for the file's path.
     * 
     * @return file's path
     */
    public IPath getFilePath() {
        return this.filePath;
    }

    /**
     * Getter for the descriptors.
     * 
     * @return the descriptors
     */
    public List<FunctionDescriptor> getDescriptors() {
        return this.descriptors;
    }

    /**
     * Setter for the file's path.
     * 
     * @param pFilePath
     *            the file's path to set
     */
    public void setFilePath(final IPath pFilePath) {
        this.filePath = pFilePath;
    }

    /**
     * Setter for the descriptors.
     * 
     * @param pDescriptors
     *            the descriptors to set
     */
    public void setDescriptors(final List<FunctionDescriptor> pDescriptors) {
        this.descriptors = pDescriptors;
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.metrics.IMetricDescriptor#getName()
     */
    @Override
    public String getName() {
        return this.filePath.toFile().getName();
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.rules.IRuleDescriptor#getValue()
     */
    @Override
    public Integer getValue() {
        int sum = 0;
        for (final FunctionDescriptor fd : this.descriptors) {
            sum += fd.getValue().intValue();
        }
        return Integer.valueOf(sum);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(final Object object) {
        boolean isEqual;
        if (object instanceof FileRuleDescriptor) {
            isEqual = this.filePath.equals(((FileRuleDescriptor) object).getFilePath());
        } else {
            isEqual = false;
        }
        return isEqual;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        assert false : "hashCode not designed";
        return this.descriptors.size();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public FileRuleDescriptor clone() throws CloneNotSupportedException {
        final FileRuleDescriptor clone = (FileRuleDescriptor) super.clone();
        clone.setFilePath(this.filePath);
        clone.setDescriptors(new LinkedList<FunctionDescriptor>(this.descriptors));
        return clone;
    }
}
