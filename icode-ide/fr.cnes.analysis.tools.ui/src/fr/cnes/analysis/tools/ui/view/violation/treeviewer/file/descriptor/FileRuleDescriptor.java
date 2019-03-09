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
    /** Class name **/
    private static final String CLASS = FileRuleDescriptor.class.getName();

    /** File's path. **/
    private IPath filePath;
    /** List of violations in the file. **/
    private List<FunctionDescriptor> descriptors;

    /**
     * Empty constructor.
     */
    public FileRuleDescriptor() {
        final String method = "FileRuleDescriptor";
        ICodeLogger.entering(CLASS, method);
        this.descriptors = new LinkedList<FunctionDescriptor>();
        ICodeLogger.exiting(CLASS, method);

    }

    /**
     * Constructor with the file's path.
     * 
     * @param pFilePath
     *            the file's path.
     */
    public FileRuleDescriptor(final IPath pFilePath) {
        final String method = "FileRuleDescriptor";
        ICodeLogger.entering(CLASS, method, pFilePath);
        this.filePath = pFilePath;
        this.descriptors = new LinkedList<FunctionDescriptor>();
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Getter for the file's path.
     * 
     * @return file's path
     */
    public IPath getFilePath() {
        final String method = "getFilePath";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, this.filePath);
        return this.filePath;
    }

    /**
     * Getter for the descriptors.
     * 
     * @return the descriptors
     */
    public List<FunctionDescriptor> getDescriptors() {
        final String method = "getDescriptors";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, this.descriptors);
        return this.descriptors;
    }

    /**
     * Setter for the file's path.
     * 
     * @param pFilePath
     *            the file's path to set
     */
    public void setFilePath(final IPath pFilePath) {
        final String method = "setFilePath";
        ICodeLogger.entering(CLASS, method, pFilePath);
        this.filePath = pFilePath;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Setter for the descriptors.
     * 
     * @param pDescriptors
     *            the descriptors to set
     */
    public void setDescriptors(final List<FunctionDescriptor> pDescriptors) {
        final String method = "setDescriptors";
        ICodeLogger.entering(CLASS, method, pDescriptors);
        this.descriptors = pDescriptors;
        ICodeLogger.exiting(CLASS, method);
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.metrics.IMetricDescriptor#getName()
     */
    @Override
    public String getName() {
        final String method = "getName";
        ICodeLogger.entering(CLASS, method);
        final String name = this.filePath.toFile().getName();
        ICodeLogger.exiting(CLASS, method, name);
        return name;
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.ui.view.rules.IRuleDescriptor#getValue()
     */
    @Override
    public Integer getValue() {
        final String method = "getValue";
        ICodeLogger.entering(CLASS, method);
        int sum = 0;
        for (final FunctionDescriptor fd : this.descriptors) {
            sum += fd.getValue().intValue();
        }
        ICodeLogger.exiting(CLASS, method, Integer.valueOf(sum));
        return Integer.valueOf(sum);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(final Object object) {
        final String method = "equals";
        ICodeLogger.entering(CLASS, method, object);
        final boolean isEqual;
        if (object instanceof FileRuleDescriptor) {
            isEqual = this.filePath.equals(((FileRuleDescriptor) object).getFilePath());
        } else {
            isEqual = false;
        }
        ICodeLogger.exiting(CLASS, method, Boolean.valueOf(isEqual));
        return isEqual;
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
        assert false : "hashCode not designed";
        ICodeLogger.exiting(CLASS, method);
        return this.descriptors.size();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public FileRuleDescriptor clone() throws CloneNotSupportedException {
        final String method = "clone";
        ICodeLogger.entering(CLASS, method);
        final FileRuleDescriptor clone = (FileRuleDescriptor) super.clone();
        clone.setFilePath(this.filePath);
        clone.setDescriptors(new LinkedList<FunctionDescriptor>(this.descriptors));
        ICodeLogger.exiting(CLASS, method, clone);
        return clone;
    }
}
