/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.violation.treeviewer.file;

import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor.FileRuleDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor.FunctionDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor.RuleDescriptor;
import fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.descriptor.ViolationDescriptor;

/**
 * Thread job converting all {@link Violation} in {@link #inputs} into a
 * {@link #container} of {@link FileRuleDescriptor}. </br>
 * 
 * The job consist in verifying different attributes of the {@link Violation}
 * and the {@link #container}, creating a new : </br>
 * <ul>
 * <li>{@link FileRuleDescriptor} for {@link Violation#getFile()} not in the
 * container.</li>
 * <li>{@link FunctionDescriptor} for {@link Violation#getLocation()} not in the
 * container, and associating it into it's related descriptors.</li>
 * <li>{@link RuleDescriptor} for {@link Violation#getRuleName()} not in the
 * container,and associating it into it's related {@link FunctionDescriptor}'s
 * descriptors.</li>
 * <li>{@link ViolationDescriptor} for each violation and putting it into the
 * right {@link RuleDescriptor}.
 * </ul>
 * 
 * @see Job
 * @see fr.cnes.analysis.tools.ui.view.violation.treeviewer.file.FileTreeViewer
 * 
 * @since 2.0
 * @version 2.1
 */
public class ViolationToFileTreeViewerConverter extends Job {
    /** Logger. **/
    public final static Logger LOGGER = Logger
            .getLogger(ViolationToFileTreeViewerConverter.class.getName());

    /** The original inputs. **/
    private Violation[] inputs;
    /** A value container which has all values of rules. **/
    private FileRuleDescriptor[] container;

    /**
     * Empty constructor for this Job.
     */
    public ViolationToFileTreeViewerConverter() {
        super("Converting results...");
        this.inputs = new Violation[0];
        this.container = new FileRuleDescriptor[0];
    }

    /**
     * Constructor for this Job with an array of violations.
     * 
     * @param pInputs
     *            the inputs
     */
    public ViolationToFileTreeViewerConverter(final Violation[] pInputs) {
        super("Converting results...");
        this.inputs = pInputs.clone();
        this.container = new FileRuleDescriptor[0];
    }

    /**
     * Getter for the inputs id.
     * 
     * @return the inputs
     */
    public Violation[] getInputs() {
        return this.inputs.clone();
    }

    /**
     * Getter for the container
     * 
     * @return the container
     */
    public FileRuleDescriptor[] getContainer() {
        return this.container.clone();
    }

    /**
     * Setter for the inputs.
     * 
     * @param pInputs
     *            the inputs to set
     */
    public void setInputs(final Violation[] pInputs) {
        this.inputs = pInputs.clone();
    }

    /**
     * Setter for the container
     * 
     * @param pContainer
     *            the container to set
     */
    public void setContainer(final FileRuleDescriptor[] pContainer) {
        this.container = pContainer.clone();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    public IStatus run(final IProgressMonitor monitor) {
        LOGGER.finest("Begin run method");
        // Instantiate return variable
        IStatus status = Status.OK_STATUS;
        final int totalWork = this.inputs.length;

        // Instantiate descriptors
        final List<FileRuleDescriptor> descriptors = new LinkedList<>();

        FileRuleDescriptor file = new FileRuleDescriptor();
        FunctionDescriptor function = new FunctionDescriptor();
        RuleDescriptor rule = new RuleDescriptor();
        ViolationDescriptor viold = new ViolationDescriptor();

        // Start converting
        monitor.beginTask("Converting...", totalWork);
        try {
            /*
             * For every violation contained in the TreeViewer inputs. We add
             * the violation to the descriptor list verifying that, for each
             * violation : -> If there is already one about the concerned file,
             * to add only information inside the FileRuleDescriptor -> Same for
             * element inside of the rule as FunctionDescriptor and
             * RuleDescriptor. -> If it's not included in the descriptor, then
             * to create required new descriptors.
             */
            for (final Violation value : this.inputs) {
                file = new FileRuleDescriptor( new Path(value.getFile().getAbsolutePath()));

                if (descriptors.contains(file)) {
                    file = descriptors.get((descriptors.indexOf(file)));
                    function = new FunctionDescriptor(value.getLocation(), -1,  new Path(value.getFile().getAbsolutePath()));
                    if (file.getDescriptors().contains(function)) {
                        function = file.getDescriptors()
                                .get(file.getDescriptors().indexOf(function));
                        rule = new RuleDescriptor(value.getRuleId(), value.getRuleName(),
                                value.getLocation(), -1,  new Path(value.getFile().getAbsolutePath()));
                        if (function.getDescriptors().contains(rule)) {
                            rule = function.getDescriptors()
                                    .get(function.getDescriptors().indexOf(rule));
                            viold = new ViolationDescriptor(value.getRuleName(),
                                    value.getLocation(), value.getMessage(), value.getLine(),
                                     new Path(value.getFile().getAbsolutePath()));
                            if (rule.getDescriptors().contains(viold)) {
                                /*
                                 * This shouldn't happen, this mean the
                                 * violation was recorded two times.
                                 */
                            } else {
                                rule.getDescriptors().add(viold.clone());
                            }
                        } else {
                            viold = new ViolationDescriptor(value.getRuleName(),
                                    value.getLocation(), value.getMessage(), value.getLine(),
                                     new Path(value.getFile().getAbsolutePath()));
                            rule.getDescriptors().add(viold.clone());
                            function.getDescriptors().add(rule.clone());
                        }
                    } else {
                        rule = new RuleDescriptor(value.getRuleId(), value.getRuleName(),
                                value.getLocation(), -1,  new Path(value.getFile().getAbsolutePath()));
                        viold = new ViolationDescriptor(value.getRuleName(), value.getLocation(),
                                value.getMessage(), value.getLine(),  new Path(value.getFile().getAbsolutePath()));
                        rule.getDescriptors().add(viold.clone());
                        function.getDescriptors().add(rule.clone());
                        file.getDescriptors().add(function.clone());
                    }
                } else {
                    rule = new RuleDescriptor(value.getRuleId(), value.getRuleName(),
                            value.getLocation(), -1,  new Path(value.getFile().getAbsolutePath()));
                    viold = new ViolationDescriptor(value.getRuleName(), value.getLocation(),
                            value.getMessage(), value.getLine(),  new Path(value.getFile().getAbsolutePath()));
                    function = new FunctionDescriptor(value.getLocation(), -1, new Path(value.getFile().getAbsolutePath()));
                    rule.getDescriptors().add(viold.clone());
                    function.getDescriptors().add(rule.clone());
                    file.getDescriptors().add(function.clone());
                    descriptors.add(file.clone());
                }

            }
            this.container = descriptors.toArray(new FileRuleDescriptor[descriptors.size()]);

        } catch (final CloneNotSupportedException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            status = new Status(Status.ERROR, "fr.cnes.analysis.tools.ui", Status.ERROR,
                    exception.getMessage(), exception);
        }

        LOGGER.finest("End run method");
        return status;
    }
}
