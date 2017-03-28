/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.handler;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.analyzer.exception.NullContributionException;
import fr.cnes.analysis.tools.ui.exception.EmptyResourceException;
import fr.cnes.analysis.tools.ui.exception.EmptySelectionException;
import fr.cnes.analysis.tools.ui.exception.InvalidResourceTypeException;
import fr.cnes.analysis.tools.ui.exception.NonAccessibleResourceException;
import fr.cnes.analysis.tools.ui.exception.UnknownResourceTypeException;
import fr.cnes.analysis.tools.ui.utils.AnalysisHandlerUIUtils;

/**
 * This class is the Handler that linked together action button on the tool bar
 * and analyze function
 */
public abstract class AbstractAnalysisHandler extends AbstractHandler
        implements IExecutableExtension {
    /** Logger. **/
    private static final Logger LOGGER = Logger.getLogger(AbstractAnalysisHandler.class.getName());

    /** Store the data of this IExecutableExtension **/
    private String[] fileExtension;

    /** Store the extension id for analyzer **/
    private String analyzerId;

    /** Store the files to analyzed **/
    private List<IPath> files;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org
     * .eclipse.core.runtime.IConfigurationElement, java.lang.String,
     * java.lang.Object)
     */
    @Override
    public void setInitializationData(final IConfigurationElement config, final String propertyName,
            final Object data) throws CoreException {
        if (data instanceof Map) {
            this.analyzerId = ((Map<?, ?>) data).get("extensionId").toString();
            final Collection<?> dataList = ((Map<?, ?>) data).values();
            dataList.remove(this.analyzerId);
            this.fileExtension = dataList.toArray(new String[dataList.size()]);
        } else {
            throw new CoreException(new Status(IStatus.ERROR, PlatformUI.PLUGIN_ID, 0,
                    "Data argument must be a Hashtable for " + this.getClass(), null));
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.
     * commands .ExecutionEvent)
     */
    @Override
    public Object execute(final ExecutionEvent event) {
        try {
            LOGGER.finest("Begin execute method");

            // retrieve the active selection in the package explorer
            final IStructuredSelection selection = (IStructuredSelection) PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getSelectionService().getSelection();

            // check selection
            AnalysisHandlerUIUtils.checkSelection(selection);

            // retrieve the file(s) of the selected language
            setFiles(AnalysisHandlerUIUtils.retrieveFiles(selection, fileExtension));

            // run the analysis on the retrieved files
            this.runAnalysis(getFiles(), this.analyzerId);

            LOGGER.finest("End execute method");
        } catch (final EmptySelectionException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Empty Selection",
                    "Please, select a file, a folder or a project before launching the analyse.");
        } catch (final EmptyResourceException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openWarning(
                    PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Empty Resource", exception.getMessage());
        } catch (final NonAccessibleResourceException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openWarning(
                    PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Accessibility Error", exception.getMessage());
        } catch (final InvalidResourceTypeException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Internal Error", "Contact support service : \n" + exception.getMessage());
        } catch (final UnknownResourceTypeException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Internal Error", "Contact support service : \n" + exception.getMessage());
        } catch (final CoreException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Internal Error", "Contact support service : \n" + exception.getMessage());
        } catch (final NullContributionException exception) {
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Internal Error", "Contact support service : \n" + exception.getMessage());
        }
        return null;
    }

    /**
     * Run the analysis on the retrieved files.
     * 
     * @param pFiles
     *            the files to analyze
     * @param pAnalyzerID
     *            the id of analyzer on which the analysis is made
     * @throws CoreException
     *             internal error
     * @throws NullContributionException
     *             when no analyzer correspond to analyzer's id
     */
    protected abstract void runAnalysis(final List<IPath> pFiles, final String pAnalyzerID)
            throws CoreException, NullContributionException;

    /**
     * @return files The files to analyze
     */
    public List<IPath> getFiles() {
        return files;
    }

    /**
     * @param pFiles
     *            The files to analyze
     */
    public void setFiles(List<IPath> pFiles) {
        this.files = pFiles;
    }

    /**
     * Getter for file extensions.
     * 
     * @return the list of file extensions allowed
     */
    public String[] getFileExtension() {
        return this.fileExtension.clone();
    }

    /**
     * Setter for list of file extension.
     * 
     * @param pFileExtension
     *            list of file extensions to set
     */
    public void setFileExtension(final String[] pFileExtension) {
        this.fileExtension = pFileExtension.clone();
    }

    /**
     * Setter for analyzer id.
     * 
     * @param pAnalyzerId
     *            the analyzer id to set
     */
    public void setAnalyzerId(final String pAnalyzerId) {
        this.analyzerId = pAnalyzerId;
    }
}
