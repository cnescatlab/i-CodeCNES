/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.utils;

import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.IStructuredSelection;

import fr.cnes.analysis.tools.ui.exception.EmptyResourceException;
import fr.cnes.analysis.tools.ui.exception.EmptySelectionException;
import fr.cnes.analysis.tools.ui.exception.InvalidResourceTypeException;
import fr.cnes.analysis.tools.ui.exception.NonAccessibleResourceException;
import fr.cnes.analysis.tools.ui.exception.UnknownResourceTypeException;

/**
 * Contains tools methods for handler purpose.
 */
public final class AnalysisHandlerUIUtils {
    /** Logger **/
    private final static Logger LOGGER = Logger.getLogger(AnalysisHandlerUIUtils.class.getName());

    /**
     * Protected constructor. This method should not be instantiated.
     */
    private AnalysisHandlerUIUtils() {
        // do nothing
    }

    /**
     * Check if the selection is not empty.
     * 
     * @param selection
     *            the current selection.
     * @throws EmptySelectionException
     *             if the selection is empty.
     */
    public static void checkSelection(final IStructuredSelection selection)
                throws EmptySelectionException {
        LOGGER.finest("Begin checkSelection method");

        if (selection == null) {
            throw new EmptySelectionException("Selection is null.");
        } else if (selection.isEmpty()) {
            throw new EmptySelectionException("Selection is empty.");
        } else if (selection.getFirstElement() == null) {
            throw new EmptySelectionException("Selection element is null.");
        }

        LOGGER.finest("End checkSelection method");
    }

    /**
     * Retrieve the files on the corresponding language from the selection.
     * 
     * @param selection
     *            the selection
     * @param pFileExtension
     *            file's possible extensions
     * @return the retrieved files.
     * @throws EmptyResourceException
     *             if the resource is empty.
     * @throws UnknownResourceTypeException
     *             if the resource type is unknown.
     * @throws InvalidResourceTypeException
     *             if the resource type is invalid.
     * @throws NonAccessibleResourceException
     *             if the resource is not accessible.
     * @throws EmptySelectionException
     *             if the selection is empty.
     */
    public static List<IPath> retrieveFiles(final IStructuredSelection selection,
                final String[] pFileExtension) throws EmptyResourceException,
                            UnknownResourceTypeException, InvalidResourceTypeException,
                            NonAccessibleResourceException, EmptySelectionException {
        LOGGER.finest("Begin retrieveFiles method");

        final List<IPath> files = browseSelection(selection, pFileExtension);
        if (files.isEmpty()) {
            throw new EmptyResourceException(
                        "Please select at least one file before launching the analyse.");
        }

        LOGGER.finest("End retrieveFiles method");
        return files;
    }

    /**
     * Retrieve the selected files for all files in a selection.
     * 
     * @param selection
     *            The current selection
     * @param fileExtension
     *            list of file's possible extensions
     * @return The selected files list
     * @throws EmptyResourceException
     *             A project or a folder has no members.
     * @throws UnknownResourceTypeException
     *             Resource has unknown type
     * @throws InvalidResourceTypeException
     *             Resource has invalid type (a project's member or a folder's
     *             member cannot have ROOT type)
     * @throws NonAccessibleResourceException
     *             Resource is not accessible
     * @throws EmptySelectionException
     *             Root has no projects
     */
    public static List<IPath> browseSelection(final IStructuredSelection selection,
                final String[] fileExtension) throws EmptyResourceException,
                            UnknownResourceTypeException, InvalidResourceTypeException,
                            NonAccessibleResourceException, EmptySelectionException {
        LOGGER.finest("Begin browseSelection method");

        final List<IPath> files = new LinkedList<IPath>();

        for (int i = 0; i < selection.size(); i++) {
            if (selection.toList().get(i) instanceof IResource) {
                files.addAll(retrieveSelectedFiles((IResource) selection.toList().get(i),
                            fileExtension));
            }
        }

        LOGGER.finest("End browseSelection method");
        return files;
    }

    /**
     * Retrieve the selected files.
     * 
     * @param resource
     *            The input resource.
     * @param fileExtension
     *            list of file's possible extensions
     * @return The selected files list.
     * @throws EmptyResourceException
     *             A project or a folder has no members.
     * @throws UnknownResourceTypeException
     *             Resource has unknown type.
     * @throws InvalidResourceTypeException
     *             Resource has invalid type (a project's member or a folder's
     *             member cannot have ROOT type).
     * @throws NonAccessibleResourceException
     *             Resource is not accessible.
     * @throws EmptySelectionException
     *             Root has no projects.
     */
    public static List<IPath> retrieveSelectedFiles(final IResource resource,
                final String[] fileExtension) throws EmptyResourceException,
                            UnknownResourceTypeException, InvalidResourceTypeException,
                            NonAccessibleResourceException, EmptySelectionException {
        LOGGER.finest("Begin retrieveSelectedFiles method");

        final List<IPath> files = new LinkedList<IPath>();

        switch (resource.getType()) {
        case IResource.ROOT:
            files.addAll(retrieveFilesFromRoot((IWorkspaceRoot) resource, fileExtension));
            break;
        case IResource.PROJECT:
            files.addAll(retrieveFilesFromProject((IProject) resource, fileExtension));
            break;
        case IResource.FOLDER:
            files.addAll(retrieveFilesFromFolder((IFolder) resource, fileExtension));
            break;
        case IResource.FILE:
            if (checkFileExtension(resource, fileExtension)) {
                files.add(resource.getLocation());
            }
            break;
        default:
            final UnknownResourceTypeException exception = new UnknownResourceTypeException(
                        "Resource " + resource.getName() + " has type " + resource.getType() + ".");
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                        exception);
            throw exception;
        }

        LOGGER.finest("End retrieveSelectedFiles method");
        return files;
    }

    /**
     * Retrieve selected files from Root.
     * 
     * @param resource
     *            the selection, as workspace root.
     * @param fileExtension
     *            list of file's possible extensions
     * @return all the files from root.
     * @throws EmptyResourceException
     *             A project or a folder has no members.
     * @throws UnknownResourceTypeException
     *             Resource has unknown type.
     * @throws InvalidResourceTypeException
     *             Resource has invalid type (a project's member or a folder's
     *             member cannot have ROOT type).
     * @throws NonAccessibleResourceException
     *             Resource is not accessible.
     * @throws EmptySelectionException
     *             Root has not projects.
     */
    public static List<IPath> retrieveFilesFromRoot(final IWorkspaceRoot resource,
                final String[] fileExtension) throws EmptyResourceException,
                            UnknownResourceTypeException, InvalidResourceTypeException,
                            NonAccessibleResourceException, EmptySelectionException {
        LOGGER.finest("Begin retrieveFilesFromRoot method");

        final List<IPath> files = new LinkedList<IPath>();
        final IProject[] projects = resource.getProjects();
        if (projects == null) {
            throw new EmptySelectionException("Selected root has no projects.");
        } else {
            for (final IProject project : projects) {
                files.addAll(retrieveFilesFromProject(project, fileExtension));
            }
        }

        LOGGER.finest("End retrieveFilesFromRoot method");
        return files;
    }

    /**
     * Retrieve all project's files.
     * 
     * @param project
     *            the current project.
     * @param fileExtension
     *            list of file's possible extensions
     * @return The project's files.
     * @throws EmptyResourceException
     *             A project or a folder has no members.
     * @throws UnknownResourceTypeException
     *             Resource has unknown type.
     * @throws InvalidResourceTypeException
     *             Resource has invalid type (a project's member or a folder's
     *             member cannot have ROOT type).
     * @throws NonAccessibleResourceException
     *             Resource is not accessible.
     */
    public static List<IPath> retrieveFilesFromProject(final IProject project,
                final String[] fileExtension)
                            throws EmptyResourceException, UnknownResourceTypeException,
                            InvalidResourceTypeException, NonAccessibleResourceException {
        LOGGER.finest("Begin retrieveFilesFromProject method");

        final List<IPath> files = new LinkedList<IPath>();
        if (project.isAccessible()) {
            try {
                final IResource[] members = project.members();
                if (members == null) {
                    throw new EmptyResourceException("Project " + project.getName() + " is empty.");
                } else {
                    for (final IResource member : members) {
                        switch (member.getType()) {
                        case IResource.PROJECT:
                            files.addAll(
                                        retrieveFilesFromProject((IProject) member, fileExtension));
                            break;
                        case IResource.FOLDER:
                            files.addAll(retrieveFilesFromFolder((IFolder) member, fileExtension));
                            break;
                        case IResource.FILE:
                            if (checkFileExtension(member, fileExtension)) {
                                files.add(member.getLocation());
                            }
                            break;
                        case IResource.ROOT:
                            final InvalidResourceTypeException invalidException = new InvalidResourceTypeException(
                                        "Resource " + member.getName() + " has type "
                                                    + member.getType() + ".");
                            LOGGER.log(Level.FINER,
                                        invalidException.getClass() + " : "
                                                    + invalidException.getMessage(),
                                        invalidException);
                            throw invalidException;
                        default:
                            final UnknownResourceTypeException unknownException = new UnknownResourceTypeException(
                                        "Resource " + member.getName() + " has type "
                                                    + member.getType() + ".");
                            LOGGER.log(Level.FINER,
                                        unknownException.getClass() + " : "
                                                    + unknownException.getMessage(),
                                        unknownException);
                            throw unknownException;
                        }
                    }
                }
            } catch (final CoreException e) {
                throw new NonAccessibleResourceException(
                            "Project " + project.getName() + " is not accessible or is closed.", e);
            }
        } else {
            throw new NonAccessibleResourceException(
                        "Project " + project.getName() + " is not accessible or is closed.");
        }

        LOGGER.finest("End retrieveFilesFromProject method");
        return files;
    }

    /**
     * Retrieve all folder's files.
     * 
     * @param folder
     *            the current folder.
     * @param fileExtension
     *            list of file's possible extensions
     * @return The folder's files.
     * @throws EmptyResourceException
     *             A project or a folder has no members.
     * @throws UnknownResourceTypeException
     *             Resource has unknown type.
     * @throws InvalidResourceTypeException
     *             Resource has invalid type (a project's member or a folder's
     *             member cannot have ROOT type).
     * @throws NonAccessibleResourceException
     *             Resource is not accessible.
     */
    public static List<IPath> retrieveFilesFromFolder(final IFolder folder,
                final String[] fileExtension)
                            throws EmptyResourceException, UnknownResourceTypeException,
                            InvalidResourceTypeException, NonAccessibleResourceException {
        LOGGER.finest("Begin retrieveFilesFromFolder method");

        try {
            final List<IPath> files = new LinkedList<IPath>();
            final IResource[] members = folder.members();
            if (members == null) {
                throw new EmptyResourceException("Folder " + folder.getName() + " is empty.");
            } else {
                for (final IResource member : members) {
                    switch (member.getType()) {
                    case IResource.PROJECT:
                        files.addAll(retrieveFilesFromProject((IProject) member, fileExtension));
                        break;
                    case IResource.FOLDER:
                        files.addAll(retrieveFilesFromFolder((IFolder) member, fileExtension));
                        break;
                    case IResource.FILE:
                        if (checkFileExtension(member, fileExtension)) {
                            files.add(member.getLocation());
                        }
                        break;
                    case IResource.ROOT:
                        final InvalidResourceTypeException exception = new InvalidResourceTypeException(
                                    "Resource " + member.getName() + " has type " + member.getType()
                                                + ".");
                        LOGGER.log(Level.FINER,
                                    exception.getClass() + " : " + exception.getMessage(),
                                    exception);
                        throw exception;
                    default:
                        final UnknownResourceTypeException unknownException = new UnknownResourceTypeException(
                                    "Resource " + member.getName() + " has type " + member.getType()
                                                + ".");
                        LOGGER.log(Level.FINER, unknownException.getClass() + " : "
                                    + unknownException.getMessage(), unknownException);
                        throw unknownException;
                    }
                }
            }

            LOGGER.finest("End retrieveFilesFromFolder method");
            return files;
        } catch (final CoreException e) {
            throw new NonAccessibleResourceException(
                        "Folder " + folder.getName() + " does not exist.", e);
        }
    }

    /**
     * Check file extension.
     * 
     * @param file
     *            the file to be checked.
     * @param fileExtension
     *            list of file's possible extensions
     * @return true if the file extension is f, f77 (non case sensitive).
     */
    public static boolean checkFileExtension(final IResource file, final String[] fileExtension) {
        LOGGER.finest("Begin checkFileExtension method");

        boolean rightExtension = false;
        final boolean isRightFile = (file.getName() != null) && !(file.getName().startsWith("."));
        if (isRightFile && (file.getFileExtension() != null)) {
            for (final String extension : fileExtension) {
                rightExtension = rightExtension
                            | extension.equalsIgnoreCase(file.getFileExtension());
            }
        }

        LOGGER.finest("End checkFileExtension method");
        return rightExtension;

    }
}
