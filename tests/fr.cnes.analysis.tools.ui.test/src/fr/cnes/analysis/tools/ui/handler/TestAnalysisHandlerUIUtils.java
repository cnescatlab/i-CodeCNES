/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.handler;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.junit.Test;

import fr.cnes.analysis.tools.ui.exception.EmptyResourceException;
import fr.cnes.analysis.tools.ui.exception.EmptySelectionException;
import fr.cnes.analysis.tools.ui.exception.InvalidResourceTypeException;
import fr.cnes.analysis.tools.ui.exception.NonAccessibleResourceException;
import fr.cnes.analysis.tools.ui.exception.UnknownResourceTypeException;
import fr.cnes.analysis.tools.ui.utils.AnalysisHandlerUIUtils;

/**
 * This class is used to test
 * 
 */
public class TestAnalysisHandlerUIUtils {

	/** Message for a SecurityException fail. **/
	public final static String SECURITY_FAIL = "Erreur d'ex�cution (SecurityException).";
	/** Message for a NoSuchMethodException fail. **/
	public final static String METHOD_FAIL = "Erreur d'ex�cution (NoSuchMethodException).";
	/** Message for a IllegalArgumentException fail. **/
	public final static String ARGUMENT_FAIL = "Erreur d'ex�cution (IllegalArgumentException).";
	/** Message for a IllegalAccessException fail. **/
	public final static String ACCESS_FAIL = "Erreur d'ex�cution (IllegalAccessException).";
	/** Message for a InvocationTargetException fail. **/
	public final static String TARGET_FAIL = "Erreur d'ex�cution (InvocationTargetException).";
	/** Message for a CoreException fail. **/
	public final static String CORE_FAIL = "Erreur d'ex�cution (CoreException).";

	/** Message for a EmptyResourceException fail. **/
	public final static String EMPTY_RES_FAIL = "Erreur d'ex�cution (EmptyResourceException).";
	/** Message for a InvalidResourceTypeException fail. **/
	public final static String INVALID_FAIL = "Erreur d'ex�cution (InvalidResourceTypeException).";
	/** Message for a UnknownResourceTypeException fail. **/
	public final static String UNKNOWN_FAIL = "Erreur d'ex�cution (UnknownResourceTypeException).";
	/** Message for a EmptySelectionException fail. **/
	public final static String EMPTY_SEL_FAIL = "Erreur d'ex�cution (EmptySelectionException).";
	/** Message for a NonAccessibleResourceException fail. **/
	public final static String ACCES_FAIL = "Erreur d'ex�cution (NonAccessibleResourceException).";

	/** Name for retrieve method with a file. **/
	public final static String[] FILE_EXTENSION = { "f" };
	/** Name of the file to test. **/
	public final static String NAME = "test";

	/** Assert message when path is empty and should not. **/
	public final static String EMPTY_PATH_MSG = "Path is empty";
	/** Assert message when path is not empty and should. **/
	public final static String NOT_EMP_PATH_MSG = "Path is not empty";
	/** Assert message when exception is thrown but not the right one. **/
	public final static String WRG_EXCP_MSG = "Wrong exception";

	// checkSelection(IStructuredSelection)
	// --------------------------------------

	/**
	 * Check that no error is thrown with a normal execution of checkSelection
	 * method.
	 */
	@Test
	public void testCheckSelection() {

		try {
			final IStructuredSelection selection = mock(IStructuredSelection.class);

			when(selection.isEmpty()).thenReturn(false);
			when(selection.getFirstElement()).thenReturn(1);

			AnalysisHandlerUIUtils.checkSelection(selection);
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final EmptySelectionException exception) {
			fail("Erreur d'ex�cution (EmptySelectionException).");
		}
	}

	/**
	 * Test right behavior of checkSelection with null first element.
	 * 
	 * @throws EmptySelectionException
	 *             when no good selection is made
	 */
	@Test(expected = EmptySelectionException.class)
	public void testCheckSelectionWithNullFirstElement() throws EmptySelectionException {

		try {
			final IStructuredSelection selection = mock(IStructuredSelection.class);

			when(selection.isEmpty()).thenReturn(false);
			when(selection.getFirstElement()).thenReturn(null);

			AnalysisHandlerUIUtils.checkSelection(selection);
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		}
	}

	/**
	 * Test right behavior of checkSelection with null first element.
	 * 
	 * @throws EmptySelectionException
	 *             when no good selection is made
	 */
	@Test(expected = EmptySelectionException.class)
	public void testCheckSelectionWithEmptySelection() throws EmptySelectionException {

		try {
			final IStructuredSelection selection = mock(IStructuredSelection.class);

			when(selection.isEmpty()).thenReturn(true);

			AnalysisHandlerUIUtils.checkSelection(selection);
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with root) returns a not empty list in the
	 * right conditions.
	 */
	@Test
	public void testRetrieveSelected() {

		try {
			final IWorkspaceRoot root = mock(IWorkspaceRoot.class);
			final IProject project = mock(IProject.class);
			final IProject[] projects = { project };
			final IFile file = mock(IFile.class);
			final IResource[] resources = { file };

			when(root.getProjects()).thenReturn(projects);
			when(project.isAccessible()).thenReturn(true);
			when(project.members()).thenReturn(resources);
			when(file.getType()).thenReturn(IResource.FILE);
			when(file.getFileExtension()).thenReturn("f");
			when(file.getLocation()).thenReturn(new Path(NAME));
			when(file.getName()).thenReturn(NAME);

			final List<IPath> paths = AnalysisHandlerUIUtils.retrieveFilesFromRoot(root, FILE_EXTENSION);

			assertFalse(EMPTY_PATH_MSG, paths.isEmpty());
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final EmptyResourceException exception) {
			fail(EMPTY_RES_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		} catch (final EmptySelectionException exception) {
			fail(EMPTY_SEL_FAIL);
		} catch (final CoreException exception) {
			fail(CORE_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with root) returns an empty list in the
	 * right conditions.
	 */
	@Test
	public void testRetrieveSelectedWithNoFortranFile() {

		try {
			final IWorkspaceRoot root = mock(IWorkspaceRoot.class);
			final IProject project = mock(IProject.class);
			final IProject[] projects = { project };
			final IFile file = mock(IFile.class);
			final IResource[] resources = { file };

			when(root.getProjects()).thenReturn(projects);
			when(project.isAccessible()).thenReturn(true);
			when(project.members()).thenReturn(resources);
			when(file.getType()).thenReturn(IResource.FILE);
			when(file.getFileExtension()).thenReturn("c");
			when(file.getLocation()).thenReturn(new Path(NAME));

			final List<IPath> paths = AnalysisHandlerUIUtils.retrieveFilesFromRoot(root, FILE_EXTENSION);

			assertTrue(NOT_EMP_PATH_MSG, paths.isEmpty());
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final EmptyResourceException exception) {
			fail(EMPTY_RES_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		} catch (final EmptySelectionException exception) {
			fail(EMPTY_SEL_FAIL);
		} catch (final CoreException exception) {
			fail(CORE_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with root) returns an error when there is
	 * no project.
	 * 
	 * @throws EmptyResourceException
	 *             when no resource if found
	 */
	@Test(expected = EmptyResourceException.class)
	public void testRetrieveSelectedWithNoProjectMembers() throws EmptyResourceException {

		try {
			final IWorkspaceRoot root = mock(IWorkspaceRoot.class);
			final IProject project = mock(IProject.class);
			final IProject[] projects = { project };

			when(root.getProjects()).thenReturn(projects);
			when(project.isAccessible()).thenReturn(true);
			when(project.members()).thenReturn(null);

			AnalysisHandlerUIUtils.retrieveFilesFromRoot(root, FILE_EXTENSION);
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		} catch (final EmptySelectionException exception) {
			fail(EMPTY_SEL_FAIL);
		} catch (final CoreException exception) {
			fail(CORE_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with root) returns an error when a project
	 * is not accessible.
	 * 
	 * @throws NonAccessibleResourceException
	 *             when a project is not accessible
	 */
	@Test(expected = NonAccessibleResourceException.class)
	public void testRetrieveSelectedWithUnaccessibleProject() throws NonAccessibleResourceException {

		try {
			new MetricAnalysisHandler();
			final IWorkspaceRoot root = mock(IWorkspaceRoot.class);
			final IProject project = mock(IProject.class);
			final IProject[] projects = { project };

			when(root.getProjects()).thenReturn(projects);
			when(project.isAccessible()).thenReturn(false);

			AnalysisHandlerUIUtils.retrieveFilesFromRoot(root, FILE_EXTENSION);
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final EmptyResourceException exception) {
			fail(EMPTY_RES_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		} catch (final EmptySelectionException exception) {
			fail(EMPTY_SEL_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with root) returns an error when there is
	 * no project.
	 * 
	 * @throws EmptySelectionException
	 *             when there is no selection
	 */
	@Test(expected = EmptySelectionException.class)
	public void testRetrieveSelectedWithNoProjects() throws EmptySelectionException {

		try {
			new MetricAnalysisHandler();
			final IWorkspaceRoot root = mock(IWorkspaceRoot.class);

			when(root.getProjects()).thenReturn(null);

			AnalysisHandlerUIUtils.retrieveFilesFromRoot(root, FILE_EXTENSION);
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final EmptyResourceException exception) {
			fail(EMPTY_RES_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with project) returns a not empty list in
	 * the right conditions.
	 */
	@Test
	public void testRetrieveFilesFromProject() {

		try {
			final IProject project = mock(IProject.class);
			final IFile file = mock(IFile.class);
			final IResource[] resources = { file };

			when(project.isAccessible()).thenReturn(true);
			when(project.members()).thenReturn(resources);
			when(file.getType()).thenReturn(IResource.FILE);
			when(file.getFileExtension()).thenReturn("f");
			when(file.getLocation()).thenReturn(new Path(NAME));
			when(file.getName()).thenReturn(NAME);

			final List<IPath> paths = AnalysisHandlerUIUtils.retrieveFilesFromProject(project, FILE_EXTENSION);

			assertFalse(EMPTY_PATH_MSG, paths.isEmpty());
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final EmptyResourceException exception) {
			fail(EMPTY_RES_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		} catch (final CoreException exception) {
			fail(CORE_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with project) returns a not empty list
	 * when a folder is given has entry.
	 */
	@Test
	public void testRetrieveFilesFromProjectWithIFolder() {

		try {
			final IProject project = mock(IProject.class);
			final IFolder folder = mock(IFolder.class);
			final IFile file = mock(IFile.class);
			final IResource[] resources = { folder };
			final IResource[] members = { file };

			when(project.isAccessible()).thenReturn(true);
			when(project.members()).thenReturn(resources);
			when(folder.getType()).thenReturn(IResource.FOLDER);
			when(folder.members()).thenReturn(members);
			when(file.getType()).thenReturn(IResource.FILE);
			when(file.getFileExtension()).thenReturn("f");
			when(file.getLocation()).thenReturn(new Path(NAME));
			when(file.getName()).thenReturn(NAME);

			final List<IPath> paths = AnalysisHandlerUIUtils.retrieveFilesFromProject(project, FILE_EXTENSION);

			assertFalse(EMPTY_PATH_MSG, paths.isEmpty());
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final EmptyResourceException exception) {
			fail(EMPTY_RES_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		} catch (final CoreException exception) {
			fail(CORE_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with project) returns an empty list when a
	 * project with no fortran file is given has entry.
	 */
	@Test
	public void testRetrieveFilesFromProjectWithIProject() {

		try {
			final IProject project = mock(IProject.class);
			final IProject project2 = mock(IProject.class);
			final IFile file = mock(IFile.class);
			final IResource[] resources = { project2 };
			final IResource[] members = { file };

			when(project.isAccessible()).thenReturn(true);
			when(project.members()).thenReturn(resources);
			when(project2.getType()).thenReturn(IResource.PROJECT);
			when(project2.members()).thenReturn(members);
			when(project2.isAccessible()).thenReturn(true);
			when(file.getType()).thenReturn(IResource.FILE);
			when(file.getFileExtension()).thenReturn("f");
			when(file.getLocation()).thenReturn(new Path(NAME));
			when(file.getName()).thenReturn(NAME);

			final List<IPath> paths = AnalysisHandlerUIUtils.retrieveFilesFromProject(project, FILE_EXTENSION);

			assertFalse(EMPTY_PATH_MSG, paths.isEmpty());
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final EmptyResourceException exception) {
			fail(EMPTY_RES_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		} catch (final CoreException exception) {
			fail(CORE_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with project) returns an
	 * InvalidResourceTypeException when a root is given has entry.
	 * 
	 * @throws InvalidResourceTypeException
	 *             when a root is in a project
	 */
	@Test(expected = InvalidResourceTypeException.class)
	public void testRetrieveFilesFromProjectWithIRoot() throws InvalidResourceTypeException {

		try {
			final IProject project = mock(IProject.class);
			final IWorkspaceRoot root = mock(IWorkspaceRoot.class);
			final IResource[] resources = { root };

			when(project.isAccessible()).thenReturn(true);
			when(project.members()).thenReturn(resources);
			when(root.getType()).thenReturn(IResource.ROOT);

			AnalysisHandlerUIUtils.retrieveFilesFromProject(project, FILE_EXTENSION);
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final EmptyResourceException exception) {
			fail(EMPTY_RES_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		} catch (final CoreException exception) {
			fail(CORE_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with project) returns an empty list when
	 * no fortran file is given.
	 */
	@Test
	public void testRetrieveFilesFromProjectWithNoFortranFile() {

		try {
			final IProject project = mock(IProject.class);
			final IFile file = mock(IFile.class);
			final IResource[] resources = { file };

			when(project.isAccessible()).thenReturn(true);
			when(project.members()).thenReturn(resources);
			when(file.getType()).thenReturn(IResource.FILE);
			when(file.getFileExtension()).thenReturn("c");
			when(file.getLocation()).thenReturn(new Path(NAME));
			when(file.getName()).thenReturn(NAME);

			final List<IPath> paths = AnalysisHandlerUIUtils.retrieveFilesFromProject(project, FILE_EXTENSION);

			assertTrue(NOT_EMP_PATH_MSG, paths.isEmpty());
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final EmptyResourceException exception) {
			fail(EMPTY_RES_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		} catch (final CoreException exception) {
			fail(CORE_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with project) returns an
	 * EmptyResourceException when project is empty.
	 * 
	 * @throws EmptyResourceException
	 *             when project is empty
	 */
	@Test(expected = EmptyResourceException.class)
	public void testRetrieveFilesFromProjectWithNoProjectMembers() throws EmptyResourceException {

		try {
			final IProject project = mock(IProject.class);

			when(project.isAccessible()).thenReturn(true);
			when(project.members()).thenReturn(null);

			AnalysisHandlerUIUtils.retrieveFilesFromProject(project, FILE_EXTENSION);
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		} catch (final CoreException exception) {
			fail(CORE_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with project) returns an
	 * NonAccessibleResourceException when project is unaccessible.
	 * 
	 * @throws NonAccessibleResourceException
	 *             when project is unaccessible
	 */
	@Test(expected = NonAccessibleResourceException.class)
	public void testRetrieveFilesFromProjectWithUnaccessibleProject() throws NonAccessibleResourceException {

		try {
			final IProject project = mock(IProject.class);

			when(project.isAccessible()).thenReturn(false);

			AnalysisHandlerUIUtils.retrieveFilesFromProject(project, FILE_EXTENSION);

		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final EmptyResourceException exception) {
			fail(EMPTY_RES_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with folder) returns a not empty list in
	 * the right conditions.
	 */
	@Test
	public void testRetrieveFilesFromFolder() {

		try {
			final IFolder folder = mock(IFolder.class);
			final IFile file = mock(IFile.class);
			final IResource[] resources = { file };

			when(folder.isAccessible()).thenReturn(true);
			when(folder.members()).thenReturn(resources);
			when(file.getType()).thenReturn(IResource.FILE);
			when(file.getFileExtension()).thenReturn("f");
			when(file.getLocation()).thenReturn(new Path(NAME));
			when(file.getName()).thenReturn(NAME);

			final List<IPath> paths = AnalysisHandlerUIUtils.retrieveFilesFromFolder(folder, FILE_EXTENSION);

			assertFalse(EMPTY_PATH_MSG, paths.isEmpty());
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final EmptyResourceException exception) {
			fail(EMPTY_RES_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		} catch (final CoreException exception) {
			fail(CORE_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with folder) returns a not empty list with
	 * a folder given has entry..
	 */
	@Test
	public void testRetrieveFilesFromFolderWithIFolder() {

		try {
			final IFolder folder = mock(IFolder.class);
			final IFolder folder2 = mock(IFolder.class);
			final IFile file = mock(IFile.class);
			final IResource[] resources = { folder2 };
			final IResource[] members = { file };

			when(folder.isAccessible()).thenReturn(true);
			when(folder.members()).thenReturn(resources);
			when(folder2.getType()).thenReturn(IResource.FOLDER);
			when(folder2.members()).thenReturn(members);
			when(file.getType()).thenReturn(IResource.FILE);
			when(file.getFileExtension()).thenReturn("f");
			when(file.getLocation()).thenReturn(new Path(NAME));
			when(file.getName()).thenReturn(NAME);

			final List<IPath> paths = AnalysisHandlerUIUtils.retrieveFilesFromFolder(folder, FILE_EXTENSION);

			assertFalse(EMPTY_PATH_MSG, paths.isEmpty());
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final EmptyResourceException exception) {
			fail(EMPTY_RES_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		} catch (final CoreException exception) {
			fail(CORE_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with folder) returns a not empty list with
	 * a project given has entry..
	 */
	@Test
	public void testRetrieveFilesFromFolderWithIProject() {

		try {
			final IFolder folder = mock(IFolder.class);
			final IProject project = mock(IProject.class);
			final IFile file = mock(IFile.class);
			final IResource[] resources = { project };
			final IResource[] members = { file };

			when(folder.isAccessible()).thenReturn(true);
			when(folder.members()).thenReturn(resources);
			when(project.getType()).thenReturn(IResource.PROJECT);
			when(project.members()).thenReturn(members);
			when(project.isAccessible()).thenReturn(true);
			when(file.getType()).thenReturn(IResource.FILE);
			when(file.getFileExtension()).thenReturn("f");
			when(file.getLocation()).thenReturn(new Path(NAME));
			when(file.getName()).thenReturn(NAME);

			final List<IPath> paths = AnalysisHandlerUIUtils.retrieveFilesFromFolder(folder, FILE_EXTENSION);

			assertFalse(EMPTY_PATH_MSG, paths.isEmpty());
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final EmptyResourceException exception) {
			fail(EMPTY_RES_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		} catch (final CoreException exception) {
			fail(CORE_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with folder) returns an
	 * InvalidResourceTypeException with a root given has entry..
	 * 
	 * @throws InvalidResourceTypeException
	 *             when a root is found
	 */
	@Test(expected = InvalidResourceTypeException.class)
	public void testRetrieveFilesFromFolderWithIRoot() throws InvalidResourceTypeException {

		try {
			final IFolder folder = mock(IFolder.class);
			final IWorkspaceRoot root = mock(IWorkspaceRoot.class);
			final IResource[] resources = { root };

			when(folder.isAccessible()).thenReturn(true);
			when(folder.members()).thenReturn(resources);
			when(root.getType()).thenReturn(IResource.ROOT);

			AnalysisHandlerUIUtils.retrieveFilesFromFolder(folder, FILE_EXTENSION);
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final EmptyResourceException exception) {
			fail(EMPTY_RES_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		} catch (final CoreException exception) {
			fail(CORE_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with folder) returns an empty list when no
	 * fortran file is given.
	 */
	@Test
	public void testRetrieveFilesFromFolderWithNoFortranFile() {

		try {
			final IFolder folder = mock(IFolder.class);
			final IFile file = mock(IFile.class);
			final IResource[] resources = { file };

			when(folder.isAccessible()).thenReturn(true);
			when(folder.members()).thenReturn(resources);
			when(file.getType()).thenReturn(IResource.FILE);
			when(file.getFileExtension()).thenReturn("c");
			when(file.getLocation()).thenReturn(new Path(NAME));
			when(file.getName()).thenReturn(NAME);

			final List<IPath> paths = AnalysisHandlerUIUtils.retrieveFilesFromFolder(folder, FILE_EXTENSION);

			assertTrue(NOT_EMP_PATH_MSG, paths.isEmpty());
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final EmptyResourceException exception) {
			fail(EMPTY_RES_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		} catch (final CoreException exception) {
			fail(CORE_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with folder) returns an
	 * EmptyResourceException when folder is empty.
	 * 
	 * @throws EmptyResourceException
	 *             when project is empty
	 */
	@Test(expected = EmptyResourceException.class)
	public void testRetrieveFilesFromFolderWithNoMembers() throws EmptyResourceException {

		try {
			new MetricAnalysisHandler();
			final IFolder folder = mock(IFolder.class);

			when(folder.isAccessible()).thenReturn(true);
			when(folder.members()).thenReturn(null);

			AnalysisHandlerUIUtils.retrieveFilesFromFolder(folder, FILE_EXTENSION);

		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		} catch (final CoreException exception) {
			fail(CORE_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with file) returns a not empty list in the
	 * right conditions.
	 */
	@Test
	public void testRetrieveSelectedFiles() {

		try {
			final IFile file = mock(IFile.class);

			when(file.getType()).thenReturn(IResource.FILE);
			when(file.getFileExtension()).thenReturn("f");
			when(file.getLocation()).thenReturn(new Path(NAME));
			when(file.getName()).thenReturn(NAME);

			final List<IPath> paths = AnalysisHandlerUIUtils.retrieveSelectedFiles(file, FILE_EXTENSION);

			assertFalse(EMPTY_PATH_MSG, paths.isEmpty());
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final EmptyResourceException exception) {
			fail(EMPTY_RES_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		} catch (final EmptySelectionException exception) {
			fail(EMPTY_SEL_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with file) returns a not empty list with a
	 * folder given has entry.
	 */
	@Test
	public void testRetrieveSelectedFilesWithIFolder() {

		try {
			final IFolder folder = mock(IFolder.class);
			final IFile file = mock(IFile.class);
			final IResource[] resources = { file };

			when(folder.isAccessible()).thenReturn(true);
			when(folder.members()).thenReturn(resources);
			when(folder.getType()).thenReturn(IResource.FOLDER);
			when(file.getType()).thenReturn(IResource.FILE);
			when(file.getFileExtension()).thenReturn("f");
			when(file.getLocation()).thenReturn(new Path(NAME));
			when(file.getName()).thenReturn(NAME);

			final List<IPath> paths = AnalysisHandlerUIUtils.retrieveSelectedFiles(folder, FILE_EXTENSION);

			assertFalse(EMPTY_PATH_MSG, paths.isEmpty());
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final EmptyResourceException exception) {
			fail(EMPTY_RES_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		} catch (final EmptySelectionException exception) {
			fail(EMPTY_SEL_FAIL);
		} catch (final CoreException exception) {
			fail(CORE_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with file) returns a not empty list with a
	 * project given has entry.
	 */
	@Test
	public void testRetrieveSelectedFilesWithIProject() {

		try {
			final IProject project = mock(IProject.class);
			final IFile file = mock(IFile.class);
			final IResource[] resources = { file };

			when(project.getType()).thenReturn(IResource.PROJECT);
			when(project.members()).thenReturn(resources);
			when(project.isAccessible()).thenReturn(true);
			when(file.getType()).thenReturn(IResource.FILE);
			when(file.getFileExtension()).thenReturn("f");
			when(file.getLocation()).thenReturn(new Path(NAME));
			when(file.getName()).thenReturn(NAME);

			final List<IPath> paths = AnalysisHandlerUIUtils.retrieveSelectedFiles(project, FILE_EXTENSION);

			assertFalse(EMPTY_PATH_MSG, paths.isEmpty());
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final EmptyResourceException exception) {
			fail(EMPTY_RES_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		} catch (final EmptySelectionException exception) {
			fail(EMPTY_SEL_FAIL);
		} catch (final CoreException exception) {
			fail(CORE_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with file) returns a not empty list with a
	 * root given has entry.
	 */
	@Test
	public void testRetrieveSelectedFilesWithIRoot() {

		try {
			final IWorkspaceRoot root = mock(IWorkspaceRoot.class);
			final IProject project = mock(IProject.class);
			final IProject[] projects = { project };
			final IFile file = mock(IFile.class);
			final IResource[] members = { file };

			when(root.getType()).thenReturn(IResource.ROOT);
			when(root.getProjects()).thenReturn(projects);
			when(project.getType()).thenReturn(IResource.PROJECT);
			when(project.members()).thenReturn(members);
			when(project.isAccessible()).thenReturn(true);
			when(file.getType()).thenReturn(IResource.FILE);
			when(file.getFileExtension()).thenReturn("f");
			when(file.getLocation()).thenReturn(new Path(NAME));
			when(file.getName()).thenReturn(NAME);

			final List<IPath> paths = AnalysisHandlerUIUtils.retrieveSelectedFiles(root, FILE_EXTENSION);

			assertFalse(EMPTY_PATH_MSG, paths.isEmpty());
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final EmptyResourceException exception) {
			fail(EMPTY_RES_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		} catch (final EmptySelectionException exception) {
			fail(EMPTY_SEL_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		} catch (final CoreException exception) {
			fail(CORE_FAIL);
		}
	}

	/**
	 * Assert that retrievedSelected (with file) returns an empty list when no
	 * fortran file is given.
	 */
	@Test
	public void testRetrieveSelectedFilesWithNoFortranFile() {

		try {
			final IFile file = mock(IFile.class);

			when(file.getType()).thenReturn(IResource.FILE);
			when(file.getFileExtension()).thenReturn("c");
			when(file.getLocation()).thenReturn(new Path(NAME));
			when(file.getName()).thenReturn(NAME);

			final List<IPath> paths = AnalysisHandlerUIUtils.retrieveSelectedFiles(file, FILE_EXTENSION);

			assertTrue(NOT_EMP_PATH_MSG, paths.isEmpty());
		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		} catch (final IllegalArgumentException e) {
			fail(ARGUMENT_FAIL);
		} catch (final EmptyResourceException exception) {
			fail(EMPTY_RES_FAIL);
		} catch (final UnknownResourceTypeException exception) {
			fail(UNKNOWN_FAIL);
		} catch (final InvalidResourceTypeException exception) {
			fail(INVALID_FAIL);
		} catch (final NonAccessibleResourceException exception) {
			fail(ACCES_FAIL);
		} catch (final EmptySelectionException exception) {
			fail(EMPTY_SEL_FAIL);
		}
	}

	/**
	 * Assert taht the right file extension is detected.
	 */
	@Test
	public void testCheckFileExtension() {

		try {
			final String[] extensions = { "f", "f77", "f90" };

			final IResource file = mock(IResource.class);
			final IResource file2 = mock(IResource.class);
			final IResource file3 = mock(IResource.class);
			final IResource file4 = mock(IResource.class);
			final IResource file5 = mock(IResource.class);
			final IResource file6 = mock(IResource.class);

			when(file.getFileExtension()).thenReturn("f");
			when(file.getName()).thenReturn("file");
			when(file2.getFileExtension()).thenReturn("F");
			when(file2.getName()).thenReturn("file2");
			when(file3.getFileExtension()).thenReturn("f77");
			when(file3.getName()).thenReturn("file3");
			when(file4.getFileExtension()).thenReturn("F77");
			when(file4.getName()).thenReturn("file4");
			when(file5.getFileExtension()).thenReturn("f90");
			when(file5.getName()).thenReturn("file5");
			when(file6.getFileExtension()).thenReturn("F90");
			when(file6.getName()).thenReturn("file6");

			boolean result = AnalysisHandlerUIUtils.checkFileExtension(file, extensions);
			assertTrue("Wrong file extension : file", result);
			result = AnalysisHandlerUIUtils.checkFileExtension(file2, extensions);
			assertTrue("Wrong file extension : file2", result);
			result = AnalysisHandlerUIUtils.checkFileExtension(file3, extensions);
			assertTrue("Wrong file extension : file3", result);
			result = AnalysisHandlerUIUtils.checkFileExtension(file4, extensions);
			assertTrue("Wrong file extension : file4", result);
			result = AnalysisHandlerUIUtils.checkFileExtension(file5, extensions);
			assertTrue("Wrong file extension : file5", result);
			result = AnalysisHandlerUIUtils.checkFileExtension(file6, extensions);
			assertTrue("Wrong file extension : file6", result);

		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		}
	}

	/**
	 * Assert that wrong file extension are detected.
	 */
	@Test
	public void testCheckFileExtensionWithNoFortranFile() {

		try {
			final IResource file = mock(IResource.class);
			final IResource file2 = mock(IResource.class);

			when(file.getFileExtension()).thenReturn("c");
			when(file.getName()).thenReturn("file");
			when(file2.getFileExtension()).thenReturn("");
			when(file2.getName()).thenReturn("file2");

			boolean result = AnalysisHandlerUIUtils.checkFileExtension(file, FILE_EXTENSION);
			assertFalse("Righ file extension : file", result);
			result = AnalysisHandlerUIUtils.checkFileExtension(file2, FILE_EXTENSION);
			assertFalse("Right file extension : file2", result);

		} catch (final SecurityException e) {
			fail(SECURITY_FAIL);
		}
	}

}
