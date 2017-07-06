package fr.cnes.analysis.tools.ui.markers;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.SimpleMarkerAnnotation;

/**
 * ViolationErrorMarker This class implements the creation of marker and the
 * listing of theses ones. This class is useful for the use of markers and also
 * the decorator. It allows also multiple lines selection.
 * 
 */
public class ViolationErrorMarker {
    // ID of the marker
    public static final String MARKER = "fr.cnes.analysis.tools.ui.markers.ViolationErrorMarker";

    // ID of the annotation
    public static final String ANNOTATION = "fr.cnes.analysis.tools.ui.ViolationError";

    /**
     * Create a new marker
     * 
     * @param res
     *            The resource in which must be put the marker (file)
     * @param line
     *            Line number of the marker
     * @param description
     *            The description of the function
     * @param message
     *            The error message
     *
     * @return the new marker
     */
    public static IMarker createMarker(IResource res, Integer line, String description,
            String message) throws CoreException {
        IMarker marker = null;
        // note: you use the id that is defined in your plugin.xml
        marker = res.createMarker(MARKER);
        marker.setAttribute(IMarker.MESSAGE, message);
        marker.setAttribute(IMarker.LINE_NUMBER, line);
        marker.setAttribute(IMarker.PRIORITY, IMarker.PRIORITY_HIGH);
        marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
        return marker;
    }

    /**
     * Find all markers in a file.
     * 
     * @param resource
     * @return list of a resources markers
     */
    public static List<IMarker> findMarkers(IResource resource) {
        try {
            return Arrays.asList(resource.findMarkers(MARKER, true, IResource.DEPTH_ZERO));
        } catch (CoreException e) {
            return new ArrayList<IMarker>();
        }
    }

    /**
     * Returns a list of markers that are linked to the resource or any sub
     * resource of the resource
     * 
     * @param resource
     * @return list of markers that are linked to the resource or any
     *         sub-resource or resource
     */
    public static List<IMarker> findAllMarkers(IResource resource) {
        try {
            return Arrays.asList(resource.findMarkers(MARKER, true, IResource.DEPTH_INFINITE));
        } catch (CoreException e) {
            return new ArrayList<IMarker>();
        }
    }

    /**
     * Returns the selection of the package explorer
     * 
     * @return the selection of the package explorer
     */
    public static TreeSelection getTreeSelection() {
        TreeSelection toReturn = null;
        final ISelection selection = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getSelectionService().getSelection();
        if (selection instanceof TreeSelection) {
            toReturn = (TreeSelection) selection;
        }
        return toReturn;
    }

    /**
     * Returns the selection of the package explorer
     * 
     * @return selection of the package explorer
     */
    public static TextSelection getTextSelection() {
        TextSelection toReturn = null;
        final ISelection selection = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getSelectionService().getSelection();
        if (selection instanceof TextSelection) {
            toReturn = (TextSelection) selection;
        }
        return toReturn;
    }

    public static void addAnnotation(IMarker marker, ITextSelection selection, ITextEditor editor) {
        // The DocumentProvider enables to get the document currently loaded in
        // the editor
        IDocumentProvider idp = editor.getDocumentProvider();

        // This is the document we want to connect to. This is taken from the
        // current editor input.
        IDocument document = idp.getDocument(editor.getEditorInput());

        // The IannotationModel enables to add/remove/change annoatation to a
        // Document loaded in an Editor
        IAnnotationModel iamf = idp.getAnnotationModel(editor.getEditorInput());

        // Note: The annotation type id specify that you want to create one of
        // your annotations
        SimpleMarkerAnnotation ma = new SimpleMarkerAnnotation(ANNOTATION, marker);

        // Finally add the new annotation to the model
        iamf.connect(document);
        iamf.addAnnotation(ma, new Position(selection.getOffset(), selection.getLength()));
        iamf.disconnect(document);
    }
}
