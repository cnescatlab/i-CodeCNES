/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
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

import fr.cnes.icode.logger.ICodeLogger;

/**
 * ViolationErrorMarker This class implements the creation of marker and the
 * listing of theses ones. This class is useful for the use of markers and also
 * the decorator. It allows also multiple lines selection.
 */
public final class InformationMarker {
    /**
     * ID of the marker
     */
    public static final String MARKER = "fr.cnes.analysis.tools.ui.markers.InformationMarker";

    /**
     * ID of the annotation
     */
    public static final String ANNOTATION = "fr.cnes.analysis.tools.ui.Information";
    /**
     * Class name
     */
    private static final String CLASS = InformationMarker.class.getName();

    /**
     * Default constructor removal to avoid instantiation.
     */
    private InformationMarker() {

    }

    /**
     * Create a new marker
     *
     * @param res         The resource in which must be put the marker (file)
     * @param line        Line number of the marker
     * @param description The description of the function
     * @param message     The error message
     * @return the new marker
     * @throws CoreException when marker couldn't be created.
     */
    public static IMarker createMarker(final IResource res, final Integer line,
                                       final String description, final String message) throws CoreException {
        final String method = "createMarker";
        ICodeLogger.entering(CLASS, method, new Object[]{
                res, line, description, message
        });
        IMarker marker = null;
        // note: you use the id that is defined in your plugin.xml
        marker = res.createMarker(MARKER);
        marker.setAttribute(IMarker.MESSAGE, message);
        marker.setAttribute(IMarker.LINE_NUMBER, line);
        marker.setAttribute(IMarker.PRIORITY, IMarker.PRIORITY_LOW);
        marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_INFO);
        ICodeLogger.exiting(CLASS, method, marker);
        return marker;
    }

    /**
     * Find all markers in a file.
     *
     * @param resource resource to find marker on.
     * @return list of a resources markers
     */
    public static List<IMarker> findMarkers(final IResource resource) {
        final String method = "findMarkers";
        ICodeLogger.entering(CLASS, method, resource);
        List<IMarker> markers;
        try {
            markers = Arrays.asList(resource.findMarkers(MARKER, true, IResource.DEPTH_ZERO));
        } catch (@SuppressWarnings("unused") CoreException e) {
            markers = new ArrayList<IMarker>();
        }
        ICodeLogger.exiting(CLASS, method, markers);
        return markers;
    }

    /**
     * Returns a list of markers that are linked to the resource or any sub
     * resource of the resource
     *
     * @param resource resource to find marker on.
     * @return list of markers that are linked to the resource or any
     * sub-resource or resource
     */
    public static List<IMarker> findAllMarkers(final IResource resource) {
        final String method = "findAllMarkers";
        ICodeLogger.entering(CLASS, method, resource);
        List<IMarker> markers;
        try {
            markers = Arrays.asList(resource.findMarkers(MARKER, true, IResource.DEPTH_INFINITE));
        } catch (@SuppressWarnings("unused") CoreException e) {
            markers = new ArrayList<IMarker>();
        }
        ICodeLogger.exiting(CLASS, method, markers);
        return markers;
    }

    /**
     * Returns the selection of the package explorer
     *
     * @return the selection of the package explorer
     */
    public static TreeSelection getTreeSelection() {
        final String method = "getTreeSelection";
        ICodeLogger.entering(CLASS, method);
        TreeSelection toReturn = null;
        final ISelection selection = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getSelectionService().getSelection();
        if (selection instanceof TreeSelection) {
            toReturn = (TreeSelection) selection;
        }
        ICodeLogger.exiting(CLASS, method, toReturn);
        return toReturn;
    }

    /**
     * Returns the selection of the package explorer
     *
     * @return selection of the package explorer
     */
    public static TextSelection getTextSelection() {
        final String method = "getTextSelection";
        ICodeLogger.entering(CLASS, method);
        TextSelection toReturn = null;
        final ISelection selection = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getSelectionService().getSelection();
        if (selection instanceof TextSelection) {
            toReturn = (TextSelection) selection;
        }
        ICodeLogger.exiting(CLASS, method, toReturn);
        return toReturn;
    }

    /**
     * @param marker    Marker to add annotation
     * @param selection selection to add annotation on.
     * @param editor    document editor to add annotation on.
     */
    public static void addAnnotation(final IMarker marker, final ITextSelection selection,
                                     final ITextEditor editor) {
        final String method = "addAnnotation";
        ICodeLogger.entering(CLASS, method, new Object[]{
                marker, selection, editor
        });
        // The DocumentProvider enables to get the document currently loaded in
        // the editor
        final IDocumentProvider idp = editor.getDocumentProvider();

        // This is the document we want to connect to. This is taken from the
        // current editor input.
        final IDocument document = idp.getDocument(editor.getEditorInput());

        // The IannotationModel enables to add/remove/change annoatation to a
        // Document loaded in an Editor
        final IAnnotationModel iamf = idp.getAnnotationModel(editor.getEditorInput());

        // Note: The annotation type id specify that you want to create one of
        // your annotations
        final SimpleMarkerAnnotation ma = new SimpleMarkerAnnotation(ANNOTATION, marker);

        // Finally add the new annotation to the model
        iamf.connect(document);
        iamf.addAnnotation(ma, new Position(selection.getOffset(), selection.getLength()));
        iamf.disconnect(document);
        ICodeLogger.exiting(CLASS, method);
    }
}
