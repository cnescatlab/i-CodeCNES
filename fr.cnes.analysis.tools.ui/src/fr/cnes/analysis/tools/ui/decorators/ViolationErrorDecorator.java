/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.decorators;

import java.util.List;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.ui.IDecoratorManager;
import org.eclipse.ui.PlatformUI;

import fr.cnes.analysis.tools.ui.markers.ViolationErrorMarker;

/**
 * Put a new Decoration in the files tree on the top right of an icon of a file
 * if a file contains a violation error marker.
 * 
 * This class is being called everytime a document is being refreshed in the
 * files explorer view.
 * 
 * In case there is no marker anymore (error && warning), this class restore
 * back the original icon.
 *
 */
public class ViolationErrorDecorator extends LabelProvider implements ILightweightLabelDecorator {

    /**
     * Link to the Violation Error icon
     */
    public static final String ICON                           = "/icons/logo-i-code-rouge-8x8.png";
    public static final String ID_VIOLATION_WARNING_DECORATOR = "fr.cnes.tools.ui.decorators.violationwarningdecorator";

    /**
     * An Violation Error icon is being put on the top-right of the icon's file
     * only if the file contain a marker of type "ViolationErrorMarker".
     * 
     */
    @Override
    public void decorate(Object resource, final IDecoration decoration) {

        /*
         * We call the decorator manager to be able to know if the Violation
         * Warning decorator is activated. If it's is activated then the
         * decorators removal will be done by the Violoation Warning decorator.
         * Otherwise, it's the Violation Error decorator that removes the errors
         * decorators.
         */
        final IDecoratorManager manager = PlatformUI.getWorkbench().getDecoratorManager();
        if (resource instanceof IResource) {
            final List<IMarker> markers = ViolationErrorMarker.findAllMarkers((IResource) resource);
            if (!markers.isEmpty()) {
                decoration.addOverlay(
                        ImageDescriptor.createFromFile(ViolationErrorDecorator.class, ICON),
                        IDecoration.TOP_RIGHT);
                // By recursivity, decorate all parents.

            }

        } else if (!manager.getEnabled(ID_VIOLATION_WARNING_DECORATOR)) {
            decoration.addOverlay(null);
        }
    }

}
