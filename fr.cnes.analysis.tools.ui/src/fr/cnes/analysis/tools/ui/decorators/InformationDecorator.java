/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.decorators;

import java.util.List;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;
import org.eclipse.jface.viewers.LabelProvider;

import fr.cnes.analysis.tools.ui.images.ImageFactory;
import fr.cnes.analysis.tools.ui.logger.UILogger;
import fr.cnes.analysis.tools.ui.markers.InformationMarker;
import fr.cnes.analysis.tools.ui.markers.ViolationErrorMarker;
import fr.cnes.analysis.tools.ui.markers.ViolationWarningMarker;

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
public class InformationDecorator extends LabelProvider implements ILightweightLabelDecorator {

    /**
     * Link to the Violation Error icon
     */
    public static final String ICON = ImageFactory.INFO_VERY_SMALL;
    /** Decorator ID */
    public static final String ID_INFORMATION_DECORATOR = "fr.cnes.analysis.tools.ui.decorators"
                    + ".informationdecorator";

    /** Class name **/
    private static final String CLASS = InformationDecorator.class.getName();

    /**
     * An Violation Error icon is being put on the top-right of the icon's file
     * only if the file contain a marker of type "ViolationErrorMarker".
     * 
     */
    @Override
    public void decorate(Object resource, final IDecoration decoration) {
        final String method = "decorate";
        UILogger.entering(CLASS, method, new Object[] {
            resource, decoration
        });

        if (resource instanceof IResource) {
            // We add a Information decorator only if there is a information in
            // the file and that there is no errors markers nor warning markers
            // in the file
            final List<IMarker> vErrorMarkers = ViolationErrorMarker
                            .findAllMarkers((IResource) resource);
            final List<IMarker> vWarningMarkers = ViolationWarningMarker
                            .findAllMarkers((IResource) resource);
            final List<IMarker> vInformationMarkers = InformationMarker
                            .findAllMarkers((IResource) resource);
            if (vErrorMarkers.isEmpty() && vWarningMarkers.isEmpty()
                            && !vInformationMarkers.isEmpty()) {
                // If the file do not contain error marker and contain warning
                // markers then we put an overlay icon on the top right of the
                // file's icon
                decoration.addOverlay(ImageFactory.getDescriptor(ICON), IDecoration.TOP_RIGHT);
            } else {
                // otherwise we remove the overlay if there is no violation
                // error neither violation warning markers.
                // NOTE : When warnings decorators are activated, this is only
                // here that we remove the markers if both
                // Error and Warnings decorators are removed.
                if (vErrorMarkers.isEmpty() && vWarningMarkers.isEmpty()
                                && vInformationMarkers.isEmpty()) {
                    decoration.addOverlay(null);
                }
            }
        }
        UILogger.exiting(CLASS, method);
    }

}
