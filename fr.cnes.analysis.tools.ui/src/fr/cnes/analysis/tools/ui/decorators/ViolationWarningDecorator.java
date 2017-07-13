/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.decorators;

import fr.cnes.analysis.tools.ui.images.ImageFactory;
import fr.cnes.analysis.tools.ui.markers.ViolationErrorMarker;
import fr.cnes.analysis.tools.ui.markers.ViolationWarningMarker;
import java.util.List;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;
import org.eclipse.jface.viewers.LabelProvider;

/**
 * ViolationWarningDecorator add decorators to the file's icon when there is
 * markers of warning criticity and no marker of error criticity.
 * 
 * This class is being called everytime a document is being refreshed in the
 * files explorer view.
 * 
 * In case there is no marker anymore (error && warning), this class restore
 * back the original icon.
 *
 */
public class ViolationWarningDecorator extends LabelProvider implements ILightweightLabelDecorator {

    /**
     * Link to the i-Code CNES Warning criticity icon.
     */
    public static final String ICON = ImageFactory.WARNING_VERY_SMALL;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ILightweightLabelDecorator#decorate(java.lang.
     * Object, org.eclipse.jface.viewers.IDecoration)
     */
    @Override
    public void decorate(final Object resource, final IDecoration decoration) {

        if (resource instanceof IResource) {
            // We add a ViolationWarningDecorator only if there is a warning in
            // the file and that there is no errors markers in the file
            final List<IMarker> vErrorMarkers = ViolationErrorMarker
                    .findAllMarkers((IResource) resource);
            final List<IMarker> vWarningMarkers = ViolationWarningMarker
                    .findAllMarkers((IResource) resource);

            if (vErrorMarkers.isEmpty() && !vWarningMarkers.isEmpty()) {
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
                if (vErrorMarkers.isEmpty() && vWarningMarkers.isEmpty()) {
                    decoration.addOverlay(null);
                }
            }
        }

    }
}
