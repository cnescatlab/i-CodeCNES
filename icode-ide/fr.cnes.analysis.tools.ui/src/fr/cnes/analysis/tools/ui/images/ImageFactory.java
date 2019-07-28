/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.images;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import fr.cnes.analysis.tools.ui.Activator;
import fr.cnes.icode.logger.ICodeLogger;

/**
 * This service should be used to reach images stored in the plugin resources.
 *
 * @since 3.0
 */
public final class ImageFactory {

    /**
     * I-code logo red, 8x8
     */
    public static final String ENABLED = "./icons/enabled.png";
    /**
     * I-code logo red, 8x8
     */
    public static final String DISABLED = "./icons/disabled.png";
    /**
     * I-code logo red, 8x8
     */
    public static final String ERROR_VERY_SMALL = "./icons/logo-i-code-rouge-8x8.png";
    /**
     * I-code logo red, 16x16
     */
    public static final String ERROR_SMALL = "./icons/logo-i-code-rouge-16x16.png";
    /**
     * I-code logo red, 32x32
     */
    public static final String ERROR_MEDIUM = "./icons/logo-i-code-rouge-32x32.png";
    /**
     * I-code logo red, 8x8
     */
    public static final String ERROR_BIG = "./icons/logo-i-code-rouge-45x45.png";
    /**
     * I-code logo blue, 8x8
     */
    public static final String INFO_VERY_SMALL = "./icons/logo-i-code-bleue-8x8.png";
    /**
     * I-code logo blue, 16x16
     */
    public static final String INFO_SMALL = "./icons/logo-i-code-bleue-16x16.png";
    /**
     * I-code logo orange, 8x8
     */
    public static final String WARNING_VERY_SMALL = "./icons/logo-i-code-orange-8x8.png";
    /**
     * I-code logo orange, 16x16
     */
    public static final String WARNING_SMALL = "./icons/logo-i-code-orange-16x16.png";
    /**
     * I-code logo orange, 32x32
     */
    public static final String WARNING_MEDIUM = "./icons/logo-i-code-orange-32x32.png";
    /**
     * I-code logo orange, 8x8
     */
    public static final String WARNING_BIG = "./icons/logo-i-code-orange-45x45.png";
    /**
     * Class name
     */
    private static final String CLASS = ImageFactory.class.getName();

    /**
     * Private constructor to remove public constructor has this utility class
     * should not be instantiate.
     */
    private ImageFactory() {
        // not called
    }

    /**
     * @param pImageLocation Location of the image (relative to the plugin).
     * @return The ImageDescriptor located in <code>pImageLocation</code>
     */
    public static ImageDescriptor getDescriptor(final String pImageLocation) {
        final String method = "getDescriptor";
        ICodeLogger.entering(CLASS, method, pImageLocation);
        final ImageDescriptor descriptor = AbstractUIPlugin
                .imageDescriptorFromPlugin(Activator.PLUGIN_ID, pImageLocation);
        ICodeLogger.exiting(CLASS, method, descriptor);
        return descriptor;
    }

    /**
     * @param pImageLocation Location of the image file (relative to the plug-in).
     * @return The Image located in <code>pImageLocation</code>
     */
    public static Image getImage(final String pImageLocation) {
        final String method = "getImage";
        ICodeLogger.entering(CLASS, method, pImageLocation);
        final Image image = AbstractUIPlugin
                .imageDescriptorFromPlugin(Activator.PLUGIN_ID, pImageLocation)
                .createImage();
        ICodeLogger.exiting(CLASS, method, image);
        return image;
    }
}
