package fr.cnes.analysis.tools.ui.images;

import fr.cnes.analysis.tools.ui.Activator;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.plugin.AbstractUIPlugin;

public class ImageFactory {

    public static final String ENABLED = "./icons/enabled.png";
    public static final String DISABLED = "./icons/disabled.png";
    public static final String ERROR_VERY_SMALL = "./icons/logo-i-code-rouge-8x8.png";
    public static final String ERROR_SMALL = "./icons/logo-i-code-rouge-16x16.png";
    public static final String ERROR_MEDIUM = "./icons/logo-i-code-rouge-32x32.png";
    public static final String ERROR_BIG = "./icons/logo-i-code-rouge-45x45.png";
    public static final String INFO_VERY_SMALL = "./icons/logo-i-code-bleue-8x8.png";
    public static final String INFO_SMALL = "./icons/logo-i-code-bleue-16x16.png";
    public static final String INFO_MEDIUM = "./icons/logo-i-code-bleue-32x32.png";
    public static final String INFO_BIG = "./icons/logo-i-code-bleue-45x45.png";
    public static final String WARNING_VERY_SMALL = "./icons/logo-i-code-orange-8x8.png";
    public static final String WARNING_SMALL = "./icons/logo-i-code-orange-16x16.png";
    public static final String WARNING_MEDIUM = "./icons/logo-i-code-orange-32x32.png";
    public static final String WARNING_BIG = "./icons/logo-i-code-orange-45x45.png";

    public static ImageDescriptor getDescriptor(String pImageLocation) {
        return AbstractUIPlugin.imageDescriptorFromPlugin(Activator.PLUGIN_ID, pImageLocation);
    }

    public static Image getImage(String pImageLocation) {
        return AbstractUIPlugin.imageDescriptorFromPlugin(Activator.PLUGIN_ID, pImageLocation)
                .createImage();
    }
}
