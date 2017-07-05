package fr.cnes.analysis.tools.ui.images;

import fr.cnes.analysis.tools.ui.Activator;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.plugin.AbstractUIPlugin;

public class ImageService {

    public static final Image ENABLED = AbstractUIPlugin
            .imageDescriptorFromPlugin(Activator.PLUGIN_ID, ("./icons/enabled.png")).createImage();
    public static final Image DISABLED = AbstractUIPlugin
            .imageDescriptorFromPlugin(Activator.PLUGIN_ID, ("./icons/disabled.png")).createImage();

    public static final Image ERROR_VERY_SMALL = AbstractUIPlugin
            .imageDescriptorFromPlugin(Activator.PLUGIN_ID, ("./icons/logo-i-code-rouge-8x8.png"))
            .createImage();
    public static final Image ERROR_SMALL = AbstractUIPlugin
            .imageDescriptorFromPlugin(Activator.PLUGIN_ID, ("./icons/logo-i-code-rouge-16x16.png"))
            .createImage();

    public static final Image ERROR_MEDIUM = AbstractUIPlugin
            .imageDescriptorFromPlugin(Activator.PLUGIN_ID, "./icons/logo-i-code-rouge-32x32.png")
            .createImage();

    public static final Image ERROR_BIG = AbstractUIPlugin
            .imageDescriptorFromPlugin(Activator.PLUGIN_ID, "./icons/logo-i-code-rouge-45x45.png")
            .createImage();

    public static final Image INFO_VERY_SMALL = AbstractUIPlugin
            .imageDescriptorFromPlugin(Activator.PLUGIN_ID, ("./icons/logo-i-code-bleue-8x8.png"))
            .createImage();
    public static final Image INFO_SMALL = AbstractUIPlugin
            .imageDescriptorFromPlugin(Activator.PLUGIN_ID, ("./icons/logo-i-code-bleue-16x16.png"))
            .createImage();

    public static final Image INFO_MEDIUM = AbstractUIPlugin
            .imageDescriptorFromPlugin(Activator.PLUGIN_ID, "./icons/logo-i-code-bleue-32x32.png")
            .createImage();

    public static final Image INFO_BIG = AbstractUIPlugin
            .imageDescriptorFromPlugin(Activator.PLUGIN_ID, "./icons/logo-i-code-bleue-45x45.png")
            .createImage();

    public static final Image WARNING_VERY_SMALL = AbstractUIPlugin
            .imageDescriptorFromPlugin(Activator.PLUGIN_ID, ("./icons/logo-i-code-orange-8x8.png"))
            .createImage();
    public static final Image WARNING_SMALL = AbstractUIPlugin.imageDescriptorFromPlugin(
            Activator.PLUGIN_ID, ("./icons/logo-i-code-orange-16x16.png")).createImage();

    public static final Image WARNING_MEDIUM = AbstractUIPlugin
            .imageDescriptorFromPlugin(Activator.PLUGIN_ID, "./icons/logo-i-code-orange-32x32.png")
            .createImage();

    public static final Image WARNING_BIG = AbstractUIPlugin
            .imageDescriptorFromPlugin(Activator.PLUGIN_ID, "./icons/logo-i-code-orange-45x45.png")
            .createImage();

}
