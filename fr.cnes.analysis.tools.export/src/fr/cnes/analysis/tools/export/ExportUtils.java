package fr.cnes.analysis.tools.export;

import java.util.Map;
import java.util.TreeMap;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;

import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;
import fr.cnes.analysis.tools.export.exception.NoExtensionIndicatedException;

/**
 * This class contains method for export plug-in.
 */
public final class ExportUtils {

    /** Export extension point ID */
    public static final String EXPORT_EXTENSIONPOINT_ID = "fr.cnes.analysis.tools.export";
    /** Export extension point formatName attribute */
    public static final String EXPORT_EXTENSIONPOINT_ATTRIBUTE_FORMATNAME = "formatName";
    /** Export extension point formatExtension attribute */
    public static final String EXPORT_EXTENSIONPOINT_ATTRIBUTE_EXTENSION = "formatExtension";

    /** Class name */
    private static final String CLASS = ExportUtils.class.getName();

    /**
     * This utils class should not be instantied.
     */
    private ExportUtils() {
        // no constructor
    }

    /**
     * This function returns all {@link IConfigurationElement} contributing to
     * the {@code ExtensionPoint} {@value #EXPORT_EXTENSIONPOINT_ID}.
     * 
     * @return all configuration elements contributing to ExtensionPoint
     *         {@link #EXPORT_EXTENSIONPOINT_ID}
     */
    protected static IConfigurationElement[] getContributions() {
        final String method = "getContributions";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method);
        return Platform.getExtensionRegistry()
                        .getConfigurationElementsFor(EXPORT_EXTENSIONPOINT_ID);
    }

    /**
     * This function parse a {@link String} indicating the filePath of a file to
     * return it's format.
     * 
     * @param filePath
     *            to search for the extension.
     * @return the format extension of the file without ".".
     * @throws NoExtensionIndicatedException
     *             when there is no indicated format in the file's path.
     */
    protected static String getExtensionFromFilePath(final String filePath)
                    throws NoExtensionIndicatedException {
        final String method = "getExtensionFromFilePath";
        ICodeLogger.entering(CLASS, method);
        String extension = "";

        final int index = filePath.lastIndexOf('.');
        final int parents = Math.max(filePath.lastIndexOf('/'), filePath.lastIndexOf('\\'));

        if (index > parents) {
            extension = filePath.substring(index + 1);
        } else {
            final NoExtensionIndicatedException exception = new NoExtensionIndicatedException();
            ICodeLogger.throwing(CLASS, method, exception);
            throw exception;
        }
        ICodeLogger.exiting(CLASS, method, extension);
        return extension;
    }

    /**
     * This function return all available {@code formatName} and
     * {@code formatExtension} defined by {@link #EXPORT_EXTENSIONPOINT_ID}
     * contributors.
     * 
     * @return a Map with formatName as key and and formatExtension as value of
     *         every format handled by the contributor of the ExtensionPoint
     *         {@link Export_ExtensionPoint_ID}
     */
    public static Map<String, String> getAvailableFormats() {
        final String method = "getAvailableFormats";
        ICodeLogger.entering(CLASS, method);
        final Map<String, String> formats = new TreeMap<>();
        for (IConfigurationElement contribution : getContributions()) {
            formats.put(contribution.getAttribute(EXPORT_EXTENSIONPOINT_ATTRIBUTE_FORMATNAME),
                            contribution.getAttribute(EXPORT_EXTENSIONPOINT_ATTRIBUTE_EXTENSION));
        }
        ICodeLogger.exiting(CLASS, method, formats);
        return formats;
    }
}
