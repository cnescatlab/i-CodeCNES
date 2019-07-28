/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.services.export;

import fr.cnes.icode.logger.ICodeLogger;
import fr.cnes.icode.reflexion.ClassFinder;
import fr.cnes.icode.services.export.exception.NoExtensionIndicatedException;

import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

/**
 * This class contains method for export plug-in.
 */
public final class ExportUtils {

    /** Class name */
    private static final String CLASS = ExportUtils.class.getName();

    /**
     * This utils class should not be instantied.
     */
    private ExportUtils() {
        // no constructor
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
     * {@code formatExtension} defined by contributors.
     * 
     * @return a Map with formatName as key and and formatExtension as value of
     *         every format handled by the contributor of the ExtensionPoint.
     */
    public static Map<String, String> getAvailableFormats() {
        final String method = "getAvailableFormats";
        ICodeLogger.entering(CLASS, method);
        final Map<String, String> formats = new TreeMap<>();
        try {
            Set<Class<?>> exporters = ClassFinder.find(IExporter.class);
            for(final Class current : exporters) {
                final IExporter exporter = (IExporter) current.newInstance();
                formats.put(exporter.getFormatName(), exporter.getFormatExtension());
            }
        } catch (final Exception e) {
            ICodeLogger.error(CLASS, method, e);
        }
        ICodeLogger.exiting(CLASS, method, formats);
        return formats;
    }
}
