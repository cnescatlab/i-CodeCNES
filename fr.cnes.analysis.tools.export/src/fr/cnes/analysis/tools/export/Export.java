package fr.cnes.analysis.tools.export;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

/************************************************************************************************/
/* i-Code CNES is a static code analyzer. */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html */
/************************************************************************************************/

import fr.cnes.analysis.tools.export.exception.NoContributorMatchingException;
import fr.cnes.analysis.tools.export.exception.NoExtensionIndicatedException;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;

/**
 * This class is an import and export service for i-Code CNES.
 * <p>
 * To use it, your plugin must be dependent of the
 * {@link fr.cnes.analysis.tools.analyzer} plugin.
 * </p>
 * 
 * <p>
 * The methods of this class throw <tt>NoContributorMatchingException</tt> when
 * a function could not retrieve or reach intended data of a contributor. It
 * also throw <tt>NoIndicatedFormatInFileException</tt> when the file to import
 * or export has no extension.
 * </p>
 * 
 * @since 3.0
 * 
 */
public class Export {

    public static final String Export_ExtensionPoint_ID = "fr.cnes.analysis.tools.export";
    public static final String Export_ExtensionPoint_Attribute_formatName = "formatName";
    public static final String Export_ExtensionPoint_Attribute_formatExtension = "formatExtension";
    public static final String Export_ExtensionPoint_Attribute_exportClass = "ExportClass";
    public static final String Export_ExtensionPoint_Attribute_importClass = "ImportClass";

    /**
     * This function return all available {@code formatName} and
     * {@code formatExtension} defined by {@link #Export_ExtensionPoint_ID}
     * contributors.
     * 
     * @return a Map with formatName as key and and formatExtension as value of
     *         every format handled by the contributor of the ExtensionPoint
     *         {@link Export_ExtensionPoint_ID}
     */
    public Map<String, String> getAvailableFormats() {
        Map<String, String> formats = new TreeMap<>();
        for (IConfigurationElement contribution : this.getContributions()) {
            formats.put(contribution.getAttribute(Export_ExtensionPoint_Attribute_formatName),
                    contribution.getAttribute(Export_ExtensionPoint_Attribute_formatExtension));
        }
        return formats;
    }

    /**
     * This function returns all {@link IConfigurationElement} contributing to
     * the {@code ExtensionPoint} {@value #Export_ExtensionPoint_ID}.
     * 
     * @return all configuration elements contributing to ExtensionPoint
     *         {@link #Export_ExtensionPoint_ID}
     */
    private IConfigurationElement[] getContributions() {
        return Platform.getExtensionRegistry()
                .getConfigurationElementsFor(Export_ExtensionPoint_ID);
    }

    /**
     * This function export each {@link CheckResult} of {@code checkResults}
     * parameter into the {@link File} parameter.
     * 
     * @param checkResults
     *            to export.
     * @param outputFile
     *            to use for the export.
     * @throws NoContributorMatchingException
     *             when a format can not be handled by the @link {@link Export}
     *             service.
     * @throws NoExtensionIndicatedException
     *             when the {@code outputFile} has no extension indicated.
     * @throws IOException
     *             when the export failed due to a {@link java.io.File}
     *             exception.
     */
    public void export(List<CheckResult> checkResults, File outputFile)
            throws NoContributorMatchingException, NoExtensionIndicatedException, IOException {
        IExport exporter = this
                .getExportClass(this.getExtensionFromFilePath(outputFile.getAbsolutePath()));
        exporter.export(checkResults, outputFile);
    }

    /**
     * @param inputFile
     *            to realize the import on.
     * @return the list of {@link CheckResult} extracted from the
     *         {@code inputFile}.
     * @throws NoExtensionIndicatedException
     *             when the {@code inputFile} do not end with an extension.
     * @throws NoContributorMatchingException
     *             when the format of the {@code intputFile} is not handled by
     *             any contributor of the {@link #Export_ExtensionPoint_ID}
     *             contributors {@code ExtensionPoint}.
     */
    public List<CheckResult> importResults(File inputFile)
            throws NoExtensionIndicatedException, NoContributorMatchingException {
        List<CheckResult> checkResults = null;
        IImport importer = this
                .getImportClass(this.getExtensionFromFilePath(inputFile.getAbsolutePath()));
        checkResults = importer.importResults(inputFile);
        if (checkResults == null) {
            throw new NoContributorMatchingException();
        }
        return checkResults;
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
    private String getExtensionFromFilePath(final String filePath)
            throws NoExtensionIndicatedException {
        String extension = "";

        int index = filePath.lastIndexOf('.');
        int parents = Math.max(filePath.lastIndexOf('/'), filePath.lastIndexOf('\\'));

        if (index > parents) {
            extension = filePath.substring(index + 1);
        } else {
            throw new NoExtensionIndicatedException();
        }
        return extension;
    }

    /**
     * This function browse {@value #Export_ExtensionPoint_ID} contributors and
     * return the first class implementing {@link IExport} based on the format
     * of {@literal formatExtension} parameter requested.
     * 
     * @param formatExtension
     *            The extension of the file (without ".")
     * @return The IImport class found in contribution to import the extension
     *         of this format.
     * @throws NoImportClassFoundInContributions
     *             when after browsing contributor no class importing this
     *             format was found.
     */
    private IImport getImportClass(String formatExtension) throws NoContributorMatchingException {
        IImport importClass = null;
        for (IConfigurationElement contribution : this.getContributions()) {
            if (contribution.getAttribute(Export_ExtensionPoint_Attribute_formatExtension)
                    .equals(formatExtension)) {
                try {
                    importClass = (IImport) contribution
                            .createExecutableExtension(Export_ExtensionPoint_Attribute_importClass);
                } catch (CoreException e) {
                    e.printStackTrace();
                }
            }
        }
        if (importClass == null) {
            throw new NoContributorMatchingException();
        }
        return importClass;
    }

    /**
     * This function browses {@value #Export_ExtensionPoint_ID} contributors and
     * return the first class implementing {@link IExport} set for the format
     * matching {@code formatExtension} parameter requested.
     * 
     * @param formatExtension
     *            extension requested (without ".").
     * @return {@link IExport} set by a contributor to export
     *         {@code formatExtension} requested.
     * @throws NoContributorMatchingException
     *             when there is no contributor of
     *             {@value #Export_ExtensionPoint_ID} able to export the
     *             requested format.
     */
    private IExport getExportClass(String formatExtension) throws NoContributorMatchingException {
        /*
         * The export class to return from the contributors of the Extension
         * Point.
         */
        IExport exportClass = null;
        for (IConfigurationElement contribution : this.getContributions()) {
            if (contribution.getAttribute(Export_ExtensionPoint_Attribute_formatExtension)
                    .equals(formatExtension)) {
                try {
                    Object o = contribution
                            .createExecutableExtension(Export_ExtensionPoint_Attribute_exportClass);
                    if (o instanceof IExport) {
                        exportClass = (IExport) o;
                    }
                } catch (CoreException e) {
                    e.printStackTrace();
                }
            }
        }

        if (exportClass == null) {
            throw new NoContributorMatchingException();
        }
        return exportClass;
    }

}
