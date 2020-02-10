/************************************************************************************************/
/* i-Code CNES is a static code analyzer. */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html */
/************************************************************************************************/
package fr.cnes.analysis.tools.export;

import java.io.File;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;
import fr.cnes.analysis.tools.export.exception.NoContributorMatchingException;
import fr.cnes.analysis.tools.export.exception.NoExtensionIndicatedException;

/**
 * This class is an import and export service for i-Code CNES.
 * <p>
 * To use it, your plug-in must be dependent of the
 * {@link fr.cnes.analysis.tools.analyzer} plug-in.
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
public class ImportService implements IImportService {

    /** Export extension point ID */
    public static final String EXPORT_EXTENSIONPOINT_ID = "fr.cnes.analysis.tools.export";
    /** Export extension point formatName attribute */
    public static final String EXPORT_EXTENSIONPOINT_ATTRIBUTE_FORMATNAME = "formatName";
    /** Export extension point formatExtension attribute */
    public static final String EXPORT_EXTENSIONPOINT_ATTRIBUTE_EXTENSION = "formatExtension";
    /** Export extension point attribute ImportClass */
    public static final String EXPORT_EXTENSIONPOINT_ATTRIBUTE_IMPORTCLASS = "ImportClass";
    /** Class name */
    private static final String CLASS = ImportService.class.getName();

    /*
     * (non-Javadoc)
     * 
     * @see
     * fr.cnes.analysis.tools.export.IImportService#importResults(java.io.File)
     */
    @Override
    public List<CheckResult> importResults(final File inputFile)
                    throws NoExtensionIndicatedException, NoContributorMatchingException {
        final String method = "importResults";
        ICodeLogger.entering(CLASS, method, inputFile);
        List<CheckResult> checkResults = null;
        final IImporter importer = getImportClass(
                        ExportUtils.getExtensionFromFilePath(inputFile.getAbsolutePath()));
        checkResults = importer.importResults(inputFile);
        if (checkResults == null) {
            final NoContributorMatchingException exception = new NoContributorMatchingException();
            ICodeLogger.throwing(CLASS, method, exception);
            throw exception;
        }
        ICodeLogger.exiting(CLASS, method, checkResults);
        return checkResults;
    }

    /**
     * This function browse {@value #EXPORT_EXTENSIONPOINT_ID} contributors and
     * return the first class implementing {@link IExporter} based on the format
     * of {@literal formatExtension} parameter requested.
     * 
     * @param formatExtension
     *            The extension of the file (without ".")
     * @return The IImport class found in contribution to import the extension
     *         of this format.
     * @throws NoContributorMatchingException
     *             when the indicated format has no exporter contribution
     *             defined when failing to create executable from contributor of
     *             {@link #EXPORT_EXTENSIONPOINT_ID}'s attribute
     *             {@link #EXPORT_EXTENSIONPOINT_ATTRIBUTE_EXPORTCLASS}.
     */
    private IImporter getImportClass(final String formatExtension)
                    throws NoContributorMatchingException {
        final String method = "getImportClass";
        ICodeLogger.entering(CLASS, method, formatExtension);
        IImporter importClass = null;
        for (IConfigurationElement contribution : ExportUtils.getContributions()) {
            if (contribution.getAttribute(EXPORT_EXTENSIONPOINT_ATTRIBUTE_EXTENSION)
                            .equals(formatExtension)) {
                try {
                    importClass = (IImporter) contribution.createExecutableExtension(
                                    EXPORT_EXTENSIONPOINT_ATTRIBUTE_IMPORTCLASS);
                } catch (CoreException e) {
                    ICodeLogger.error(CLASS, method, e);
                }
            }
        }
        if (importClass == null) {
            final NoContributorMatchingException exception = new NoContributorMatchingException();
            ICodeLogger.throwing(CLASS, method, exception);
            throw exception;
        }
        ICodeLogger.exiting(CLASS, method, importClass);
        return importClass;
    }

    @Override
    public Map<String, String> getAvailableFormats() {
        return ExportUtils.getAvailableFormats();
    }
}
