/************************************************************************************************/
/* i-Code CNES is a static code analyzer. */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html */
/************************************************************************************************/
package fr.cnes.analysis.tools.export;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;
import fr.cnes.analysis.tools.export.exception.NoContributorMatchingException;
import fr.cnes.analysis.tools.export.exception.NoExtensionIndicatedException;

/**
 * This class is an export service for i-Code CNES.
 * 
 * <p>
 * The methods of this class throw <tt>NoContributorMatchingException</tt> when
 * a function could not retrieve or reach intended data of a contributor. It
 * also throw <tt>NoIndicatedFormatInFileException</tt> when the file to export
 * has no extension.
 * </p>
 * 
 * @since 3.0
 * 
 */
public class ExportService implements IExportService {

    /** Export extension point attribute ExportClass */
    public static final String EXPORT_EXTENSIONPOINT_ATTRIBUTE_EXPORTCLASS = "ExportClass";

    /** Class name */
    private static final String CLASS = ExportService.class.getName();

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.export.IExportService#export(java.util.List,
     * java.io.File, java.util.Map)
     */
    @Override
    public void export(List<CheckResult> checkResults, File outputFile,
                    Map<String, String> parameters) throws NoContributorMatchingException,
                    NoExtensionIndicatedException, IOException, CoreException {
        export(checkResults, outputFile, parameters,
                        ExportUtils.getExtensionFromFilePath(outputFile.getAbsolutePath()));
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.export.IExportService#export(java.util.List,
     * java.io.File, java.util.Map, java.lang.String)
     */
    @Override
    public void export(List<CheckResult> pCheckResults, File pOutputFile,
                    Map<String, String> pParameters, String pFormat) throws NoContributorMatchingException,
                    NoExtensionIndicatedException, IOException, CoreException {
        final String method = "export";
        ICodeLogger.entering(CLASS, method);
        final IExporter exporter = getExportClass(pFormat);
        exporter.export(pCheckResults, pOutputFile, pParameters);
        ICodeLogger.exiting(CLASS, method);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * fr.cnes.analysis.tools.export.IExportService#getParameters(java.lang.
     * String)
     */
    @Override
    public Map<String, String> getParameters(String formatExtension)
                    throws NoContributorMatchingException, CoreException {
        final String method = "getParameters";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method);
        return getExportClass(formatExtension).getParameters();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * fr.cnes.analysis.tools.export.IExportService#hasParameters(java.lang.
     * String)
     */
    @Override
    public boolean hasParameters(String formatExtension)
                    throws NoContributorMatchingException, CoreException {
        final String method = "hasParameters";
        ICodeLogger.entering(CLASS, method, formatExtension);
        final boolean hasParams = getExportClass(formatExtension).hasParameters();
        ICodeLogger.exiting(CLASS, method, Boolean.valueOf(hasParams));
        return hasParams;
    }

    /**
     * This function browses {@value #EXPORT_EXTENSIONPOINT_ID} contributors and
     * return the first class implementing {@link IExporter} set for the format
     * matching {@code formatExtension} parameter requested.
     * 
     * @param formatExtension
     *            extension requested (without ".").
     * @return {@link IExporter} set by a contributor to export
     *         {@code formatExtension} requested.
     * @throws NoContributorMatchingException
     *             when there is no contributor of
     *             {@value #EXPORT_EXTENSIONPOINT_ID} able to export the
     *             requested format.
     * @throws CoreException
     *             when failing to create executable from contributor of
     *             {@link #EXPORT_EXTENSIONPOINT_ID}'s attribute
     *             {@link #EXPORT_EXTENSIONPOINT_ATTRIBUTE_EXPORTCLASS}.
     */
    private IExporter getExportClass(String formatExtension)
                    throws NoContributorMatchingException, CoreException {
        final String method = "getExportClass";
        ICodeLogger.entering(CLASS, method);
        /*
         * The export class to return from the contributors of the Extension
         * Point.
         */
        IExporter exportClass = null;
        for (IConfigurationElement contribution : ExportUtils.getContributions()) {
            if (contribution.getAttribute(ExportUtils.EXPORT_EXTENSIONPOINT_ATTRIBUTE_EXTENSION)
                            .equals(formatExtension)) {
                final Object o = contribution.createExecutableExtension(
                                EXPORT_EXTENSIONPOINT_ATTRIBUTE_EXPORTCLASS);
                if (o instanceof IExporter) {
                    exportClass = (IExporter) o;
                }
            }
        }

        if (exportClass == null) {
            final NoContributorMatchingException exception = new NoContributorMatchingException();
            ICodeLogger.throwing(CLASS, method, exception);
            throw exception;
        }
        ICodeLogger.exiting(CLASS, method, exportClass);
        return exportClass;
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.export.IExportService#getAvailableFormats()
     */
    @Override
    public Map<String, String> getAvailableFormats() {
        return ExportUtils.getAvailableFormats();
    }

}
