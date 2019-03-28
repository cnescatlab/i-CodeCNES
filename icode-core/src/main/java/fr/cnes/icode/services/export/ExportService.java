/************************************************************************************************/
/* i-Code CNES is a static code analyzer. */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html */
/************************************************************************************************/
package fr.cnes.icode.services.export;

import java.io.File;
import java.io.IOException;
import java.util.*;

import fr.cnes.icode.datas.CheckResult;
import fr.cnes.icode.logger.ICodeLogger;
import fr.cnes.icode.reflexion.ClassFinder;
import fr.cnes.icode.services.export.exception.NoContributorMatchingException;
import fr.cnes.icode.services.export.exception.NoExtensionIndicatedException;

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

    /** Class name */
    private static final String CLASS = ExportService.class.getName();

    /*
     * (non-Javadoc)
     * 
     * @see IExportService#export(java.util.List,
     * java.io.File, java.util.Map)
     */
    @Override
    public void export(List<CheckResult> checkResults, File outputFile,
                       Map<String, String> parameters) throws NoContributorMatchingException,
                    NoExtensionIndicatedException, IOException {
        export(checkResults, outputFile, parameters,
                        ExportUtils.getExtensionFromFilePath(outputFile.getAbsolutePath()));
    }

    /*
     * (non-Javadoc)
     * 
     * @see IExportService#export(java.util.List,
     * java.io.File, java.util.Map, java.lang.String)
     */
    @Override
    public void export(List<CheckResult> pCheckResults, File pOutputFile,
                    Map<String, String> pParameters, String pFormat) throws NoContributorMatchingException,
                    NoExtensionIndicatedException, IOException {
        final String method = "fr/cnes/icode/services/export";
        ICodeLogger.entering(CLASS, method);
        final IExporter exporter = getExportClass(pFormat);
        exporter.export(pCheckResults, pOutputFile, pParameters);
        ICodeLogger.exiting(CLASS, method);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * IExportService#getParameters(java.lang.
     * String)
     */
    @Override
    public Map<String, String> getParameters(String formatExtension)
                    throws NoContributorMatchingException {
        final String method = "getParameters";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method);
        return getExportClass(formatExtension).getParameters();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * IExportService#hasParameters(java.lang.
     * String)
     */
    @Override
    public boolean hasParameters(String formatExtension)
                    throws NoContributorMatchingException {
        final String method = "hasParameters";
        ICodeLogger.entering(CLASS, method, formatExtension);
        final boolean hasParams = getExportClass(formatExtension).hasParameters();
        ICodeLogger.exiting(CLASS, method, hasParams);
        return hasParams;
    }

    /**
     * This function browses contributors and
     * return the first class implementing {@link IExporter} set for the format
     * matching {@code formatExtension} parameter requested.
     * 
     * @param formatExtension
     *            extension requested (without ".").
     * @return {@link IExporter} set by a contributor to export
     *         {@code formatExtension} requested.
     * @throws NoContributorMatchingException
     *             when there is no contributor of able to export the
     *             requested format.
     */
    private IExporter getExportClass(final String formatExtension)
                    throws NoContributorMatchingException {
        final String method = "getExportClass";
        ICodeLogger.entering(CLASS, method);
        /*
         * The export class to return from the contributors of the Extension
         * Point.
         */
        IExporter exportClass = null;

        try {
            final Set<Class<?>> classes = ClassFinder.find(IExporter.class);
            final Iterator<Class<?>> iterator = classes.iterator();
            while(iterator.hasNext() && null==exportClass) {
                final IExporter exporter = (IExporter) iterator.next().newInstance();
                if(exporter.getFormatExtension().equals(formatExtension)) {
                    exportClass = exporter;
                }
            }
        } catch (final Exception e) {
            ICodeLogger.error(CLASS, method, e);
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
     * @see IExportService#getAvailableFormats()
     */
    @Override
    public Map<String, String> getAvailableFormats() {
        return ExportUtils.getAvailableFormats();
    }

}
