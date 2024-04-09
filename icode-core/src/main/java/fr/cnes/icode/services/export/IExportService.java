/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.services.export;

import fr.cnes.icode.data.CheckResult;
import fr.cnes.icode.services.export.exception.NoContributorMatchingException;
import fr.cnes.icode.services.export.exception.NoExtensionIndicatedException;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

/**
 * Interface to implement to realize an export service.
 */
public interface IExportService {

    /**
     * This function export each {@link CheckResult} of {@code checkResults}
     * parameter into the {@link File} parameter.
     * 
     * @param checkResults
     *            to export.
     * @param outputFile
     *            to use for the export.
     * @param parameters
     *            parameter required by the export plugin
     * @throws NoContributorMatchingException
     *             when a format can not be handled by the @link
     *             {@link ExportService} service.
     * @throws NoExtensionIndicatedException
     *             when the {@code outputFile} has no extension indicated.
     * @throws IOException
     *             when the export failed due to a {@link java.io.File}
     *             exception.
     */
    public void export(List<CheckResult> checkResults, File outputFile,
                    Map<String, String> parameters) throws NoContributorMatchingException,
                    NoExtensionIndicatedException, IOException;

    /**
     * @param formatExtension
     *            the extension to retrieve from exporter plugin
     * @return if an exporter contributor requires parameters
     * @throws NoContributorMatchingException
     *             when the indicated format has no exporter defined.
     */
    public boolean hasParameters(String formatExtension)
                    throws NoContributorMatchingException;

    /**
     * This function return all available {@code formatName} and
     * {@code formatExtension} defined by contributors.
     * 
     * @return a Map with formatName as key and and formatExtension as value of
     *         every format handled by the contributor of the ExtensionPoint
     */
    public Map<String, String> getAvailableFormats();

    /**
     * 
     * @param formatExtension
     *            the extension to retrieve exporter plugin
     * @return the parameters of the plugin exporting the format requested
     * @throws NoContributorMatchingException
     *             when the indicated format has no exporter defined
     */
    public Map<String, String> getParameters(String formatExtension)
                    throws NoContributorMatchingException;

    /**
     * This function export each {@link CheckResult} of {@code checkResults}
     * parameter into the {@link File} parameter.
     * 
     * @param pCheckResults
     *            to export.
     * @param pOutputFile
     *            to use for the export.
     * @param pParameters
     *            parameter required by the export plugin.
     * @param pFormat
     * 			  define the format of the results file.
     * @throws NoContributorMatchingException
     *             when a format can not be handled by the @link
     *             {@link ExportService} service.
     * @throws NoExtensionIndicatedException
     *             when the {@code outputFile} has no extension indicated.
     * @throws IOException
     *             when the export failed due to a {@link java.io.File}
     *             exception.
     */
	void export(List<CheckResult> pCheckResults, File pOutputFile, Map<String, String> pParameters, String pFormat)
			throws NoContributorMatchingException, NoExtensionIndicatedException, IOException;
}
