package fr.cnes.analysis.tools.analyzer.services.export;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.services.export.exception.NoContributorMatchingException;
import fr.cnes.analysis.tools.analyzer.services.export.exception.NoExtensionIndicatedException;

/**
 * Interface to implement to realize an fr.cnes.analysis.tools.analyzer.services.export service.
 */
public interface IExportService {

    /**
     * This function fr.cnes.analysis.tools.analyzer.services.export each {@link CheckResult} of {@code checkResults}
     * parameter into the {@link File} parameter.
     * 
     * @param checkResults
     *            to fr.cnes.analysis.tools.analyzer.services.export.
     * @param outputFile
     *            to use for the fr.cnes.analysis.tools.analyzer.services.export.
     * @param parameters
     *            parameter required by the fr.cnes.analysis.tools.analyzer.services.export plugin
     * @throws NoContributorMatchingException
     *             when a format can not be handled by the @link
     *             {@link ExportService} service.
     * @throws NoExtensionIndicatedException
     *             when the {@code outputFile} has no extension indicated.
     * @throws IOException
     *             when the fr.cnes.analysis.tools.analyzer.services.export failed due to a {@link java.io.File}
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
     * This function fr.cnes.analysis.tools.analyzer.services.export each {@link CheckResult} of {@code checkResults}
     * parameter into the {@link File} parameter.
     * 
     * @param pCheckResults
     *            to fr.cnes.analysis.tools.analyzer.services.export.
     * @param pOutputFile
     *            to use for the fr.cnes.analysis.tools.analyzer.services.export.
     * @param pParameters
     *            parameter required by the fr.cnes.analysis.tools.analyzer.services.export plugin.
     * @param pFormat
     * 			  define the format of the results file.
     * @throws NoContributorMatchingException
     *             when a format can not be handled by the @link
     *             {@link ExportService} service.
     * @throws NoExtensionIndicatedException
     *             when the {@code outputFile} has no extension indicated.
     * @throws IOException
     *             when the fr.cnes.analysis.tools.analyzer.services.export failed due to a {@link java.io.File}
     *             exception.
     */
	void export(List<CheckResult> pCheckResults, File pOutputFile, Map<String, String> pParameters, String pFormat)
			throws NoContributorMatchingException, NoExtensionIndicatedException, IOException;
}
