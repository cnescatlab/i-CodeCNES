package fr.cnes.analysis.tools.export;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.export.exception.NoContributorMatchingException;
import fr.cnes.analysis.tools.export.exception.NoExtensionIndicatedException;

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
     * @throws CoreException
     *             when failing to create executable from contributor of
     *             {@link #EXPORT_EXTENSIONPOINT_ID}'s attribute
     *             {@link #EXPORT_EXTENSIONPOINT_ATTRIBUTE_EXPORTCLASS}.
     */
    public void export(List<CheckResult> checkResults, File outputFile,
                    Map<String, String> parameters) throws NoContributorMatchingException,
                    NoExtensionIndicatedException, IOException, CoreException;

    /**
     * @param formatExtension
     *            the extension to retrieve from exporter plugin
     * @return if an exporter contributor requires parameters
     * @throws NoContributorMatchingException
     *             when the indicated format has no exporter defined.
     * @throws CoreException
     *             when failing to create executable from contributor of
     *             {@link #EXPORT_EXTENSIONPOINT_ID}'s attribute
     *             {@link #EXPORT_EXTENSIONPOINT_ATTRIBUTE_EXPORTCLASS}.
     */
    public boolean hasParameters(String formatExtension)
                    throws NoContributorMatchingException, CoreException;

    /**
     * This function return all available {@code formatName} and
     * {@code formatExtension} defined by {@link #EXPORT_EXTENSIONPOINT_ID}
     * contributors.
     * 
     * @return a Map with formatName as key and and formatExtension as value of
     *         every format handled by the contributor of the ExtensionPoint
     *         {@link Export_ExtensionPoint_ID}
     */
    public Map<String, String> getAvailableFormats();

    /**
     * 
     * @param formatExtension
     *            the extension to retrieve exporter plugin
     * @return the parameters of the plugin exporting the format requested
     * @throws NoContributorMatchingException
     *             when the indicated format has no exporter defined
     * @throws CoreException
     *             when failing to create executable from contributor of
     *             {@link #EXPORT_EXTENSIONPOINT_ID}'s attribute
     *             {@link #EXPORT_EXTENSIONPOINT_ATTRIBUTE_EXPORTCLASS}.
     */
    public Map<String, String> getParameters(String formatExtension)
                    throws NoContributorMatchingException, CoreException;
}
