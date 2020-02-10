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
 * This interface must be implemented by any class of {@code ExportClass}
 * attribute of the contributor of the {@code ExtensionPoint}.
 *
 * @since 3.0
 * 
 */
public interface IExporter {

    /** <i>i-CodeVersion</i> parameter. */
    public static final String PARAM_ICODE_VERSION = "icodeVersion";

    /**
     * This function export each {@link CheckResult} of {@code checkResults}
     * parameter into the {@link File} parameter.
     * 
     * @param checkResults
     *            to export.
     * @param outputFile
     *            to use for the export.
     * @param params
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
    public void export(List<CheckResult> checkResults, File outputFile, Map<String, String> params)
                    throws IOException, NoContributorMatchingException,
                    NoExtensionIndicatedException;

    /**
     * @return if the export function requires more parameter.
     */
    public boolean hasParameters();

    /**
     * @return required parameters.
     */
    public Map<String, String> getParameters();

    /**
     * @return name of the format.
     */
    public String getFormatName();

    /**
     * @return extension of the format.
     */
    public String getFormatExtension();

}
