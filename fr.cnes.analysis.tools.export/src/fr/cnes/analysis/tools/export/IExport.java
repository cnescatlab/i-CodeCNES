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

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

/**
 * This interface must be implemented by any class of {@code ExportClass}
 * attribute of the contributor of the {@code ExtensionPoint}
 * {@value Export#EXPORT_EXTENSIONPOINT_ID}.
 *
 * @since 3.0
 * 
 */
public interface IExport {

    /**
     * This function exports {@link CheckResult} of {@code checkResults} into
     * {@link File} indicated pending it's format.
     * 
     * @param checkResults
     *            to exports.
     * @param outputFile
     *            to write the {@link CheckResult}s.
     * @param params
     *            export plug-in required parameters
     * @throws IOException
     *             when a {@link java.io.File} exception occur while using
     *             {@code outputFile}.
     */
    public void export(List<CheckResult> checkResults, File outputFile, Map<String, String> params)
                    throws IOException;

    /**
     * @return if the export function requires more parameter.
     */
    public boolean hasParameters();

    /**
     * @return required parameters.
     */
    public Map<String, String> getParameters();

}
