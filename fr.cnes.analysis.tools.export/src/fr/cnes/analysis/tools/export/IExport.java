package fr.cnes.analysis.tools.export;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

/************************************************************************************************/
/* i-Code CNES is a static code analyzer. */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html */
/************************************************************************************************/

import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * This interface must be implemented by any class of {@code ExportClass}
 * attribute of the contributor of the {@code ExtensionPoint}
 * {@value Export#Export_ExtensionPoint_ID}.
 *
 * @since 3.0
 * 
 */
public interface IExport {

    /**
     * This function exports {@link Violation} of {@code violations} into
     * {@link File} indicated pending it's format.
     * 
     * @param checkResults
     *            to exports.
     * @param outputFile
     *            to write the violations.
     * @throws IOException
     *             when a {@link java.io.File} exception occur while using
     *             {@code outputFile}.
     */
    public void export(List<CheckResult> checkResults, File outputFile) throws IOException;

}
