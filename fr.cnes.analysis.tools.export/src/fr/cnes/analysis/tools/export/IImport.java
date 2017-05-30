package fr.cnes.analysis.tools.export;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

/************************************************************************************************/
/* i-Code CNES is a static code analyzer. */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html */
/************************************************************************************************/

import java.io.File;
import java.util.List;

/**
 * This interface must be implemented by any class of {@code ImportClass}
 * attribute of the contributor of the {@code ExtensionPoint}
 * {@value Export#Export_ExtensionPoint_ID}.
 *
 * @since 3.0
 */
public interface IImport {

    /**
     * @param inputFile
     *            containing the {@link Violation} to import.
     * @return the violations in a List of {@link Violation} data.
     */
    public List<CheckResult> importResults(File inputFile);

}
