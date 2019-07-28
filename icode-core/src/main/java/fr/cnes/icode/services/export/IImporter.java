/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.services.export;

import java.io.File;
import java.util.List;

import fr.cnes.icode.datas.CheckResult;

/**
 * This interface must be implemented by any class of {@code ImportClass}
 * attribute of the contributor of the {@code ExtensionPoint}
 * {@value ExportService#EXPORT_EXTENSIONPOINT_ID}.
 *
 * @since 3.0
 */
public interface IImporter {

    /**
     * @param inputFile
     *            containing the {@link CheckResult} to import.
     * @return the checkReults in a List of {@link CheckResult} data.
     */
    public List<CheckResult> importResults(File inputFile);

}
