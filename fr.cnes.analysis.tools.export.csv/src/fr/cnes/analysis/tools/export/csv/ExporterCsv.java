/************************************************************************************************/
/* i-Code CNES is a static code analyzer. */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html */
/************************************************************************************************/
package fr.cnes.analysis.tools.export.csv;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.export.IExporter;

/**
 * This class is an attribute of the {@code ExtensionPoint} implementing
 * {@link IExporter} interface of the plugin
 * <i>fr.cnes.analysis.tools.export</i>.
 * <p>
 * This class is also exported in the <i>fr.cnes.analysis.tools.export.csv</i>
 * plugin and could be used as a service from any third.
 * </p>
 * <p>
 * This class is responsible of the export in the format CSV of
 * {@link CheckResult} elements into a {@link File}.
 * </p>
 * 
 * @since 3.0
 */
public class ExporterCsv implements IExporter {

    /** Field separator */
    private static final String SEPARATOR = "\t";
    /** Field separator */
    private static final String BLANK = "\n";

    /**
     * Default constructor. Required to execute a class from the contributed
     * extension point.
     */
    public ExporterCsv() {
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.export.IExport#export(java.util.List,
     * java.io.File)
     */
    @Override
    public void export(List<CheckResult> checkResults, File outputFile,
                    Map<String, String> parameters) throws IOException {
        try (final FileWriter writer = new FileWriter(outputFile)) {
            writer.write("Rule" + SEPARATOR + "File" + SEPARATOR + "Location" + SEPARATOR + "Line"
                            + SEPARATOR + "Message" + SEPARATOR + "Language" + SEPARATOR
                            + "Value\n");
            for (final CheckResult checkResult : checkResults) {
                if (checkResult.getLocation() != null || checkResult.getValue() != null) {
                    if (checkResult.getLocation() == null) {
                        writer.write(checkResult.getName() + SEPARATOR
                                        + checkResult.getFile().getAbsolutePath() + SEPARATOR + ""
                                        + SEPARATOR + checkResult.getLine().toString() + SEPARATOR
                                        + checkResult.getLangageId() + SEPARATOR + "" + SEPARATOR
                                        + checkResult.getValue() + BLANK);
                    } else if (checkResult.getValue() != null) {
                        writer.write(checkResult.getName() + SEPARATOR
                                        + checkResult.getFile().getAbsolutePath() + SEPARATOR + ""
                                        + SEPARATOR + checkResult.getLine().toString() + SEPARATOR
                                        + checkResult.getLangageId() + SEPARATOR + "" + SEPARATOR
                                        + checkResult.getValue() + BLANK);
                    } else {
                        writer.write(checkResult.getName() + SEPARATOR
                                        + checkResult.getFile().getAbsolutePath() + SEPARATOR
                                        + checkResult.getLocation() + SEPARATOR
                                        + checkResult.getLine().toString() + SEPARATOR
                                        + checkResult.getLangageId() + SEPARATOR
                                        + checkResult.getMessage().toString()
                                                        .replaceAll(SEPARATOR, "")
                                                        .replaceAll(BLANK, "")
                                        + SEPARATOR + "" + BLANK);
                    }
                }
            }
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.export.IExport#hasParameters()
     */
    @Override
    public boolean hasParameters() {
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.export.IExport#getParameters()
     */
    @Override
    public Map<String, String> getParameters() {
        return null;
    }

}
