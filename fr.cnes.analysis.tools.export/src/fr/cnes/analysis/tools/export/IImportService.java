package fr.cnes.analysis.tools.export;

import java.io.File;
import java.util.List;
import java.util.Map;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.export.exception.NoContributorMatchingException;
import fr.cnes.analysis.tools.export.exception.NoExtensionIndicatedException;

public interface IImportService {

    /**
     * @param inputFile
     *            to realize the import on.
     * @return the list of {@link CheckResult} extracted from the
     *         {@code inputFile}.
     * @throws NoExtensionIndicatedException
     *             when the {@code inputFile} do not end with an extension.
     * @throws NoContributorMatchingException
     *             when the format of the {@code intputFile} is not handled by
     *             any contributor of the {@link #EXPORT_EXTENSIONPOINT_ID}
     *             contributors {@code ExtensionPoint}.
     */
    public List<CheckResult> importResults(final File inputFile)
                    throws NoExtensionIndicatedException, NoContributorMatchingException;

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

}
