/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.application;

import com.google.common.collect.Sets;
import fr.cnes.icode.Analyzer;
import fr.cnes.icode.application.exception.BadArgumentValueException;
import fr.cnes.icode.data.CheckResult;
import fr.cnes.icode.exception.JFlexException;
import fr.cnes.icode.services.checkers.CheckerContainer;
import fr.cnes.icode.services.checkers.CheckerService;
import fr.cnes.icode.services.export.ExportService;
import fr.cnes.icode.services.export.IExporter;
import fr.cnes.icode.services.export.exception.NoContributorMatchingException;
import fr.cnes.icode.services.export.exception.NoExtensionIndicatedException;
import fr.cnes.icode.services.languages.ILanguage;
import fr.cnes.icode.services.languages.LanguageService;
import org.apache.commons.io.FileUtils;

import java.io.*;
import java.util.*;
import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.Logger;

/**
 * This class provide a main command line application to
 * use i-Code in a console without GUI.
 */
public class ICodeApplication {

    /** Logger for the current class @see /logging.properties for more information. **/
    protected static final Logger LOGGER = Logger.getLogger(ICodeApplication.class.getName());

    // Static initialization to set logging configuration.
    static {
        // Configure logging system
        try (InputStream fis = ICodeApplication.class.getResourceAsStream("/logging.properties")) {
            LogManager manager = LogManager.getLogManager();
            manager.readConfiguration(fis);
            ConsoleHandler handler = new ConsoleHandler();
            handler.setFormatter(new DisplayFormatter());
            LOGGER.addHandler(handler);
        } catch (IOException e) {
            throw new ExceptionInInitializerError(e);
        }
        // Configure temp files: creation of folder ~/.icode/log to contain log files
        (new File(FileUtils.getUserDirectory().getPath().concat("/.icode/log"))).mkdirs();
    }

    /** Contains language's name as key and language's id as key. **/
    private Map<String, String> languages;

    /** Manage the parsing and formatting of data provided by cli. **/
    private CommandLineManager cli;

    /** Needed services for running i-Code. **/
    private ExportService exportService;

    /** Set an analyzer. **/
    private Analyzer analyzer;

    /** List of language id (not name). **/
    private List<String> checkedLanguages;

    /** List of rule id. **/
    private List<String> excludedRules;

    /** Format of the export (xml by default). **/
    private String exportFormat;

    /** File where write results (a temp file is created by default). **/
    private File outputFile;

    /** Boolean true if output is the console. **/
    private boolean outputToStdOut;

    /** Default constructor to set available languages data. **/
    public ICodeApplication() {
        super();

        // Initialize services.
        cli = new CommandLineManager();
        exportService = new ExportService();
        analyzer = new Analyzer();

        // Analysis parameters with default values
        languages = new HashMap<>();
        checkedLanguages = LanguageService.getLanguagesIds();
        excludedRules = new ArrayList<>();

        // default export format is set to XML
        exportFormat = "xml";

        // init a temp file to retrieve results for displaying on screen
        try {
            outputFile = File.createTempFile("icode", ".res");
            outputFile.deleteOnExit();
        } catch (IOException e) {
            LOGGER.warning(e.getLocalizedMessage());
        }

        // by default results are printed to screen
        outputToStdOut = true;

        // build map for translating languages' name to id
        for (final ILanguage language : LanguageService.getLanguages()) {
            languages.put(language.getName(), language.getId());
        }
    }

    /**
     * Main program's entry.
     *
     * @param args Arguments passed in the command line.
     */
    public static void main(final String[] args) {

        final ICodeApplication application = new ICodeApplication();

        application.runICode(args);

    }

    /**
     * Run i-Code analysis.
     *
     * @param args Arguments passed in the command line.
     * @throws IOException When reading files.
     */
    public void runICode(String[] args) {
        try {
            // Parse the command line arguments.
            if(cli.parse(args)) {
                // Get list of filenames.
                final String[] arguments = cli.getArgs().toArray(new String[0]);

                // Get list of files to analyze.
                final Set<File> sources = getFiles(arguments);

                // Handle options.
                if (cli.hasOption(CommandLineManager.EXPORTERS)) {
                    // display all available exporters
                    displayList(new ArrayList(exportService.getAvailableFormats().values()),
                            "List of available exporters for analysis:");
                } else if (cli.hasOption(CommandLineManager.LANGUAGES)) {
                    // display all available languages
                    displayList(languages.keySet(),
                            "List of available languages for analysis:");
                } else if (cli.hasOption(CommandLineManager.RULES)) {
                    // display all available checks by language
                    for (final ILanguage language : LanguageService.getLanguages()) {
                        displayList(CheckerService.getCheckersIds(language.getId()),
                                String.format("List of available rules for %s [%s]:", language.getName(), language.getId()));
                    }
                } else if (cli.hasOption(CommandLineManager.LIST_EXPORT_PARAMETERS)) {
                    displayAvailableExportFormat();
                } else {
                    runAnalysis(sources);
                }
            }
        } catch (final NoContributorMatchingException | NoExtensionIndicatedException | JFlexException |
                BadArgumentValueException | IOException e) {
            LOGGER.log(Level.SEVERE, e.getMessage(), e);
        }
    }

    /**
     * Run i-Code analysis on provided files.
     *
     * @param sources Files to analyze.
     * @throws BadArgumentValueException      On user error.
     * @throws JFlexException                 On Jflex error.
     * @throws NoContributorMatchingException On missing element.
     * @throws NoExtensionIndicatedException  On missing extension.
     * @throws IOException                    On I/O error.
     */
    public void runAnalysis(final Set<File> sources) throws BadArgumentValueException, JFlexException, NoContributorMatchingException, NoExtensionIndicatedException, IOException {
        Map<String, String> exporterParameters;// Get list of languages id to check.
        if (cli.hasOption(CommandLineManager.CHECKED_LANGUAGES)) {
            checkedLanguages = Arrays.asList(cli.getOptionValue(CommandLineManager.CHECKED_LANGUAGES).split(","));
            for (final String id : checkedLanguages) {
                if (!languages.containsKey(id)) {
                    String message = String.format("Language '%s' is not available in i-Code.", id);
                    throw new BadArgumentValueException(message);
                }
            }
            // Translate Name to Id
            for (int i = 0; i < checkedLanguages.size(); i++) {
                checkedLanguages.set(i, languages.get(checkedLanguages.get(i)));
            }
        }
        // Get list of excluded rules.
        if (cli.hasOption(CommandLineManager.EXCLUDED_RULES)) {
            List<CheckerContainer> checkers = CheckerService.getCheckers();
            Iterator<CheckerContainer> iterator;
            for (final String rule : cli.getOptionValue(CommandLineManager.EXCLUDED_RULES).split(",")) {
                iterator = checkers.iterator();
                boolean found = false;
                while (iterator.hasNext() && !found) {
                    CheckerContainer checker = iterator.next();
                    found = checker.getId().equals(rule);
                }

                // if the rule does not exist, just warn the user
                if (!found) {
                    String message = String.format("Rule '%s' is not available in i-Code.", rule);
                    LOGGER.warning(message);
                } else { // otherwise we add it to skipped checks
                    excludedRules.add(rule);
                }
            }
        }
        // Get export format from command line.
        if (cli.hasOption(CommandLineManager.EXPORT_FORMAT)) {
            exportFormat = cli.getOptionValue(CommandLineManager.EXPORT_FORMAT);
            if (!exportService.getAvailableFormats().containsValue(exportFormat)) {
                String message = String.format("Export in format '%s' is not available in i-Code.", exportFormat);
                throw new BadArgumentValueException(message);
            }
        }
        // Get the filename for result file and prevent i-Code to write in stdout.
        if (cli.hasOption(CommandLineManager.OUTPUT)) {
            outputFile = new File(cli.getOptionValue(CommandLineManager.OUTPUT));
            outputToStdOut = false;
        }

        // Run the analysis.
        final List<CheckResult> checkResults = analyzer.stableCheck(sources, checkedLanguages, excludedRules);

        // Get default parameters for the chosen export.
        exporterParameters = exportService.getParameters(exportFormat);
        exporterParameters.put(IExporter.PARAM_ICODE_VERSION, getClass().getPackage().getImplementationVersion());

        // Add user parameters if there are some.
        if (cli.hasOption(CommandLineManager.EXPORT_PARAMETERS)) {
            setExporterParameters(exporterParameters);
        }

        // Export results to a file.
        exportService.export(checkResults, outputFile, exporterParameters, exportFormat);

        // Display data to standard output if no file is asked by the user.
        if (outputToStdOut) {
            printToStdOut();
        }
    }

    /**
     * Display available export formats as list in stdout.
     *
     * @throws NoContributorMatchingException Occur on missing contributor.
     * @throws BadArgumentValueException      Occur on bad argument.
     */
    public void displayAvailableExportFormat() throws NoContributorMatchingException, BadArgumentValueException {
        Map<String, String> exporterParameters;// Get format as value of the option.
        exportFormat = cli.getOptionValue(CommandLineManager.LIST_EXPORT_PARAMETERS);

        if (exportService.getAvailableFormats().containsValue(exportFormat)) {
            // Get default parameters for the chosen export.
            exporterParameters = exportService.getParameters(exportFormat);

            // Security in the case of a null return.
            if (exporterParameters == null) {
                exporterParameters = new HashMap<>();
            }

            // display all available languages
            displayList(exporterParameters.keySet(),
                    String.format("List of available parameters for %s export:", exportFormat));
        } else {
            String message = String.format("Exporting in format '%s' is not available in i-Code.", exportFormat);
            throw new BadArgumentValueException(message);
        }
    }

    /**
     * Configure parameters for the selected exporter.
     *
     * @param exporterParameters Parameters from the command line.
     * @throws BadArgumentValueException Occurs when bad parameter is provided.
     */
    public void setExporterParameters(final Map<String, String> exporterParameters) throws BadArgumentValueException {
        // Split all pairs of key=value.
        final String[] params = cli.getOptionValue(CommandLineManager.EXPORT_PARAMETERS).split(",");

        // For each key=value.
        for (final String param : params) {
            // Split key from the value.
            final String[] values = param.split("=");
            // There should be 2 parts (key and value).
            if (values.length == 2) {
                // If the key exist in map of default parameters we add the parameter to it.
                if (exporterParameters.containsKey(values[0])) {
                    exporterParameters.put(values[0], values[1]);
                } else {
                    String message = String.format("Export parameter '%s' is not a valid parameter for %s export.",
                            values[0], exportFormat);
                    throw new BadArgumentValueException(message);
                }
            } else {
                String message = String.format("Export parameter '%s' is malformed.", param);
                throw new BadArgumentValueException(message);
            }
        }
    }

    /**
     * Print results to stdout.
     *
     * @throws IOException Occur when there is stdout problems.
     */
    public void printToStdOut() throws IOException {
        try (BufferedReader br = new BufferedReader(new FileReader(outputFile))) {
            String line;
            while ((line = br.readLine()) != null) {
                LOGGER.info(line);
            }
        }
    }

    /**
     * Print a formatted list to the log output with an optional header.
     *
     * @param list   List<String> containing data to print.
     * @param header String which can be printed before the list.
     */
    private void displayList(final Collection<String> list, final String header) {
        // if header exists, we print it
        if (header != null && !header.isEmpty()) {
            LOGGER.info(header);
        }

        // print each line of a list
        if (list != null) {
            for (final String item : list) {
                LOGGER.info(String.format("+ %s", item));
            }
        }
    }

    /**
     * Retrieve files to analyze from list of path with wildcards.
     *
     * @param pFilenames Array containing path of researched files.
     * @return List of found files.
     */
    private Set<File> getFiles(final String[] pFilenames) {
        // Set default filenames.
        String[] filenames = pFilenames;
        if(Objects.isNull(filenames) || 0==filenames.length) {
            filenames = new String[]{"."};
        }
        return openFiles(filenames);
    }

    /**
     * Open files given as argument.
     *
     * @param filenames Filenames of files to open.
     * @return result parameter completed with new opened files.
     */
    private Set<File> openFiles(final String[] filenames) {
        // List of files found (to be returned)
        final Set<File> result = Sets.newHashSet();
        // Temporary file
        File file;
        for (final String name : filenames) {
            file = new File(name);
            if (!file.exists()) {
                LOGGER.warning(String.format("File not found: %s", name));
            } else if (file.isDirectory()) {
                result.addAll(openFiles(file.listFiles()));
            } else {
                result.add(file);
            }
        }
        return result;
    }

    /**
     * Open files given as argument.
     *
     * @param files Files to open.
     * @return result parameter completed with new opened files.
     */
    private Set<File> openFiles(final File[] files) {
        // List of files found (to be returned)
        final Set<File> result = Sets.newHashSet();
        for (final File file : files) {
            if (!file.exists()) {
                LOGGER.warning(String.format("File not found: %s", file.getName()));
            } else if (file.isDirectory()) {
                result.addAll(openFiles(file.listFiles()));
            } else {
                result.add(file);
            }
        }
        return result;
    }

}
