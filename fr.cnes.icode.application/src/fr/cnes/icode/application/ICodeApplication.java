package fr.cnes.icode.application;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Supplier;

import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.apache.tools.ant.DirectoryScanner;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.equinox.app.IApplication;
import org.eclipse.equinox.app.IApplicationContext;
import org.osgi.framework.Bundle;
import org.osgi.framework.FrameworkUtil;

import fr.cnes.analysis.tools.analyzer.Analyzer;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.services.languages.LanguageService;
import fr.cnes.analysis.tools.export.IExporter;
import fr.cnes.analysis.tools.export.csv.ExporterCsv;
import fr.cnes.analysis.tools.export.xml.ExporterXml;

/**
 * This is the application to launch icode with a line command. It can be launch
 * with :
 * 
 * icode myfile -> generate the result by default in XML format on sysout icode
 * -f csv myfile -> generate the result by default in CSV format on sysout icode
 * -output result.xml myfile -> generate the result.xml file for myfile icode
 * -output result.csv myfile -> generate the result.csv file in CSV format
 * 
 * The same can be done with as many files as necessary using pattenr : icode
 * *.f
 * 
 * @author olivier
 *
 */
public class ICodeApplication implements IApplication {

    private static final String FORMAT2HTML_XSLT = "format2html.xslt";

    // -----------------------------------------------
    // Define the parameters for this application
    // -----------------------------------------------
    /** help argument to display syntax */
    public static final String ARG_HELP = "-help"; //$NON-NLS-1$

    /** Argument for verbose */
    public static final String ARG_VERBOSE = "-v"; //$NON-NLS-1$

    /**
     * The output format for the result: can be -f xml or -f csv.. default is
     * XML
     */
    public static final String ARG_OUTPUT_FORMAT = "-f"; //$NON-NLS-1$
    /** Expected value xml for output format */
    public static final String ARG_OUTPUT_FORMAT_XML = "xml"; //$NON-NLS-1$
    /** Expected value csv for output format */
    public static final String ARG_OUTPUT_FORMAT_CSV = "csv"; //$NON-NLS-1$

    /** The optional output filename */
    public static final String ARG_OUTPUT_FILE = "-output"; //$NON-NLS-1$

    /** The optional output HTML filename */
    public static final String ARG_HTML_OUTPUT_FILE = "-htmlOutput"; //$NON-NLS-1$

    /** The optional html output format (only if xml is used) */
    public static final String ARG_HTML = "-html"; //$NON-NLS-1$

    // --------------------------------------------------------------------------------------
    // Specific additional parameters only relevant for the XML output (no
    // effect on
    // CSV)
    // --------------------------------------------------------------------------------------
    /** The optional author of analysis (for XML output) */
    public static final String ARG_AUTHOR = "-author"; //$NON-NLS-1$
    /** The optional project name (for XML output) */
    public static final String ARG_PROJECT = "-project"; //$NON-NLS-1$
    /** The optional project version (for XML output) */
    public static final String ARG_PROJECT_VERSION = "-projectVersion"; //$NON-NLS-1$
    /** The optional configuration ID (for XML output) */
    public static final String ARG_CONFIG_ID = "-configID"; //$NON-NLS-1$

    private List<String> availableArgs = Arrays.asList(new String[] {
        ARG_HELP, ARG_VERBOSE, ARG_OUTPUT_FORMAT, ARG_OUTPUT_FILE, ARG_AUTHOR, ARG_PROJECT,
        ARG_PROJECT_VERSION, ARG_CONFIG_ID, ARG_HTML_OUTPUT_FILE, ARG_HTML
    });

    // List args that are used alone (no parameters)
    private List<String> singleArgs = Arrays.asList(new String[] {
        ARG_HELP, ARG_VERBOSE, ARG_HTML
    });

    /** output format to be used by default is XML */
    private String outputFormat = ARG_OUTPUT_FORMAT_XML;

    /** output filename to be used, by default sysout */
    private String outputFilename = null;

    /** Output file depending on -output or temporary file */
    private File outputFile = null;

    /** HTML output filename to be used, by default sysout */
    private String htmlOutputFilename = null;

    /** Real HTML file created (with parameter or temporary) */
    private File htmlOutputFile = null;

    /** verbose mode (default is false) */
    private boolean verbose = false;

    /** HTML output (default is false) */
    private boolean htmlOutput = false;

    public boolean isHtmlOutput() {
        return htmlOutput;
    }

    public void setHtmlOutput(boolean htmlOutput) {
        this.htmlOutput = htmlOutput;
    }

    /** config ID */
    private String configID = null;

    private String projectName, projectVersion, author;

    private String[] args;

    private List<String> filenames = new ArrayList<String>();

    // Logger information
    private ILog log;
    private String pluginId = FrameworkUtil.getBundle(getClass()).getSymbolicName();

    // The current exporter to use.
    private IExporter exporter;

    @Override
    public Object start(IApplicationContext context) throws Exception {

        args = (String[]) context.getArguments().get(IApplicationContext.APPLICATION_ARGS);
        log = Platform.getLog(FrameworkUtil.getBundle(getClass()));

        getBooleanArg(ARG_VERBOSE, v -> setVerbose(v));
        getBooleanArg(ARG_HTML, v -> setHtmlOutput(v));

        if (verbose)
            info("Running ICode Analyzer ");

        // -------------------------------------
        // 1. Check for help (will exit if found)
        // -------------------------------------
        if (args.length == 0)
            setHelp(true);
        getBooleanArg(ARG_HELP, v -> setHelp(v));

        // -----------------------------------------------------
        // 2. Extract filenames from command line and check args
        // -----------------------------------------------------
        int indexFiles = 0;
        for (String s : args) {
            // Consider to have the list of files at the end of the command line
            // after the
            // last argument
            // Consider also to have only arguments with values : -f xml -output
            // filename
            // ....
            // No single argument without values
            // Skip all arguments to get filename index
            if (s.startsWith("-")) {
                indexFiles += ((singleArgs.contains(s) ? 1 : 2)); // Add 1 for
                                                                  // single
                                                                  // arguments,
                                                                  // else add 2
                // Check if argument exists
                if (!availableArgs.contains(s)) {
                    warning("The argument name '" + s
                                    + "' is not expected for this command. Check help : ");
                    setHelp(true);
                }
            }
        }

        for (int a = indexFiles; a < args.length; a++)
            filenames.add(args[a]);

        // Resolve the file names
        List<File> files = resolveFileNames();

        // Call help if no file names
        if (files.size() == 0) {
            warning("No input files (or non existing file(s)) in your command arguments.");
            setHelp(true);
        }

        if (verbose) {
            info("  -> Filenames :" + filenames);
            for (File f : files)
                System.out.println("\t\t\tReal file : " + f.getAbsolutePath()
                                + (f.exists() ? "" : " -> Warning : does not exists"));
        }

        // -------------------------------------
        // 3. Get other arguments
        // -------------------------------------
        getArgValue(ARG_OUTPUT_FORMAT, v -> setOutputFormat(v));
        getArgValue(ARG_OUTPUT_FILE, v -> setOutputFilename(v));
        getArgValue(ARG_HTML_OUTPUT_FILE, v -> setHtmlOutputFilename(v));
        getArgValue(ARG_AUTHOR, v -> setAuthor(v));
        getArgValue(ARG_PROJECT, v -> setProjectName(v));
        getArgValue(ARG_PROJECT_VERSION, v -> setProjectVersion(v));
        getArgValue(ARG_CONFIG_ID, v -> setConfigID(v));

        // Display additional verbose information if optional parameters were
        // missing
        if (verbose) {
            if (outputFilename == null)
                info("  -> Output filename : " + getOutputFile().getAbsolutePath());
            if (outputFormat == ARG_OUTPUT_FORMAT_XML && htmlOutputFilename == null)
                info("  -> HTML Output filename (tmp) : " + getHtmlOutputFile().getAbsolutePath());

        }

        // -------------------------------------------------------------------
        // 4. Can now launch the job using the Analyzer and export the results
        // -------------------------------------------------------------------
        Analyzer analyzer = new Analyzer();

        List<CheckResult> checks = analyzer.check(files, LanguageService.getLanguagesIds(), null);
        exporter = getExporter();

        File outputFile = getOutputFile();
        if (outputFile == null) {
            warning("Unable to create temporary file, use -output option");
            setHelp(true);
        }

        exporter.export(checks, outputFile, exporter.getParameters());

        // print the file content if no outputfilename (-> temporary file)
        if (outputFilename == null) {
            displayFile(outputFile);
        }

        // Try to launch HTML processing
        if (isHtmlOutput()) {
            generateHtml(outputFile);
            // Print the HTML result if any and if no XML already displayed and
            // no HTML
            // output provided
            if (outputFilename != null && htmlOutputFilename == null)
                displayFile(getHtmlOutputFile());
        }

        return EXIT_OK;
    }

    private void generateHtml(File xmlInputFile) throws TransformerFactoryConfigurationError,
                    URISyntaxException, TransformerConfigurationException, TransformerException {
        TransformerFactory factory = TransformerFactory.newInstance();
        Bundle b = FrameworkUtil.getBundle(getClass());
        URL xslUrl = b.getEntry(FORMAT2HTML_XSLT);

        File dataFile = null;
        try {
            dataFile = new File(FileLocator.resolve(xslUrl).toURI());
        } catch (URISyntaxException e1) {
            e1.printStackTrace();
        } catch (IOException e1) {
            e1.printStackTrace();
        }

        Source xslt = new StreamSource(dataFile);
        Transformer transformer = factory.newTransformer(xslt);

        Source text = new StreamSource(xmlInputFile);
        File result = getHtmlOutputFile();
        transformer.transform(text, new StreamResult(result));
    }

    private void displayFile(File outputFile) {
        try {
            System.out.print(new String(Files.readAllBytes(outputFile.toPath())));
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    private void setHelp(Boolean help) {
        System.out.println("Minimal Usage : \n\n\ticode files...\n");
        System.out.println(
                        "This will print the result in console using the default output in XML format.\n");
        System.out.println(
                        "Files can be either absolute paths or patterns like : \t icode *.f90 ~/tmp/**/*.f77 /tmp/myfile.f77\n");
        System.out.println("Examples :  ");
        System.out.println("*.f90           : all the f90 files in current directory ");
        System.out.println(
                        "~/tmp/**/*.f77  : all files with f77 extension in $HOME/tmp and any of its sub directoryies");
        System.out.println("/tmp/myfile.f77 : the specific file /tmp/myfile.f77");
        System.out.println("\nAvailable options : ");
        System.out.println(
                        "\t-v : verbose, will display some traces, namely the files found in pattern");
        System.out.println("\t-f [xml | csv] : output format. Can be xml or csv.");
        System.out.println(
                        "\t-output filename : optional, filename to store the xml or csv result. If none will be displayed in shell ");
        System.out.println("\t-html : display html output (only for xml output with -output) ");
        System.out.println(
                        "\t-htmlOutput filename : optional, filename to store the html transformation");
        System.out.println(
                        "\nFor XML output format these optional parameters can be used in any order : \n");
        System.out.println(
                        "\t\t-project yourProject -projectVersion yourVersion -author yourName -configID yourID \n");
        System.out.println(
                        "\nFor HTML output format it is possible to specify the html output file. \nIf none and no xml output file provided, html result will be displayed in shell \n");
        System.out.println("The XML and HTML parameters are not used by CSV format\n");
        System.out.println("Examples:");
        System.out.println("\ticode  -f xml -output result.xml *.f ");
        System.out.println("\ticode  -f xml -author Me -output result.xml *.f ");
        System.out.println("\ticode  -f xml -author Me -project MyProject -output result.xml *.f ");
        System.out.println(
                        "\ticode  -f xml -author Me -project MyProject -projectVersion 1.2.0 -output result.xml *.f ");

        System.out.println("\ticode -html *.f ");
        System.out.println("\ticode -f xml -output result.xml -html *.f ");
        System.out.println("\ticode -f xml -output result.xml -html -htmlOutput result.html *.f ");

        System.exit(0);
    }

    /** Resolve the files if there are wildcards inside */
    private List<File> resolveFileNames() {
        List<File> result = new ArrayList<>();

        // Convert parameters into array of Strings for scanner
        String[] names = new String[filenames.size()];
        int i = 0;
        for (String n : filenames) {
            names[i++] = n;
        }

        DirectoryScanner scanner = new DirectoryScanner();
        scanner.setIncludes(names);
        // Scan with absolute path in scan
        scanner.setCaseSensitive(false);
        scanner.scan();
        scanFiles(scanner, result);
        // Scan again with basedir for relative path in scan
        scanner.setBasedir(new File("."));
        if (verbose)
            info("Base dir for scan is : " + scanner.getBasedir().getAbsolutePath());
        scanner.scan();
        scanFiles(scanner, result);

        return result;

    }

    private void scanFiles(DirectoryScanner scanner, List<File> result) {
        for (String name : scanner.getIncludedFiles()) {
            File f = new File(name);
            if (!f.exists())
                warning("\t\tthe file '" + name + "' does not exist");
            else
                result.add(f);
        }
    }

    private void setOutputFilename(String v) {
        if ((v != null) && v.length() > 0)
            outputFilename = v;
        else
            outputFilename = null;

        if (verbose)
            info("  -> Output filename : " + outputFilename);

    }

    public String getHtmlOutputFilename() {
        return htmlOutputFilename;
    }

    public void setHtmlOutputFilename(String v) {
        if ((v != null) && v.length() > 0)
            htmlOutputFilename = v;
        else
            htmlOutputFilename = null;

        if (verbose)
            info("  -> HTML Output filename : " + htmlOutputFilename);
    }

    private File getOutputFile() {
        if (outputFile == null) {
            if (outputFilename != null)
                outputFile = new File(outputFilename);
            else {
                try {
                    outputFile = File.createTempFile("icodeResult", outputFormat);
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
        return outputFile;
    }

    private File getHtmlOutputFile() {
        if (htmlOutputFile == null) {
            if (htmlOutputFilename != null)
                htmlOutputFile = new File(htmlOutputFilename);
            else {
                try {
                    htmlOutputFile = File.createTempFile("icodeResult", ".html");
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
        return htmlOutputFile;
    }

    /** Set the config file if any */
    private void setConfigID(String v) {
        configID = v;
    }

    private String getConfigID() {
        return configID;
    }

    private void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    private void setProjectVersion(String projectVersion) {
        this.projectVersion = projectVersion;
    }

    private void setAuthor(String author) {
        this.author = author;
    }

    private String getProjectName() {
        return projectName;
    }

    private String getAuthor() {
        return author;
    }

    public String getProjectVersion() {
        return projectVersion;
    }

    public boolean isVerbose() {
        return verbose;
    }

    public void setVerbose(boolean verbose) {
        this.verbose = verbose;
    }

    private void setOutputFormat(String value) {
        if (ARG_OUTPUT_FORMAT_CSV.equals(value)) {
            outputFormat = ARG_OUTPUT_FORMAT_CSV;
            // No html output for csv
            if (verbose && htmlOutput)
                info("  -> The -html argument is useless. Can be applied only if output is XML");
            htmlOutput = false;
        } else if (ARG_OUTPUT_FORMAT_XML.equals(value)) {
            outputFormat = ARG_OUTPUT_FORMAT_XML;
        } else
            warning("Parameter for outputFileFormat (-f) must be 'csv' or 'xml', default is 'xml'");

        if (verbose)
            info("  -> Output format :  " + outputFormat);

    }

    private IExporter getExporter() {
        IExporter result = null;
        if (outputFormat == ARG_OUTPUT_FORMAT_XML) {
            result = new ExporterXml();
            configureXMLExporter(result, ExporterXml.PARAM_AUTHOR, () -> getAuthor());
            configureXMLExporter(result, ExporterXml.PARAM_PROJECT_NAME, () -> getProjectName());
            configureXMLExporter(result, ExporterXml.PARAM_PROJECT_VERSION,
                            () -> getProjectVersion());
            configureXMLExporter(result, ExporterXml.PARAM_CONFIGURATION_ID, () -> getConfigID());
        } else {
            result = new ExporterCsv();
        }

        return result;
    }

    private void configureXMLExporter(IExporter exporter, String propertyName,
                    Supplier<String> supplier) {
        String value = supplier.get();
        if (value != null)
            exporter.getParameters().put(propertyName, value);

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.equinox.app.IApplication#stop()
     */
    @Override
    public void stop() {
        // TODO Auto-generated method stub

    }

    private void warning(String message) {
        log.log(new Status(Status.WARNING, pluginId, message));
        System.out.println("\n Warning : " + message + "\n");
    }

    private void info(String message) {
        System.out.println(message);
    }

    /**
     * This method extract an argument value and set it on the appropriate
     * method
     * 
     * @param argName
     *            argument name followed by a value
     * @param consumer
     *            the method to call to initialize the value
     */
    private void getArgValue(String argName, Consumer<? super String> consumer) {

        for (int i = 0; i < args.length; i++) {
            if (argName.equals(args[i]) && i + 1 < args.length) {
                consumer.accept(args[i + 1]);
                break;
            }
        }

    }

    /**
     * This method extract a boolean argument and set it on the appropriate
     * method
     * 
     * @param argName
     *            argument name followed by a value
     * @param consumer
     *            the method to call to initialize the value
     */
    private void getBooleanArg(String argName, Consumer<Boolean> consumer) {

        for (int i = 0; i < args.length; i++) {
            if (argName.equals(args[i])) {
                consumer.accept(Boolean.TRUE);
                break;
            }
        }

    }

}
