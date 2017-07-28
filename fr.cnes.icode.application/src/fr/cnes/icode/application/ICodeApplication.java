package fr.cnes.icode.application;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Supplier;

import org.apache.tools.ant.DirectoryScanner;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.equinox.app.IApplication;
import org.eclipse.equinox.app.IApplicationContext;
import org.osgi.framework.FrameworkUtil;

import fr.cnes.analysis.tools.analyzer.Analyzer;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.services.languages.LanguageService;
import fr.cnes.analysis.tools.export.IExport;
import fr.cnes.analysis.tools.export.csv.ExportCsv;
import fr.cnes.analysis.tools.export.xml.ExportXml;

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

	// -----------------------------------------------
	// Define the parameters for this application
	// -----------------------------------------------
	/** help argument to display syntax */
	public static final String ARG_HELP = "-help"; //$NON-NLS-1$

	/** Argument for verbose */
	public static final String ARG_VERBOSE = "-v"; //$NON-NLS-1$

	/**
	 * The output format for the result: can be -f xml or -f csv.. default is XML
	 */
	public static final String ARG_OUTPUT_FORMAT = "-f"; //$NON-NLS-1$
	/** Expected value xml for output format */
	public static final String ARG_OUTPUT_FORMAT_XML = "xml"; //$NON-NLS-1$
	/** Expected value csv for output format */
	public static final String ARG_OUTPUT_FORMAT_CSV = "csv"; //$NON-NLS-1$

	/** The optional output filename */
	public static final String ARG_OUTPUT_FILE = "-output"; //$NON-NLS-1$


	// --------------------------------------------------------------------------------------
	// Specific additional parameters only relevant for the XML output (no effect on
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

	private List<String> availableArgs = Arrays.asList(new String[] { ARG_HELP, ARG_VERBOSE, ARG_OUTPUT_FORMAT,
			ARG_OUTPUT_FILE, ARG_AUTHOR, ARG_PROJECT, ARG_PROJECT_VERSION, ARG_CONFIG_ID });

	// List args that are used alone (no parameters)
	private List<String> singleArgs = Arrays.asList(new String[] { ARG_HELP, ARG_VERBOSE });

	/** output format to be used by default is XML */
	private String outputFormat = ARG_OUTPUT_FORMAT_XML;

	/** output filename to be used, by default sysout */
	private String outputFilename = null;

	/** verbose mode (default is false) */
	private boolean verbose = false;

	/** config ID */
	private String configID = null;

	private String projectName, projectVersion, author;

	private String[] args;

	private List<String> filenames = new ArrayList<String>();

	// Logger information
	private ILog log;
	private String pluginId = FrameworkUtil.getBundle(getClass()).getSymbolicName();

	// The current exporter to use.
	private IExport exporter;

	@Override
	public Object start(IApplicationContext context) throws Exception {

		args = (String[]) context.getArguments().get(IApplicationContext.APPLICATION_ARGS);
		log = Platform.getLog(FrameworkUtil.getBundle(getClass()));

		getBooleanArg(ARG_VERBOSE, v -> setVerbose(v));

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
			// Consider to have the list of files at the end of the command line after the
			// last argument
			// Consider also to have only arguments with values : -f xml -output filename
			// ....
			// No single argument without values
			// Skip all arguments to get filename index
			if (s.startsWith("-")) {
				indexFiles += ((singleArgs.contains(s) ? 1 : 2)); // Add 1 for single arguments, else add 2
				// Check if argument exists
				if (!availableArgs.contains(s)) {
					warning("The argument name '" + s + "' is not expected for this command. Check help : ");
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
		getArgValue(ARG_AUTHOR, v -> setAuthor(v));
		getArgValue(ARG_PROJECT, v -> setProjectName(v));
		getArgValue(ARG_PROJECT_VERSION, v -> setProjectVersion(v));
		getArgValue(ARG_CONFIG_ID, v -> setConfigID(v));

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

		// print the file content if no outputfilename (-> temporoary file)
		if (outputFilename == null) {
			displayFile(outputFile);
		}

		return EXIT_OK;
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
		System.out.println("This will print the result in console using the default output in XML format.\n");
		System.out.println("Files can be either absolute paths or patterns like : \t icode *.f90 ~/tmp/**/*.f77 /tmp/myfile.f77\n");
		System.out.println("Examples :  ");
		System.out.println("*.f90           : all the f90 files in current directory ");
		System.out.println("~/tmp/**/*.f77  : all files with f77 extension in $HOME/tmp and any of its sub directoryies");
		System.out.println("/tmp/myfile.f77 : the specific file /tmp/myfile.f77");
		System.out.println("\nAvailable options : ");
		System.out.println("\t-v : verbose, will display some traces, namely the files found in pattern");
		System.out.println("\t-f [xml | csv] : output format. Can be xml or csv.");
		System.out.println("\t-output filename : filename to store the result. ");
		System.out.println("\nFor XML output format these optional parameters can be used in any order : \n");
		System.out.println("\t\t-project yourProject -projectVersion yourVersion -author yourName -configID yourID \n");
		System.out.println("These XML parameters are not used by CSV\n");
		System.out.println("Examples:");
		System.out.println("\ticode  -f xml -output result.xml *.f ");
		System.out.println("\ticode  -f xml -author Me -output result.xml *.f ");
		System.out.println("\ticode  -f xml -author Me -project MyProject -output result.xml *.f ");
		System.out
				.println("\ticode  -f xml -author Me -project MyProject -projectVersion 1.2.0 -output result.xml *.f ");
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
			info("  -> Output filename : " + (outputFilename == null ? "in console" : outputFilename));

	}

	private File getOutputFile() {
		if (outputFilename != null)
			return new File(outputFilename);
		else {
			try {
				return File.createTempFile("icodeResult", outputFormat);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return null;
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
		if (ARG_OUTPUT_FORMAT_CSV.equals(value))
			outputFormat = ARG_OUTPUT_FORMAT_CSV;
		else if (ARG_OUTPUT_FORMAT_XML.equals(value)) {
			outputFormat = ARG_OUTPUT_FORMAT_XML;
		} else
			warning("Parameter for outputFileFormat (-f) must be 'csv' or 'xml', default is 'xml'");

		if (verbose)
			info("  -> Output format :  " + outputFormat);

	}

	private IExport getExporter() {
		IExport result = null;
		if (outputFormat == ARG_OUTPUT_FORMAT_XML) {
			result = new ExportXml();
			configureXMLExporter(result, ExportXml.PARAM_AUTHOR, () -> getAuthor());
			configureXMLExporter(result, ExportXml.PARAM_PROJECT_NAME, () -> getProjectName());
			configureXMLExporter(result, ExportXml.PARAM_PROJECT_VERSION, () -> getProjectVersion());
			configureXMLExporter(result, ExportXml.PARAM_CONFIGURATION_ID, () -> getConfigID());
		} else {
			result = new ExportCsv();
		}

		return result;
	}

	private void configureXMLExporter(IExport exporter, String propertyName, Supplier<String> supplier) {
		String value = supplier.get();
		if (value != null)
			exporter.getParameters().put(propertyName, value);

	}

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
	 * This method extract an argument value and set it on the appropriate method
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
	 * This method extract a boolean argument and set it on the appropriate method
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
