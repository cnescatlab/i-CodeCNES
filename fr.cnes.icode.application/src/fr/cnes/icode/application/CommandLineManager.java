/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.application;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.cli.BasicParser;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.UnrecognizedOptionException;

/**
 * Manage the command line by parsing it and providing preprocessed data.
 *
 * @author lequal
 */
public class CommandLineManager {

    /**
     * Default logger.
     */
    private final Logger LOGGER = Logger.getLogger(ICodeApplication.class.getName());

    /**
     * Possible options definition.
     */
    private Options options;
    /**
     * Parser used by the manager.
     */
    private CommandLineParser parser;
    /**
     * Formatter for automatic help.
     */
    private HelpFormatter helpFormatter;
    /**
     * Contain the formatted cli.
     */
    private CommandLine commandLine;
    
    /**
     * Option short name for exporters.
     */
    public static final String EXPORTERS = "e";
    /**
     * Option short name for help.
     */
    public static final String HELP = "h";
    /**
     * Option short name for languages.
     */
    public static final String LANGUAGES = "l";
    /**
     * Option short name for rules.
     */
    public static final String RULES = "r";
    /**
     * Option short name for output filename.
     */
    public static final String OUTPUT = "o";
    /**
     * Option short name for excluded rules.
     */
    public static final String EXCLUDED_RULES = "x";
    /**
     * Option short name for checked languages.
     */
    public static final String CHECKED_LANGUAGES = "c";
    /**
     * Option short name for results format.
     */
    public static final String EXPORT_FORMAT = "f";

    /**
     * Default construct which initialize and set options.
     */
    public CommandLineManager() {
        configure();
    }

    /**
     * Set all members and prepare list of possible options
     */
    private void configure() {
        // Initialize values for members.
        options = new Options();
        parser = new BasicParser();
        helpFormatter = new HelpFormatter();
        commandLine = null;

        // Add options
        options.addOption(HELP, "help", false, 
        		"Display this message.");
        options.addOption(EXPORTERS, "exporters", false, 
        		"Display all available exporters.");
        options.addOption(LANGUAGES, "languages", false, 
        		"Display all available languages.");
        options.addOption(RULES, "rules", false, 
        		"Display all available rules.");
        
        options.addOption(OUTPUT, "output", true, 
        		"Set the name for result file. Results are displayed in standard output by default.");
        options.addOption(EXCLUDED_RULES, "excluded-rules", true, 
        		"Comma separated list of rules id to exclude from analysis. None by default.");
        options.addOption(CHECKED_LANGUAGES, "checked-languages", true, 
        		"Comma separated list of languages checked during analysis. All by default.");
        options.addOption(EXPORT_FORMAT, "export-format", true, 
        		"Set the format for result file. Default format is XML.");
    }

    /**
     * Parse the provided command line.
     * @param pArgs Arguments to parse.
     */
    public void parse(final String[] pArgs) {
        try {
        	boolean areOptionsCorrect = false;
        	try {
	            // Parse the command line.
	            commandLine = parser.parse(options, pArgs);
	            areOptionsCorrect = checkOptionsUse(commandLine); 
        	} catch(UnrecognizedOptionException e) {
        		LOGGER.warning(e.getLocalizedMessage());
        		areOptionsCorrect = false;
            }
            // If help option is present we print it.
            if(!areOptionsCorrect || commandLine.hasOption(HELP)) {
                helpFormatter.printHelp(128, "icode [<FILE> [...]]",
                        "Analyze Shell, F77 & F90 code to find defects & bugs.\n\n", options,
                        "\n\nPlease report issues at https://github.com/lequal/i-CodeCNES/issues", true);
                System.exit(0);
            }
        } catch (ParseException e) {
            LOGGER.log(Level.SEVERE, e.getMessage(), e);
        }
    }

    /**
     * Check options compatibility:
     * + Options EXPORTERS, HELP, LANGUAGES & RULES cannot be mixed with other options.
     * 
     * @param commandLine Parsed command line.
     * @return True if options respect requirements.
     */
    private boolean checkOptionsUse(final CommandLine commandLine) {
    	// number of options which should be called alone
		int standaloneOptions = 0;
		// number of options without restriction
		int analysisOptions = 0;
		
		for(final Option option : commandLine.getOptions()) {
			if(option.getOpt().equals(HELP) 
					|| option.getOpt().equals(EXPORTERS) 
					|| option.getOpt().equals(LANGUAGES) 
					|| option.getOpt().equals(RULES)) {
				standaloneOptions++;
			} else {
				analysisOptions++;
			}
		}
		
		return (analysisOptions==0 || standaloneOptions==0) && standaloneOptions<2;
	}

	/**
     * Provides arguments as a list.
     * 
     * @return A List<String> with args.
     */
    public List<String> getArgs() {
        List<String> result;

        if(null!=commandLine) {
            result = commandLine.getArgList();
        } else {
            result = new ArrayList<>();
        }

        return result;
    }

    /**
     * Determine if an option is contained in the cli.
     * 
     * @param pOption Name of the option to retrieve.
     * @return True if the cli contains the option.
     */
    public boolean hasOption(final String pOption) {
        return commandLine!=null && commandLine.hasOption(pOption);
    }

    /**
     * Return the value of the corresponding option.
     * 
     * @param pOption Name of the option.
     * @return A string containing the value or an empty string.
     */
    public String getOptionValue(final String pOption) {
        String result = "";

        if(null!=commandLine) {
            result = commandLine.getOptionValue(pOption);
        }

        return result;
    }
}
