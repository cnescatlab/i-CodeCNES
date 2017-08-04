/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/
package fr.cnes.analysis.tools.shell.metrics;

/**
 * This class contains function that can be used in Lex files for parsing Shell
 * files.
 */
public final class ShellUtils {

    /**
     * Default constructor removal.
     */
    private ShellUtils() {

    }

    /**
     * @param commandOpening
     *            Command beginner
     * @return command closer for <code>commandOpening</code>.
     */
    public static String commandClosure(String commandOpening) {
        String closure = null;
        switch (commandOpening) {
        case "`":
            closure = "`";
            break;
        case "$(":
            closure = ")";
            break;
        default:
            closure = null;
            break;
        }
        return closure;
    }

}
