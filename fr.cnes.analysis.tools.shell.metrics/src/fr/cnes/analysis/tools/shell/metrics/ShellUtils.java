/**
 * 
 */
package fr.cnes.analysis.tools.shell.metrics;

/**
 * @author waldmao
 *
 */
public final class ShellUtils {

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
