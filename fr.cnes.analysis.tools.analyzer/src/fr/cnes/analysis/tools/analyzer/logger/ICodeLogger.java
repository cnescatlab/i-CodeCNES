package fr.cnes.analysis.tools.analyzer.logger;

import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;

import fr.cnes.analysis.tools.analyzer.Activator;

/**
 * <h1>i-Code CNES Logger</h1>
 * 
 * <p>
 * This logger is using {@link ILog} plugin's logger to log actions. It was
 * designed to both handles java event and i-Code Analyzer event as automaton
 * transition.
 * </p>
 * 
 * <p>
 * To use this Logger, be advised that the {@link #pluginId} is defined for the
 * plug-in {@link Activator#PLUGIN_ID}. To raise events from your own plugin
 * extend this class and redefine both {@link #getLogger()} &
 * {@link #getPluginId()} method.
 * </p>
 * 
 * 
 * @since 3.0
 */
public final class ICodeLogger {

    /** Line separator for system executing the log */
    private static final String BLANK = System.getProperty("line.separator");
    /** Enter message header */
    private static final String MSG_ENTERING = "[ENTERING] ";

    /** Exit message header */
    private static final String MSG_EXITING = "[EXITING] ";
    /** ERROR message header */
    private static final String MSG_ERROR = "[ERROR] ";
    /** WARNING message header */
    private static final String MSG_WARNING = "[WARNING] ";
    /** INFO message header */
    private static final String MSG_INFO = "[INFO] ";
    /**
     * Plugin's eclipse defined LOGGER.
     */
    private static ILog logger = getLogger();
    /**
     * Plug-in's identifier.
     */
    private static String pluginId = getPluginId();

    /**
     * No constructor for final classes.
     */
    private ICodeLogger() {
        // No constructor for this util class.
    }

    /**
     * Logging exception with {@link IStatus#ERROR} level
     * 
     * @param className
     *            class throwing the exception
     * @param methodName
     *            method throwing the exception
     * @param exception
     *            exception thrown
     */
    public static void throwing(final String className, final String methodName,
                    final Throwable exception) {
        logger.log(new Status(IStatus.ERROR, pluginId,
                        MSG_ERROR + BLANK + localizedMessageln(className, methodName), exception));
    }

    /**
     * Logging a warning message in the level.
     * 
     * @param className
     *            of the execution
     * @param methodName
     *            of the execution
     * @param message
     *            related to the warning.
     */
    public static void warning(final String className, final String methodName,
                    final String message) {
        logger.log(new Status(IStatus.WARNING, pluginId, MSG_WARNING + BLANK
                        + localizedMessageln(className, methodName) + "Message:" + message));
    }

    /**
     * Logging warning handling with {@link IStatus#WARNING} level
     * 
     * @param className
     *            class throwing the exception
     * @param methodName
     *            method throwing the exception
     * @param exception
     *            exception thrown
     */
    public static void warning(final String className, final String methodName,
                    final Throwable exception) {
        logger.log(new Status(IStatus.WARNING, pluginId,
                        MSG_WARNING + BLANK + localizedMessageln(className, methodName),
                        exception));
    }

    /**
     * Logging error handling with {@link IStatus#ERROR} level
     * 
     * @param className
     *            class throwing the exception
     * @param methodName
     *            method throwing the exception
     * @param exception
     *            exception thrown
     */
    public static void error(final String className, final String methodName,
                    final Throwable exception) {
        logger.log(new Status(IStatus.ERROR, pluginId,
                        MSG_ERROR + BLANK + localizedMessageln(className, methodName), exception));
    }

    /**
     * Logging error handling with {@link IStatus#ERROR} level
     * 
     * @param className
     *            class throwing the exception
     * @param methodName
     *            method throwing the exception
     * @param message
     *            error message
     */
    public static void error(final String className, final String methodName,
                    final String message) {
        logger.log(new Status(IStatus.ERROR, pluginId,
                        MSG_ERROR + BLANK + localizedMessageln(className, methodName) + BLANK
                                        + "Message:" + message));
    }

    /**
     * Logging method entering.
     * 
     * @param className
     *            class containing the method
     * @param methodName
     *            method entered.
     */
    public static void entering(final String className, final String methodName) {
        logger.log(new Status(IStatus.INFO, pluginId,
                        MSG_INFO + enteringMessageln(className, methodName)));
    }

    /**
     * Logging method entering with details of the several parameters.
     * 
     * @param className
     *            class containing the method
     * @param methodName
     *            method entered
     * @param params
     *            parameters
     */
    public static void entering(final String className, final String methodName,
                    final Object[] params) {
        String enteringMessage = enteringMessageln(className, methodName) + "Params:";
        for (final Object param : params) {
            enteringMessage += BLANK + "-" + objectToLogString(param);
        }
        logger.log(new Status(IStatus.INFO, pluginId, MSG_INFO + enteringMessage));
    }

    /**
     * Logging method entering with detail of only one parameter.
     * 
     * @param className
     *            class containing the method
     * @param methodName
     *            method entered
     * @param param
     *            parameter
     */
    public static void entering(final String className, final String methodName,
                    final Object param) {
        final String enteringMessage = enteringMessageln(className, methodName) + "Param:"
                        + objectToLogString(param);
        logger.log(new Status(IStatus.INFO, pluginId, MSG_INFO + enteringMessage));
    }

    /**
     * Logging void method exiting
     * 
     * @param className
     *            containing the method
     * @param methodName
     *            method left
     */
    public static void exiting(final String className, final String methodName) {
        logger.log(new Status(IStatus.INFO, pluginId,
                        MSG_INFO + exitingMessageln(className, className)));
    }

    /**
     * Logging a function exiting with it's returned parameter
     * 
     * @param className
     *            class containing the method
     * @param methodName
     *            method name
     * @param returned
     *            Object returned.
     */
    public static void exiting(final String className, final String methodName,
                    final Object returned) {
        logger.log(new Status(IStatus.INFO, pluginId,
                        MSG_INFO + exitingMessageln(className, methodName) + "Returns: "
                                        + objectToLogString(returned)));
    }

    /**
     * @param className
     *            class name
     * @param methodName
     *            method name
     * @return message containing information on method and classname entered
     *         for the log file with line return.
     */
    private static String localizedMessageln(final String className, final String methodName) {
        return "Class: " + className + BLANK + "Method: " + methodName + BLANK;

    }

    /**
     * @param className
     *            class name
     * @param methodName
     *            method name
     * @return message containing information on the method entered
     */
    private static String enteringMessageln(final String className, final String methodName) {
        return MSG_ENTERING + BLANK + localizedMessageln(className, methodName);
    }

    /**
     * @param className
     *            class name
     * @param methodName
     *            method name
     * @return message containing information on the method entered
     */
    private static String exitingMessageln(final String className, final String methodName) {
        return MSG_EXITING + BLANK + localizedMessageln(className, methodName);
    }

    /**
     * @param object
     *            to turn into string
     * @return object in a String
     */
    private static String objectToLogString(final Object object) {
        String str = "";
        if (object != null) {
            str = object.getClass().getName() + "." + object.toString();
        }
        return str;
    }

    /**
     * @return plugin logger.
     */
    public static ILog getLogger() {
        return Platform.getLog(Activator.getContext().getBundle());
    }

    /**
     * @return plugin identifier.
     */
    public static String getPluginId() {
        return Activator.PLUGIN_ID;
    }

}
