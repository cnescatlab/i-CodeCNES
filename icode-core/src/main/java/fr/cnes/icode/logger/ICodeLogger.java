/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.logger;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * <h1>i-Code CNES Logger</h1>
 * 
 * <p>
 * This logger is using {@link Logger} class to log actions. It was
 * designed to both handles java event and i-Code Analyzer event as automaton
 * transition.
 * </p>
 * 
 * <p>
 * To use this Logger, be advised that the {@link #pluginId} is defined for the
 * plug-in. To raise events from your own plugin
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
    private static final String MSG_INFO = "[OK] ";
    /**
     * Plugin's eclipse defined LOGGER.
     */
    private final static Logger LOGGER = Logger.getLogger(ICodeLogger.class.getName());
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
     * Logging exception with {@link Level#SEVERE} level
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
        LOGGER.log(Level.SEVERE, String.format("[%s] %s %s", pluginId,
                MSG_ERROR, localizedMessageln(className, methodName)), exception);
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
        LOGGER.log(Level.WARNING, String.format("[%s] %s %s Message: %s",
                pluginId, MSG_WARNING, localizedMessageln(className, methodName), message));
    }

    /**
     * Logging warning handling with {@link Level#WARNING} level
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
        LOGGER.log(Level.WARNING, String.format("[%s] %s %s", pluginId,
                MSG_WARNING, localizedMessageln(className, methodName)), exception);
    }

    /**
     * Logging error handling with {@link Level#SEVERE} level
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
        LOGGER.log(Level.SEVERE, String.format("[%s] %s %s", pluginId,
                        MSG_ERROR, localizedMessageln(className, methodName)), exception);
    }

    /**
     * Logging error handling with {@link Level#SEVERE} level
     * 
     * 
     * @param className
     *            class throwing the exception
     * @param methodName
     *            method throwing the exception
     * @param message
     *            error message
     */
    public static void error(final String className, final String methodName, final String message) {
        LOGGER.log(Level.SEVERE, String.format("[%s] %s %s Message: %s",
                pluginId, MSG_ERROR, localizedMessageln(className, methodName), message));
    }

    /**
     * Logging method entering.
     * 
     * @since 3.0.1 : Logging has been disabled to improve performances.
     * 
     * @param className
     *            class containing the method
     * @param methodName
     *            method entered.
     */
    public static void entering(final String className, final String methodName) {
        //LOGGER.log(new Status(IStatus.OK, pluginId,
        //                MSG_INFO + enteringMessageln(className, methodName)));
    }

    /**
     * Logging method entering with details of the several parameters.
     * 
     * Logging has been disabled to improve performances.
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
        //String enteringMessage = enteringMessageln(className, methodName) + "Params:";
        //for (final Object param : params) {
        //    enteringMessage += BLANK + "-" + objectToLogString(param);
        //}
        //LOGGER.log(new Status(IStatus.OK, pluginId, MSG_INFO + enteringMessage));
    }

    /**
     * Logging method entering with detail of only one parameter.
     * 
     * @since 3.0.1 : Logging has been disabled to improve performances.
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
        //final String enteringMessage = enteringMessageln(className, methodName) + "Param:"
        //                + objectToLogString(param);
        //LOGGER.log(new Status(IStatus.OK, pluginId, MSG_INFO + enteringMessage));
    }

    /**
     * Logging void method exiting
     * 
     * @since 3.0.1 : Logging has been disabled to improve performances.
     * 
     * @param className
     *            containing the method
     * @param methodName
     *            method left
     */
    public static void exiting(final String className, final String methodName) {
        //LOGGER.log(new Status(IStatus.OK, pluginId,
        //                MSG_INFO + exitingMessageln(className, className)));
    }

    /**
     * Logging a function exiting with it's returned parameter
     * 
     * @since 3.0.1 : Logging has been disabled to improve performances.
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
        //LOGGER.log(new Status(IStatus.OK, pluginId,
        //                MSG_INFO + exitingMessageln(className, methodName) + "Returns: "
        //                                + objectToLogString(returned)));
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
     * @return i-Code LOGGER.
     */
    public static Logger getLogger() {
        return LOGGER;
    }

    /**
     * @return plugin identifier.
     */
    public static String getPluginId() {
        return "fr.cnes.icode";
    }

}
