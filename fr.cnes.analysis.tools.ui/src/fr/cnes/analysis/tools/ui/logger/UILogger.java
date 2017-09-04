package fr.cnes.analysis.tools.ui.logger;

import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import fr.cnes.analysis.tools.ui.Activator;

/**
 * i-Code CNES UI logger.
 * 
 * @since 3.0
 */
public final class UILogger {

    /** Line separator for system executing the log */
    private static final String BLANK = System.getProperty("line.separator");
    /** Enter message header */
    private static final String MSG_ENTERING = "[ENTERING] ";

    /** Exit message header */
    private static final String MSG_EXITING = "[EXITING] ";
    /**
     * Plugin's eclipse defined LOGGER.
     */
    private static ILog logger = Activator.getDefault().getLog();

    /**
     * No constructor for final classes.
     */
    private UILogger() {
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
    public static void throwing(String className, String methodName, Throwable exception) {
        logger.log(new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                        localizedMessageln(className, methodName), exception));
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
    public static void warning(String className, String methodName, String message) {
        logger.log(new Status(IStatus.WARNING, Activator.PLUGIN_ID,
                        localizedMessageln(className, methodName) + "Message:" + message));
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
    public static void warning(String className, String methodName, Throwable exception) {
        logger.log(new Status(IStatus.WARNING, Activator.PLUGIN_ID,
                        localizedMessageln(className, methodName), exception));
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
    public static void error(String className, String methodName, Throwable exception) {
        logger.log(new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                        localizedMessageln(className, methodName), exception));
    }

    /**
     * Logging method entering.
     * 
     * @param className
     *            class containing the method
     * @param methodName
     *            method entered.
     */
    public static void entering(String className, String methodName) {
        logger.log(new Status(IStatus.INFO, Activator.PLUGIN_ID,
                        enteringMessageln(className, methodName)));
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
    public static void entering(String className, String methodName, Object[] params) {
        String enteringMessage = enteringMessageln(className, methodName) + "Params:";
        for (final Object param : params) {
            enteringMessage += BLANK + "-" + objectToLogString(param);
        }
        logger.log(new Status(IStatus.INFO, Activator.PLUGIN_ID, enteringMessage));
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
    public static void entering(String className, String methodName, Object param) {
        final String enteringMessage = enteringMessageln(className, methodName) + "Param:"
                        + objectToLogString(param);
        logger.log(new Status(IStatus.INFO, Activator.PLUGIN_ID, enteringMessage));
    }

    /**
     * Logging void method exiting
     * 
     * @param className
     *            containing the method
     * @param methodName
     *            method left
     */
    public static void exiting(String className, String methodName) {
        logger.log(new Status(IStatus.INFO, Activator.PLUGIN_ID,
                        exitingMessageln(className, className)));
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
    public static void exiting(String className, String methodName, Object returned) {
        logger.log(new Status(IStatus.INFO, Activator.PLUGIN_ID,
                        exitingMessageln(className, methodName) + "Returns: "
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
    private static String localizedMessageln(String className, String methodName) {
        return "Class: " + className + BLANK + "Method: " + methodName + BLANK;

    }

    /**
     * @param className
     *            class name
     * @param methodName
     *            method name
     * @return message containing information on the method entered
     */
    private static String enteringMessageln(String className, String methodName) {
        return MSG_ENTERING + BLANK + localizedMessageln(className, methodName);
    }

    /**
     * @param className
     *            class name
     * @param methodName
     *            method name
     * @return message containing information on the method entered
     */
    private static String exitingMessageln(String className, String methodName) {
        return MSG_EXITING + BLANK + localizedMessageln(className, methodName);
    }

    /**
     * @param object
     *            to turn into string
     * @return object in a String
     */
    private static String objectToLogString(Object object) {
        String str = "";
        if (object != null) {
            str = object.getClass().getName() + "." + object.toString();
        }
        return str;
    }

}
