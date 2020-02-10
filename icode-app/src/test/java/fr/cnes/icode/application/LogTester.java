/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.application;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Logger;
import java.util.logging.StreamHandler;

/**
 * This class provides a mean to extract log for test purpose.
 */
public class LogTester {

    private OutputStream logStream;
    private Handler[] handlers;
    private StreamHandler testLogHandler;
    private Logger logger;

    /**
     * Private constructor to force developer to use factory.
     */
    private LogTester() {}

    /**
     * Create a LogTester capturing log for tests.
     *
     * @param logger Logger whose log will be captured.
     * @return LogTester containing captured log.
     */
    public static LogTester capture(final Logger logger, final Formatter formatter) {
        final LogTester logTester = new LogTester();
        logTester.logger = logger;

        logTester.logStream = new ByteArrayOutputStream();
        logTester.handlers = logTester.logger.getParent().getHandlers();
        logTester.testLogHandler = new StreamHandler(logTester.logStream, formatter);
        logTester.logger.addHandler(logTester.testLogHandler);

        return logTester;
    }

    /**
     * Retrieve and flush captured log.
     *
     * @return Log as String.
     */
    public String log() {
        this.testLogHandler.flush();
        return this.logStream.toString();
    }

    /**
     * Unsubscribe LogTester from targeted Logger.
     */
    public void free() {
        this.logger.removeHandler(this.testLogHandler);
    }

}