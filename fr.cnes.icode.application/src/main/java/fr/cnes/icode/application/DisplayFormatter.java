/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.application;

import java.util.logging.Formatter;
import java.util.logging.Level;
import java.util.logging.LogRecord;

/**
 * Format log message to be displayed on the console.
 *
 * @author lequal
 */
public class DisplayFormatter extends Formatter {

    /**
     * All printed message should comply to this format.
     */
    private final String FORMAT_DEFAULT = "[%s] %s\n";

    /**
     * Following format is only for FINE level: we just print the message.
     */
    private final String FORMAT_INFO = "%s\n";

    /**
     * Inherited method to format messages.
     * @param pRecord Contain information provided to the logger.
     * @return The formatted string as defined in FORMAT constant.
     */
    @Override
    public String format(LogRecord pRecord) {
        // Default format applied at beginning
        String message = String.format(FORMAT_DEFAULT, pRecord.getLevel().getLocalizedName(), pRecord.getMessage());

        // If record level is INFO, we change the format.
        if(pRecord.getLevel().equals(Level.INFO)) {
            message = String.format(FORMAT_INFO, pRecord.getMessage());
        }

        return message;
    }
}
