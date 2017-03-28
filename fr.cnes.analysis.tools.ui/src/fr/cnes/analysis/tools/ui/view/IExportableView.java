/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
/**
 *
 */
package fr.cnes.analysis.tools.ui.view;

import java.io.File;
import java.io.IOException;

/**
 * Abstract view defining the basis for violations and metrics results export.
 * This view extends the abstract analysis view.
 * 
 */
public interface IExportableView {

    /**
     * Method used to export to CSV format the results of this view. This
     * creates a CSV formatted file which contains all the results.
     * 
     * @param file
     *            the file containing CSV export
     * @throws IOException
     *             if the file exists but is a directory rather than a regular
     *             file, does not exist but cannot be created, or cannot be
     *             opened for any other reason
     */
    public void exportToCSV(final File file) throws IOException;

    /**
     * Method used to export to XML format the results of this view. This
     * creates a XML formatted file which contains all the results.
     * 
     * @param file
     *            the file containing XML export
     * @throws IOException
     *             if the file exists but is a directory rather than a regular
     *             file, does not exist but cannot be created, or cannot be
     *             opened for any other reason
     */
    public void exportToXML(final File file) throws IOException;

}
