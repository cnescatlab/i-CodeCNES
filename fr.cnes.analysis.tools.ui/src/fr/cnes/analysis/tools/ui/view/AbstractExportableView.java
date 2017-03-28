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
import java.io.FileWriter;
import java.io.IOException;
import java.util.logging.Logger;

/**
 * Abstract view defining the basis for violations and metrics results export.
 * This view extends the abstract analysis view.
 * 
 */
public abstract class AbstractExportableView extends AbstractAnalysisView {
    /** Logger **/
    private static final Logger LOGGER = Logger.getLogger(AbstractExportableView.class.getName());

    /** File writer to contains exported results. **/
    private FileWriter out;

    /**
     * Constructor only using super constructor definitions.
     * 
     * @param pBounds
     *            viewer columns' bounds
     * @param pTitles
     *            viewer columns' titles
     */
    public AbstractExportableView(final int[] pBounds, final String[] pTitles) {
        super(pBounds, pTitles);
    }

    /**
     * Getter for the file writer.
     * 
     * @return the out
     */
    public FileWriter getOut() {
        return this.out;
    }

    /**
     * Setter for the file writer.
     * 
     * @param pOut
     *            the out to set
     */
    public void setOut(final FileWriter pOut) {
        this.out = pOut;
    }

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
    public void exportToCSV(final File file) throws IOException {
        LOGGER.finest("Begin exportToCSV method");

        out = new FileWriter(file);
        toCSV();
        out.close();

        LOGGER.finest("End exportToCSV method");
    }

    /**
     * Export results to xml file.
     * 
     * @param file
     *            output file.
     * @throws IOException
     *             In case of problem with the file.
     */
    public void exportToXML(final File file) throws IOException {
        LOGGER.finest("Begin exportToXML method");

        this.toXML(file);

        LOGGER.finest("End exportToXML method");
    }

    /**
     * Method used to write into the FileWrite in the CSV format.
     * 
     * @throws IOException
     *             if an I/O error occurs
     */
    protected abstract void toCSV() throws IOException;

    /**
     * @param file
     *            the output file.
     * @throws IOException
     *             In case of trouble with the file.
     */
    protected abstract void toXML(File file) throws IOException;

}
