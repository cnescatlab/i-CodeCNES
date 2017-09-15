/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.analysis.tools.analyzer.exception;

/**
 * This exception whenever an error occures during JFlex analysis.
 * 
 */
public class JFlexException extends Exception {
    /** Line separator for system executing the log */
    private static final String BLANK = System.getProperty("line.separator");

    /**
     * Serial Version UID.
     */
    private static final long serialVersionUID = 4198004753881804823L;
    /** fileName causing the exception */
    private String fileName;
    /** Line location of the exception */
    private int line;
    /** Column location of the exception */
    private int column;
    /** Last word scanned during the analysis */
    private String lastScan;
    /** Error message defined by the analyzer */
    private String message;
    /** Rules causing the exception */
    private String ruleName;

    /**
     * Default constructor.
     */
    public JFlexException() {
        super();
    }

    /**
     * Constructor with original exception.
     * 
     * @param exception
     *            the original exception.
     */
    public JFlexException(final Exception exception) {
        super(exception);
    }

    /**
     * @param pRuleName
     *            Rules causing the exception
     * @param pFileName
     *            fileName causing the exception
     * @param pMessage
     *            Error message defined by the analyzer
     * @param pLastScan
     *            Last word scanned during the analysis
     * @param pLine
     *            Line location of the exception
     * @param pColumn
     *            Column location of the exception
     */
    public JFlexException(final String pRuleName, final String pFileName, final String pMessage,
                    final String pLastScan, final int pLine, final int pColumn) {
        super(errorMessage(pRuleName, pFileName, pMessage, pLastScan, pLine, pColumn));
        this.ruleName = pRuleName;
        this.fileName = pFileName;
        this.message = pMessage;
        this.line = pLine;
        this.column = pColumn;
    }

    /**
     * @param pRuleName
     *            Rules causing the exception
     * @param pFileName
     *            fileName causing the exception
     * @param pMessage
     *            Error message defined by the analyzer
     * @param pLastScan
     *            Last word scanned during the analysis
     * @param pLine
     *            Line location of the exception
     * @param pColumn
     *            Column location of the exception
     * @return Exception message related to the parameters.
     */
    private static String errorMessage(String pRuleName, String pFileName, String pMessage,
                    String pLastScan, int pLine, int pColumn) {
        final String message = "i-Code CNES analysis encountered a problem." + BLANK + BLANK
                        + pMessage + BLANK + "CheckerId: " + pRuleName + BLANK + "File: "
                        + pFileName + BLANK + "Line:" + pLine + BLANK + "Column:" + pColumn + BLANK

                        + "Last word scanned : [" + pLastScan + "] [" + toDecimalCode(pLastScan)
                        + "]" + BLANK
                        + "Please report this issue on : https://github.com/dupuisa/i-CodeCNES/issues/";
        return message;
    }

    /**
     * @return the fileName
     */
    public final String getFileName() {
        return fileName;
    }

    /**
     * @return the line
     */
    public final int getLine() {
        return line;
    }

    /**
     * @return the column
     */
    public final int getColumn() {
        return column;
    }

    /**
     * @return the lastScan
     */
    public final String getLastScan() {
        return lastScan;
    }

    /**
     * @return the message
     */
    public final String getErrorMessage() {
        return message;
    }

    /**
     * @return the ruleName
     */
    public final String getRuleName() {
        return ruleName;
    }

    /**
     * @param str
     *            to set to ASCII decimal
     * @return
     * @return ASCII decimal of <code>str</code>
     */
    public static final String toDecimalCode(final String str) {
        String code = "";
        for (char character : str.toCharArray()) {
            code += (int) character + " ";
        }
        return code;
    }
}