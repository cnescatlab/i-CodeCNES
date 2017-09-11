/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.analysis.tools.analyzer.datas;

import java.io.File;

import fr.cnes.analysis.tools.analyzer.logger.ICodeLogger;

/**
 * This class contains all result from i-Code CNES analysis. {@link CheckResult}
 * contains both results of metrics computing and rule's violations.
 * 
 * @since 3.0
 */
public class CheckResult {
    /** Class name */
    private static final String CLASS = CheckResult.class.getName();
    /** Check name. */
    private String checkerName;

    /** Check id. */
    private String checkerId;

    /** Language id. */
    private String languageId;

    /** Check location. */
    private String location;

    /** Check line. */
    private Integer line;

    /** {@link CheckResult} message. */
    private String message;

    /** Metric value. */
    private Float value;

    /** Analyzed file. */
    private File file;

    /**
     * Constructor for CheckResult containing {@link AbstractChecker}
     * informations.
     * 
     * @param pCheckerName
     *            check's name
     * @param pCheckerId
     *            check's identifier
     * @param pLanguageId
     *            name of the plugin contributor
     */
    public CheckResult(final String pCheckerName, final String pCheckerId,
                    final String pLanguageId) {
        final String method = "CheckResult";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pCheckerName, pCheckerId, pLanguageId
        });
        this.checkerName = pCheckerName;
        this.checkerId = pCheckerId;
        this.languageId = pLanguageId;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * Constructor for {@link CheckResult} containing analysis configuration.
     * 
     * @param pCheckerName
     *            checker's name
     * @param pCheckerId
     *            checker's identifier
     * @param pFile
     *            analyzed file.
     */
    public CheckResult(final String pCheckerName, final String pCheckerId, final File pFile) {
        final String method = "CheckResult";
        ICodeLogger.entering(CLASS, method, new Object[] {
            pCheckerName, pCheckerId, pFile
        });
        this.checkerName = pCheckerName;
        this.checkerId = pCheckerId;
        this.file = pFile;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the name
     */
    public String getName() {
        final String method = "getName";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, checkerName);
        return checkerName;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(final String name) {
        final String method = "setName";
        ICodeLogger.entering(CLASS, method, name);
        this.checkerName = name;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the id
     */
    public String getId() {
        final String method = "getId";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, checkerId);
        return checkerId;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(final String id) {
        final String method = "setId";
        ICodeLogger.entering(CLASS, method, id);
        this.checkerId = id;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the location
     */
    public String getLocation() {
        final String method = "getLocation";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, location);
        return location;
    }

    /**
     * @param pLocation
     *            the location to set
     */
    public void setLocation(final String pLocation) {
        final String method = "setLocation";
        ICodeLogger.entering(CLASS, method, pLocation);
        this.location = pLocation;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the line
     */
    public Integer getLine() {
        final String method = "getLine";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, line);
        return line;
    }

    /**
     * @param pLine
     *            the line to set
     */
    public void setLine(final Integer pLine) {
        final String method = "setLine";
        ICodeLogger.entering(CLASS, method, pLine);
        this.line = pLine;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the message
     */
    public String getMessage() {
        final String method = "getMessage";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, message);
        return message;
    }

    /**
     * @param pMessage
     *            the message to set
     */
    public void setMessage(final String pMessage) {
        final String method = "setMessage";
        ICodeLogger.entering(CLASS, method, pMessage);
        this.message = pMessage;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the value
     */
    public Float getValue() {
        final String method = "getValue";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, value);
        return value;
    }

    /**
     * @param pValue
     *            the value to set
     */
    public void setValue(final Float pValue) {
        final String method = "setValue";
        ICodeLogger.entering(CLASS, method, pValue);
        this.value = pValue;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the langageId
     */
    public String getLangageId() {
        final String method = "getLangageId";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, languageId);
        return languageId;
    }

    /**
     * @param langageId
     *            the langageId to set
     */
    public void setLangageId(final String langageId) {
        final String method = "setLangageId";
        ICodeLogger.entering(CLASS, method, langageId);
        this.languageId = langageId;
        ICodeLogger.exiting(CLASS, method);
    }

    /**
     * @return the file
     */
    public File getFile() {
        final String method = "getFile";
        ICodeLogger.entering(CLASS, method);
        ICodeLogger.exiting(CLASS, method, file);
        return file;
    }

    /**
     * @param pFile
     *            the file to set
     */
    public void setFile(final File pFile) {
        final String method = "setFile";
        ICodeLogger.entering(CLASS, method, pFile);
        this.file = pFile;
        ICodeLogger.exiting(CLASS, method);
    }

}
