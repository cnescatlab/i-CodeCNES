/* 
 * i-Code CNES is a static code analyser. 
 * This software is a free software, under the terms of the Eclipse Public License version 1.0. 
 * http://www.eclipse.org/legal/epl-v10.html
 *  
 */
package fr.cnes.analysis.tools.analyzer.datas;

import java.io.File;

/**
 * This class contains all result from i-Code CNES analysis. {@link CheckResult}
 * contains both results of metrics computing and rule's violations.
 * 
 * @since 3.0
 */
public class CheckResult {

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
     * Constructor for CheckResult containing {@link Checker} informations.
     * 
     * @param pCheckerName
     *            check's name
     * @param pCheckerId
     *            check's identifier
     * @param pLanguageId
     *            name of the plugin contributor
     */
    public CheckResult(String pCheckerName, String pCheckerId, String pLanguageId) {
        this.checkerName = pCheckerName;
        this.checkerId = pCheckerId;
        this.languageId = pLanguageId;
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
    public CheckResult(String pCheckerName, String pCheckerId, File pFile) {
        this.checkerName = pCheckerName;
        this.checkerId = pCheckerId;
        this.file = pFile;
    }

    /**
     * @return the name
     */
    public String getName() {
        return checkerName;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.checkerName = name;
    }

    /**
     * @return the id
     */
    public String getId() {
        return checkerId;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(String id) {
        this.checkerId = id;
    }

    /**
     * @return the location
     */
    public String getLocation() {
        return location;
    }

    /**
     * @param pLocation
     *            the location to set
     */
    public void setLocation(String pLocation) {
        this.location = pLocation;
    }

    /**
     * @return the line
     */
    public Integer getLine() {
        return line;
    }

    /**
     * @param pLine
     *            the line to set
     */
    public void setLine(Integer pLine) {
        this.line = pLine;
    }

    /**
     * @return the message
     */
    public String getMessage() {
        return message;
    }

    /**
     * @param pMessage
     *            the message to set
     */
    public void setMessage(String pMessage) {
        this.message = pMessage;
    }

    /**
     * @return the value
     */
    public Float getValue() {
        return value;
    }

    /**
     * @param pValue
     *            the value to set
     */
    public void setValue(Float pValue) {
        this.value = pValue;
    }

    /**
     * @return the langageId
     */
    public String getLangageId() {
        return languageId;
    }

    /**
     * @param langageId
     *            the langageId to set
     */
    public void setLangageId(String langageId) {
        this.languageId = langageId;
    }

    /**
     * @return the file
     */
    public File getFile() {
        return file;
    }

    /**
     * @param pFile
     *            the file to set
     */
    public void setFile(File pFile) {
        this.file = pFile;
    }

}
