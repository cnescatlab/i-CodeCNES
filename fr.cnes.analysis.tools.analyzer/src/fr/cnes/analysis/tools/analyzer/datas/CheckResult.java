package fr.cnes.analysis.tools.analyzer.datas;

import java.io.File;

public class CheckResult {

	/** Check name. */
	private String name;

	/** Check id. */
	private String id;

	/** Langage id. */
	private String langageId;

	/** Check location. */
	private String location;

	/** Check line. */
	private Integer line;

	/** Violation message. */
	private String message;

	/** Metric value. */
	private Float value;

	/** Analysed file. */
	private File file;

	public CheckResult(String name, String id, String langageId) {
		// TODO Auto-generated constructor stub
	}

	public CheckResult(String name, String id, File file) {
		this.name = name;
		this.id = id;
		this.file = file;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name
	 *            the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the id
	 */
	public String getId() {
		return id;
	}

	/**
	 * @param id
	 *            the id to set
	 */
	public void setId(String id) {
		this.id = id;
	}

	/**
	 * @return the location
	 */
	public String getLocation() {
		return location;
	}

	/**
	 * @param location
	 *            the location to set
	 */
	public void setLocation(String location) {
		this.location = location;
	}

	/**
	 * @return the line
	 */
	public Integer getLine() {
		return line;
	}

	/**
	 * @param line
	 *            the line to set
	 */
	public void setLine(Integer line) {
		this.line = line;
	}

	/**
	 * @return the message
	 */
	public String getMessage() {
		return message;
	}

	/**
	 * @param message
	 *            the message to set
	 */
	public void setMessage(String message) {
		this.message = message;
	}

	/**
	 * @return the value
	 */
	public Float getValue() {
		return value;
	}

	/**
	 * @param value
	 *            the value to set
	 */
	public void setValue(Float value) {
		this.value = value;
	}

	/**
	 * @return the langageId
	 */
	public String getLangageId() {
		return langageId;
	}

	/**
	 * @param langageId
	 *            the langageId to set
	 */
	public void setLangageId(String langageId) {
		this.langageId = langageId;
	}

	/**
	 * @return the file
	 */
	public File getFile() {
		return file;
	}

	/**
	 * @param file
	 *            the file to set
	 */
	public void setFile(File file) {
		this.file = file;
	}

}
