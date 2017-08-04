/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.export.xml;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.jdom2.Attribute;
import org.jdom2.Document;
import org.jdom2.Element;
import org.jdom2.output.Format;
import org.jdom2.output.XMLOutputter;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.export.IExport;

/**
 * This class is an attribute of the {@code ExtensionPoint} implementing
 * {@link IExport} interface of the plug-in
 * <i>fr.cnes.analysis.tools.export</i>.
 * <p>
 * This class is also exported in the <i>fr.cnes.analysis.tools.export.csv</i>
 * plug-in and could be used as a service from any third.
 * </p>
 * <p>
 * This class is responsible of the export in the format XML of
 * {@link CheckResult} elements into a {@link File}.
 * </p>
 * 
 * @since 3.0
 */
public class ExportXml implements IExport {

    /** Identifier of the attribute <i>AnalysisProjectName</i>. */
    public static final String ATTRIBUTE_ANALYSISPROJECTNAME = "analysisProjectName";
    /** Identifier of the attribute <i>AnalysisProjectVersion</i>. */
    public static final String ATTRIBUTE_ANALYSISPROJECTVERSION = "analysisProjectVersion";
    /** Identifier of element <i>AnalysisProject</i>. */
    public static final String ELEMENT_ANALYSISPROJECT = "analysisProject";
    /** <i>PROJECT_NAME </i> parameter. */
    public static final String PARAM_PROJECT_NAME = "ProjectName";
    /** Default <i>AUTHOR</i> parameter. */
    public static final String PARAM_AUTHOR = "Analysis author";
    /** Default <i>PROJECT_VERSION</i> parameter. */
    public static final String PARAM_PROJECT_VERSION = "ProjectVersion";
    /** <i>CONFIGURATION_ID</i> parameter. */
    public static final String PARAM_CONFIGURATION_ID = "AnalysisConfigurationID";
    /** DEfault <i>analysisDate</i> parameter */
    public static final String PARAM_DATE = "Date";
    /** Parameter required by this export. */
    private Map<String, String> parameters;

    /**
     * Default constructor. Required to execute a class from the contributed
     * extension point.
     */
    public ExportXml() {
        this.parameters = new TreeMap<>();
        this.parameters.put(PARAM_PROJECT_NAME, "Unknown");
        this.parameters.put(PARAM_AUTHOR, "i-Code CNES Analyzer");
        this.parameters.put(PARAM_PROJECT_VERSION, "1.0.0");
        this.parameters.put(PARAM_DATE, this.currentDate());
        this.parameters.put(PARAM_CONFIGURATION_ID, "analysis1");

    }

    /**
     * @return current date.
     */
    private String currentDate() {
        final String format = "YYYY-MM-dd";
        final SimpleDateFormat formater = new SimpleDateFormat(format);
        final Date date = new Date();
        return (formater.format(date));
    }

    /*
     * (non-Javadoc)
     * 
     * @see fr.cnes.analysis.tools.export.IExport#export(java.util.List,
     * java.io.File)
     */
    @Override
    public void export(List<CheckResult> checkResults, File outputFile,
                    Map<String, String> pParameters) throws IOException {
        final List<Attribute> attributes = new ArrayList<Attribute>();
        final List<Attribute> resultAttributes = new ArrayList<Attribute>();
        /*
         * Creation of the root element <xsd:element name="analysisProject"
         * type="anr:analysisProjectType" minOccurs="1" maxOccurs="1" />
         */

        final Element analysisProjectElement = new Element(ELEMENT_ANALYSISPROJECT);
        analysisProjectElement.setAttribute(new Attribute(ATTRIBUTE_ANALYSISPROJECTNAME,
                        pParameters.get(PARAM_PROJECT_NAME)));
        analysisProjectElement.setAttribute(new Attribute(ATTRIBUTE_ANALYSISPROJECTVERSION,
                        pParameters.get(PARAM_PROJECT_VERSION)));
        final Document document = new Document(analysisProjectElement);

        // BEGINNING OF SEQUENCE <xsd:sequence>
        // -- <xsd:element name="analysisInformations"
        // -- type="anr:analysisInformationsType" minOccurs="1"
        // maxOccurs="1">

        final Element analysisInformation = new Element("analysisInformations");
        attributes.add(new Attribute("analysisConfigurationId",
                        pParameters.get(PARAM_CONFIGURATION_ID)));
        attributes.add(new Attribute("analysisDate", pParameters.get(PARAM_DATE)));
        attributes.add(new Attribute("author", pParameters.get(PARAM_AUTHOR)));

        analysisInformation.setAttributes(attributes);
        document.getRootElement().addContent(analysisInformation);
        // -- <xsd:element name="analysisFile" type="anr:analysisFileType"
        // -- minOccurs="0" maxOccurs="unbounded" />
        for (final CheckResult checkResult : checkResults) {
            attributes.clear();

            // -- <xsd:attribute name="fileName" type="xsd:string"
            // -- use="required" />
            // Note : We take only the last segment of the filepath to
            // get
            // the filename.
            // -- <xsd:attribute name="language" type="xsd:string" />
            final String language = this.getFileExtension(checkResult.getFile().getAbsolutePath());
            final String fileName = checkResult.getFile().toString();
            // The analysisFile element is being added only and only if it's not
            // already in the XML document.
            boolean analysisFileMarked = false;
            for (final Element element : document.getRootElement().getChildren("analysisFile")) {
                if (element.getAttributeValue("fileName").equals(fileName)
                                && element.getAttributeValue("language").equals(language)) {
                    analysisFileMarked = true;
                }
            }
            if (!analysisFileMarked) {
                final Element analysisFile = new Element("analysisFile");
                attributes.add(new Attribute("language", language));
                attributes.add(new Attribute("fileName", fileName));
                analysisFile.setAttributes(attributes);
                document.getRootElement().addContent(analysisFile);
            }
        }
        Element result = null;
        Element analysisRule = null;
        int resultId = 1;
        for (final CheckResult checkResult : checkResults) {
            boolean elementAlreadyExisting = false;
            Element existingElement = null;
            for (final Element element : document.getRootElement().getChildren("analysisRule")) {
                for (final Attribute attribute : element.getAttributes()) {
                    if (attribute.getName().equals("analysisRuleId")
                                    && attribute.getValue().equals(checkResult.getId())) {
                        elementAlreadyExisting = true;
                        existingElement = element;
                    }
                }
            }
            if (!elementAlreadyExisting || existingElement == null) {
                analysisRule = new Element("analysisRule");
                analysisRule.setAttribute(new Attribute("analysisRuleId", checkResult.getName()));
            } else {
                analysisRule = existingElement;
            }

            result = new Element("result");
            resultAttributes.clear();
            // no justification for now
            // no outputDetail for now neither

            resultAttributes.add(new Attribute("resultId", Integer.toString(resultId)));
            resultId++;
            resultAttributes.add(new Attribute("fileName", checkResult.getFile().toString()));
            resultAttributes.add(new Attribute("resultLine", checkResult.getLine().toString()));

            /*
             * In case no location is defined, it means the violations or the
             * metric is intended to a file. This way, there is no
             * resultNamePlace but the resultTypePlace will be "class".
             */
            if (checkResult.getLocation() == null || checkResult.getLocation().isEmpty()) {
                resultAttributes.add(new Attribute("resultTypePlace", "class"));
            } else {
                resultAttributes.add(new Attribute("resultTypePlace", "method"));
                resultAttributes.add(new Attribute("resultNamePlace", checkResult.getLocation()));
            }
            /*
             * If the analysis checkresult has a result, we add it.
             */

            if (checkResult.getValue() != null) {
                resultAttributes.add(
                                new Attribute("resultValue", checkResult.getValue().toString()));
            }
            /*
             * 
             * The result message is defined by the XSD as a sequence of element
             * resultMessage (not an attribute).
             */
            if (checkResult.getMessage() != null && !checkResult.getMessage().isEmpty()) {
                final Element resultMessage = new Element("resultMessage");

                resultMessage.addContent(checkResult.getMessage());

                result.addContent(resultMessage);
            }
            result.setAttributes(resultAttributes);
            /*
             * The result is being added to the analysisRule element;
             */
            analysisRule.addContent(result);

            /*
             * If the rule analysisRule was already in the document, then it's
             * not necessary to add it again in the document, however if it's
             * the first time that this analysisRule appear in the document we
             * have to add it to the Root element.
             */
            if (!elementAlreadyExisting) {
                document.getRootElement().addContent(analysisRule);
            }
        }

        final XMLOutputter xmlOutput = new XMLOutputter();
        xmlOutput.setFormat(Format.getPrettyFormat());
        try (final FileOutputStream fileOutput = new FileOutputStream(outputFile)) {
            xmlOutput.output(document, fileOutput);
            fileOutput.close();
        }

    }

    /**
     * This function search for the extension of a file and returns it.
     * 
     * @param filePath
     *            to search extension on.
     * @return The file's extension.
     */
    protected String getFileExtension(String filePath) {
        String extension = "unknown";

        final int i = filePath.lastIndexOf('.');
        final int p = Math.max(filePath.lastIndexOf('/'), filePath.lastIndexOf('\\'));

        if (i > p) {
            extension = filePath.substring(i + 1);
        }
        return extension;
    }

    @Override
    public boolean hasParameters() {
        return true;
    }

    @Override
    public Map<String, String> getParameters() {
        return this.parameters;
    }

}
