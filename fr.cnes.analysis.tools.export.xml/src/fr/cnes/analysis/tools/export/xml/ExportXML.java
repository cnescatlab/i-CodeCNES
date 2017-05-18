package fr.cnes.analysis.tools.export.xml;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.jdom2.Attribute;
import org.jdom2.Document;
import org.jdom2.Element;
import org.jdom2.output.Format;
import org.jdom2.output.XMLOutputter;

import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.export.IExport;

public class ExportXML implements IExport {
	
	public static String OPTION_SEPARATOR = "=";
	public static String ATTRIBUTE_AnalysisProjectName;
	public static String ELEMENT_AnalysisProject;
	private String analysisProject  = "unknown";
	private String analysisDate = "0000-00-00";
	private String analysisAuthor = "unknown";
	
	public ExportXML(){
		
	}

	@Override
	public void exportViolation(List<Violation> violations, File file) throws IOException{
        final List<Attribute> attributes = new ArrayList<Attribute>();
        final List<Attribute> resultAttributes = new ArrayList<Attribute>();
        /*
         * Creation of the root element <xsd:element name="analysisProject"
         * type="anr:analysisProjectType" minOccurs="1" maxOccurs="1" />
         */

        final Element analysisProjectElement = new Element("analysisProject");
        analysisProjectElement
                .setAttribute(new Attribute("analysisProjectName",analysisProject));
        final Document document = new Document(analysisProjectElement);

        // BEGINNING OF SEQUENCE <xsd:sequence>
        // -- <xsd:element name="analysisInformations"
        // -- type="anr:analysisInformationsType" minOccurs="1"
        // maxOccurs="1">

        final Element analysisInformation = new Element("analysisInformations");
        attributes.add(new Attribute("analysisConfigurationId", "standard"));
        attributes.add(new Attribute("analysisDate", analysisDate));
        attributes.add(new Attribute("author", analysisAuthor));

        analysisInformation.setAttributes(attributes);
        document.getRootElement().addContent(analysisInformation);
        // -- <xsd:element name="analysisFile" type="anr:analysisFileType"
        // -- minOccurs="0" maxOccurs="unbounded" />
        for (final Violation violation : violations) {
            attributes.clear();

            // -- <xsd:attribute name="fileName" type="xsd:string"
            // -- use="required" />
            // Note : We take only the last segment of the filepath to
            // get
            // the filename.
            // -- <xsd:attribute name="language" type="xsd:string" />
            final String language = this.getFileExtension(violation.getFile().getAbsolutePath());
            final String fileName = violation.getFile().toString();
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
        Element result, analysisRule = null;
        int resultId = 1;
        for (final Violation violation : violations) {
            boolean elementAlreadyExisting = false;
            Element existingElement = null;
            for (final Element element : document.getRootElement().getChildren("analysisRule")) {
                for (final Attribute attribute : element.getAttributes()) {
                    if (attribute.getName().equals("analysisRuleId")
                            && attribute.getValue().equals(violation.getRuleId())) {
                        elementAlreadyExisting = true;
                        existingElement = element;
                    }
                }
            }
            if (!elementAlreadyExisting || existingElement == null) {
                analysisRule = new Element("analysisRule");
                analysisRule.setAttribute(new Attribute("analysisRuleId", violation.getRuleName()));
            } else {
                analysisRule = existingElement;
            }

            result = new Element("result");
            resultAttributes.clear();
            // no justification for now
            // no outputDetail for now neither

            resultAttributes.add(new Attribute("resultId", Integer.toString(resultId)));
            resultId++;
            resultAttributes.add(new Attribute("fileName", violation.getFile().toString()));
            resultAttributes.add(new Attribute("resultLine", violation.getLine().toString()));

            /*
             * The location and message are defined in violations by only one
             * attribute (String) made this way: [Location -> Message], so we
             * split it to get the two exploitable strings.
             */
            resultAttributes.add(new Attribute("resultNamePlace", violation.getLocation()));
            /*
             * The result message is defined by the XSD as a sequence of element
             * resultMessage (not an attribute).
             */
            final Element resultMessage = new Element("resultMessage");

            resultMessage.addContent(violation.getMessage());

            result.addContent(resultMessage);
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
        final FileOutputStream fileOutput = new FileOutputStream(file);

        xmlOutput.output(document, fileOutput);
        fileOutput.close();

		
	}

	

    /**
     * @param fileName
     * @return The extension name of the file
     */
    protected String getFileExtension(String fileName) {
    	String extension = "unknown";

    	int i = fileName.lastIndexOf('.');
    	int p = Math.max(fileName.lastIndexOf('/'), fileName.lastIndexOf('\\'));

    	if (i > p) {
    	    extension = fileName.substring(i+1);
    	}
		return extension;
	}




}

