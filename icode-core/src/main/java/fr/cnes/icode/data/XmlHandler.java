/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.data;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import java.io.File;
import java.io.InputStream;

/**
 * Class used to unmarshal i-Code xml file (rules definition).
 *
 * It contains useful methods to handle xml files.
 *
 * @author lequal
 */
public class XmlHandler {

    /**
     * Private constructor for this utility class.
     */
    private XmlHandler(){}

    /**
     * This method use JAXB to unmarshal XML report: it transform simply
     * XML into our Java Object by reading annotations on model classes.
     *
     * @param file File descriptor of the report to import as Java Objects.
     * @param cls Destination class for unmarshalling.
     * @return AnalysisReport: the main structure of the report.
     * @throws JAXBException Exception during conversion can be met.
     */
    public static Object unmarshal(final File file, final Class<?> cls) throws JAXBException {
        final JAXBContext jaxbContext = JAXBContext.newInstance(cls);
        final Unmarshaller jaxbUnmarshaller = jaxbContext.createUnmarshaller();
        return jaxbUnmarshaller.unmarshal(file);
    }

    /**
     * This method use JAXB to unmarshal XML report: it transform simply
     * XML into our Java Object by reading annotations on model classes.
     *
     * @param file Stream of the xml file to import as Java Objects.
     * @param cls Destination class for unmarshalling.
     * @return AnalysisReport: the main structure of the report.
     * @throws JAXBException Exception during conversion can be met.
     */
    public static Object unmarshal(final InputStream file, final Class<?> cls) throws JAXBException {
        final JAXBContext jaxbContext = JAXBContext.newInstance(cls);
        final Unmarshaller jaxbUnmarshaller = jaxbContext.createUnmarshaller();
        return jaxbUnmarshaller.unmarshal(file);
    }

}