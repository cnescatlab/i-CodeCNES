/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.data.xml;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.mapper.MapperWrapper;

import java.io.InputStream;

/**
 * Class used to unmarshal i-Code xml file (rules definition).
 *
 * It contains useful methods to handle xml files.
 */
public class XmlHandler {

    /**
     * Private constructor for this utility class.
     */
    private XmlHandler(){}

    /**
     * This method use XStream to unmarshal XML report: it transform simply
     * XML into our Java Object by reading annotations on model classes.
     *
     * @param file Stream of the xml file to import as Java Objects.
     * @param cls Destination class for unmarshalling.
     * @return AnalysisReport: the main structure of the report.
     */
    public static Object unmarshal(final InputStream file, final Class<?> cls){
        final XStream xStream = new XStream() {
            @Override
            protected MapperWrapper wrapMapper(final MapperWrapper next) {
                return new MapperWrapper(next) {
                    @Override
                    public boolean shouldSerializeMember(final Class definedIn, final String fieldName) {
                        return (definedIn != Object.class) && super.shouldSerializeMember(definedIn, fieldName);
                    }
                };
            }
        };
        xStream.processAnnotations(cls);
        return xStream.fromXML(file);
    }

}