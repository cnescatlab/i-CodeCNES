/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.data.xml;

import com.thoughtworks.xstream.converters.basic.AbstractSingleValueConverter;
import fr.cnes.icode.logger.ICodeLogger;

/**
 * XML converter for turning string into CheckerContainer's class.
 */
public class ClassXMLConverter extends AbstractSingleValueConverter {

    /**
     * Test if the submitted class can be converted with this converter.
     *
     * @param type Class to be converted.
     * @return boolean true if it can be converted.
     */
    @Override
    public boolean canConvert(final Class type) {
        return Class.class.equals(type);
    }

    /**
     * Convert a class name string into the corresponding Class object.
     *
     * @param str Fully qualified ILanguage class name as string.
     * @return An ILanguage object.
     */
    @Override
    public Object fromString(final String str) {
        Object result = null;
        try {
            result = Class.forName(str);
        } catch (final ClassNotFoundException e) {
            ICodeLogger.error(getClass().getName(), "fromString", e);
        }
        return result;
    }

}
