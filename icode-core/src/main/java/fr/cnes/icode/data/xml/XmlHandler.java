/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.data.xml;

import fr.cnes.icode.data.CheckersList;
import fr.cnes.icode.exception.NullContributionException;
import fr.cnes.icode.logger.ICodeLogger;
import fr.cnes.icode.services.checkers.CheckerContainer;
import fr.cnes.icode.services.languages.ILanguage;
import fr.cnes.icode.services.languages.LanguageService;
import org.json.JSONObject;
import org.json.XML;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Map;

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
     * @return AnalysisReport: the main structure of the report.
     */
    public static Object unmarshal(final InputStream file){
        final CheckersList checkersList = new CheckersList();
        final JSONObject jsonObj = XML.toJSONObject(new InputStreamReader(file));
        final List<Object> jsonList = jsonObj.getJSONObject("checkers").getJSONArray("check").toList();

        for(final Object checker : jsonList) {
            final Map map = (Map) checker;

            final CheckerContainer container = new CheckerContainer(
                    map.get("id").toString(),
                    map.get("name").toString(),
                    languageFromString(map.get("languageId").toString()),
                    classFromString(map.get("class").toString()),
                    (boolean) map.getOrDefault("isMetric", false));

            checkersList.getCheckers().add(container);
        }

        return checkersList;
    }

    /**
     * Convert a class name string into the corresponding ILanguage object.
     *
     * @param str Fully qualified ILanguage class name as string.
     * @return An ILanguage object.
     */
    public static ILanguage languageFromString(final String str) {
        ILanguage result = null;
        try {
            result = LanguageService.getLanguage(str);
        } catch (final NullContributionException e) {
            ICodeLogger.error(XmlHandler.class.getName(), "languageFromString", e);
        }
        return result;
    }

    /**
     * Convert a class name string into the corresponding Class object.
     *
     * @param str Fully qualified ILanguage class name as string.
     * @return An ILanguage object.
     */
    public static Class<?> classFromString(final String str) {
        Class<?> result = null;
        try {
            result = Class.forName(str);
        } catch (final ClassNotFoundException e) {
            ICodeLogger.error(XmlHandler.class.getName(), "classFromString", e);
        }
        return result;
    }

}