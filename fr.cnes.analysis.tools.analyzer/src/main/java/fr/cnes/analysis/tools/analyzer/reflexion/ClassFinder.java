/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.analyzer.reflexion;

import org.reflections.Reflections;

import java.util.Collections;
import java.util.Set;

/**
 * Utility class for retrieving class from interface.
 *
 * @author lequal
 */
public class ClassFinder {

    public static Set<Class<?>> find(final Class f) throws Exception {

        final Reflections reflections = new Reflections("fr.cnes");
        final Set<Class<?>> classes;
        if(f.isInterface()) {
            classes = reflections.getSubTypesOf(f);
        } else {
            classes = Collections.singleton(f);
        }

        return classes;
    }

}
