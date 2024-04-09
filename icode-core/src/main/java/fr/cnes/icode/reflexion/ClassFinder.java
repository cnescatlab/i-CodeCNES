/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.reflexion;

import org.reflections.Reflections;

import java.lang.reflect.Modifier;
import java.util.HashSet;
import java.util.Set;

/**
 * Utility class for retrieving class from interface.
 */
public class ClassFinder {

    public static Set<Class<?>> find(final Class f) {

        final Reflections reflections = new Reflections("fr.cnes");
        final Set<Class<?>> classes = new HashSet<>();
        if(Modifier.isInterface(f.getModifiers()) || Modifier.isAbstract(f.getModifiers())) {
            Set<Class<?>> tmp = reflections.getSubTypesOf(f);
            for(Class cls : tmp) {
                classes.addAll(find(cls));
            }
        } else {
            classes.add(f);
        }
        return classes;
    }

}
