/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.fortran90;

import fr.cnes.icode.services.languages.ILanguage;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Definition of the Fortran 90 language.
 */
public class Fortran90Language implements ILanguage {

    /**
     * @return the name of the language.
     */
    public String getName() {
        return "Fortran 90";
    }

    /**
     * @return the id of the language.
     */
    public String getId() {
        return "fr.cnes.icode.fortran90";
    }

    /**
     * @return the list of extensions of the language.
     */
    public List<String> getFileExtension() {
        return Stream.of("f90", "F90").collect(Collectors.toList());
    }
}
