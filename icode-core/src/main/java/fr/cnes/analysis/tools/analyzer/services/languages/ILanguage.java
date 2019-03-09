/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.analyzer.services.languages;

import java.util.List;

/**
 * Interface for language extensions.
 */
public interface ILanguage {

    /**
     * @return the name of the language.
     */
    String getName();

    /**
     * @return the id of the language.
     */
    String getId();

    /**
     * @return the list of extensions of the language.
     */
    List<String> getFileExtension();
}
