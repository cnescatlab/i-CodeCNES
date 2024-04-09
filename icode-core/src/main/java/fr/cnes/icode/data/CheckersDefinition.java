/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.icode.data;

import com.google.common.collect.Lists;
import fr.cnes.icode.data.xml.XmlHandler;
import fr.cnes.icode.logger.ICodeLogger;
import fr.cnes.icode.services.checkers.CheckerContainer;

import java.io.InputStream;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

/**
 * Define available checkers for a given plugin.
 */
public abstract class CheckersDefinition {

    /** List of checkers defined in a plugin. **/
    private List<CheckerContainer> containers;

    /**
     * Default constructor.
     */
    public CheckersDefinition() {
        this.containers = Lists.newArrayList();
    }

    /**
     * Override this method to inject checkers in containers field.
     */
    public abstract void define();

    /**
     * Return checkers list defined in the corresponding plugin.
     *
     * @return list of checker declarations
     */
    public final List<CheckerContainer> list() {
        if(Objects.isNull(containers) || containers.isEmpty()) {
            define();
        }
        return containers;
    }

    /**
     * Setter for checkers.
     *
     * @param containers List of checkers as a List.
     */
    public final void setCheckers(final List<CheckerContainer> containers) {
        this.containers = containers;
    }

    /**
     * Add only one CheckerContainer object to the defined checkers.
     *
     * @param checkerContainer instance of CheckerContainer.
     */
    public final void add(final CheckerContainer checkerContainer) {
        if(Objects.isNull(this.containers)) {
            this.containers = Lists.newArrayList();
        }
        this.containers.add(checkerContainer);
    }

    /**
     * Add only multiple CheckerContainer objects to the defined checkers.
     *
     * @param collection instance of CheckerContainer.
     */
    public final void addAll(final Collection<CheckerContainer> collection) {
        if(Objects.isNull(this.containers)) {
            this.containers = Lists.newArrayList();
        }
        this.containers.addAll(collection);
    }

    /**
     * Add checkers from a file in resources. Xml file should have the following format:
     *
     * <checkers>
     *     <checker
     *          id="id"
     *          name="name"
     *          class="class"
     *          languageId="languageId"
     *          isMetric="boolean">
     *     </checker>
     *     <checker
     *          id="id"
     *          name="name"
     *          class="class"
     *          languageId="languageId"
     *          isMetric="boolean">
     *     </checker>
     * </checkers>
     *
     * @param file Resource file where are declared rules.
     */
    public final void addFromResources(final String file) {
        final InputStream checkersFile = this.getClass().getResourceAsStream(file);
        try {
            final CheckersList checkers = (CheckersList) XmlHandler.unmarshal(checkersFile);
            addAll(checkers.getCheckers());
        } catch (final Exception e) {
            ICodeLogger.error(getClass().getName(), "addFromResources", e);
        }
    }
}
