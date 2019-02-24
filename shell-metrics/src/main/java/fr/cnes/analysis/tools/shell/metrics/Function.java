/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/
package fr.cnes.analysis.tools.shell.metrics;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

/**
 * This class contains data that are commonly used by metrics to identify a
 * SHELL function. This can be useful to locate a function and be implemented to
 * include special details in it.
 * 
 * <h2>The starter repetition</h2>
 * <p>
 * The {@link #starterRepetition} attribute must be used to count how many times
 * a structure of control inside the function (that isn't a function itself) is
 * being started while having the same closing as the instantiated
 * {@link Function}. This way, it's possible to define on a function element
 * parsing if it's intended to close the function or a control structure inside
 * of the function.
 * </p>
 * <h2>Exceptions</h2>
 * <p>
 * This class throws only {@link JFlexException} to fulfill analyzer
 * independence of plugging in extension. It's thrown when :
 * <ul>
 * <li>It's not possible to reach a finisher for a string requested
 * <li>While trying to reduce the number of repetition of the starter while it's
 * value is zero or less</li>
 * </ul>
 * 
 * 
 * @since 3.0
 */
public class Function {
    /** Function's name */
    private String name;
    /** Function's line */
    private int beginLine;
    /** Function starter */
    private String starter;
    /**
     * Number of control structure having the same closing as the function's one
     * inside the function
     */
    private int starterRepetition;

    /**
     * Constructor for {@link Function} object.
     * 
     * @param pName
     *            name of the function
     * @param pBeginLine
     *            line number of the function
     * @param pStarter
     *            element starting the function's body.
     */
    public Function(final String pName, final int pBeginLine, final String pStarter) {
        this.name = pName;
        this.beginLine = pBeginLine;
        this.starter = pStarter;
        this.starterRepetition = 0;
    }

    /**
     * Increment the number of structure having the same closing as the current
     * function
     */
    public void addStarterRepetition() {
        starterRepetition++;
    }

    /**
     * Decrement the number of structure having same closing as the current
     * function. To use on the closing of one of them.
     * 
     * @throws JFlexException
     *             when the number of repetition below or equal zero.
     */
    public void removeStarterRepetition() throws JFlexException {
        if (starterRepetition > 0) {
            starterRepetition--;
        } else {
            throw new JFlexException(
                            new Exception("Count of function's starter closure is negative."));
        }
    }

    /**
     * @return the {@link String} element that would close the function.
     * @throws JFlexException
     *             when no finisher can be recognized from the parameter
     */
    public final String getFinisher() throws JFlexException {
        return Function.finisherOf(this.starter);
    }

    /**
     * @param str
     *            the {@link String} beginning a control structure or a
     *            function.
     * @return the {@link String} which should be considered as the function
     *         closer of the parameter
     * @throws JFlexException
     *             when no finisher can be recognized from the parameter.
     */
    public static final String finisherOf(String str) throws JFlexException {
        final String functionFinisher;
        switch (str) {
        case "if":
            functionFinisher = "fi";
            break;
        case "case":
            functionFinisher = "esac";
            break;
        case "select":
        case "for":
        case "while":
        case "until":
            functionFinisher = "done";
            break;
        case "{":
            functionFinisher = "}";
            break;
        case "(":
            functionFinisher = ")";
            break;
        case "((":
            functionFinisher = "))";
            break;
        case "[[":
            functionFinisher = "]]";
            break;
        default:
            throw new JFlexException(new Exception("Function's finisher unknown."));
        }
        return functionFinisher;
    }

    /**
     * @param str
     *            the string to compare
     * @return if the parameter can close the function or not.
     * @throws JFlexException
     *             when no finisher can be recognized from the parameter
     */
    public final boolean isFinisher(String str) throws JFlexException {
        return str.equals(this.getFinisher());
    }

    /**
     * @return the name
     */
    public final String getName() {
        return name;
    }

    /**
     * @param pName
     *            the name to set
     */
    public final void setName(String pName) {
        this.name = pName;
    }

    /**
     * @return the beginLine
     */
    public final int getBeginLine() {
        return beginLine;
    }

    /**
     * @param pBeginLine
     *            the beginLine to set
     */
    public final void setBeginLine(int pBeginLine) {
        this.beginLine = pBeginLine;
    }

    /**
     * @return the string starting the fucntion
     */
    public final String getStarter() {
        return starter;
    }

    /**
     * @param pStarter
     *            String starting the function
     */
    public final void setStarter(String pStarter) {
        this.starter = pStarter;
    }

    /**
     * @return the starterRepetition
     */
    public final int getStarterRepetition() {
        return starterRepetition;
    }

}
