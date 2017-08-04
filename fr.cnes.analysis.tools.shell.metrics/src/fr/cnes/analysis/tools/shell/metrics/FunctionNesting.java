/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/
package fr.cnes.analysis.tools.shell.metrics;

import java.util.Stack;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

/**
 * This class is intended to compute Nesting of a function for SHELL language.
 * To use it, please read first {@link Function} documentation.
 * 
 * @since 3.0
 */
/**
 * @author waldmao
 *
 */
public class FunctionNesting extends Function {

    /** Element nested in the function */
    private Stack<String> nesting;
    /**
     * Maximal number of element nested inside the function (including function
     * nesting)
     */
    private int maxNesting;
    /**
     * Number of element nested in the function and not closed during static
     * analysis.
     */
    private int currentNesting;

    /**
     * Constructor
     * 
     * @param pName
     *            of the function
     * @param pBeginLine
     *            of the function
     * @param pStarter
     *            {@link String} of the function's body
     * @param pInitialMaxValue
     *            initial value of the maximal nesting
     * @param pInitialNesting
     *            initial value of nesting on new instance.
     */
    public FunctionNesting(String pName, int pBeginLine, String pStarter, int pInitialMaxValue,
                    int pInitialNesting) {
        super(pName, pBeginLine, pStarter);
        this.nesting = new Stack<>();
        this.maxNesting = pInitialMaxValue;
        this.currentNesting = pInitialNesting;
    }

    /**
     * @param str
     *            String to verify
     * @return whether or not <code>str</code> parameter is increasing nesting.
     */
    public static final boolean isNesting(String str) {
        boolean isNesting;
        switch (str) {
        case "while":
        case "for":
        case "until":
        case "select":
        case "if":
        case "case":
            isNesting = true;
            break;
        default:
            isNesting = false;
            break;
        }
        return isNesting;
    }

    /**
     * @param str
     *            String to get finisher of
     * @return character or string excepted as finisher of <code>str</code>.
     * @throws JFlexException
     *             when the finisher is unknown.
     */
    public static final String nestingFinisherOf(String str) throws JFlexException {
        final String nestingFinisher;
        switch (str) {
        case "while":
        case "for":
        case "until":
        case "select":
            nestingFinisher = "done";
            break;
        case "if":
            nestingFinisher = "fi";
            break;
        case "case":
            nestingFinisher = "esac";
            break;
        default:
            throw new JFlexException(new Exception("Nesting finisher unknown."));
        }
        return nestingFinisher;
    }

    /**
     * Increase and push nesting then compute, if required, maximum value of
     * nesting of the function.
     * 
     * @param nestingBeginner
     *            Nesting beginner (char or str) of the nesting pushed in the
     *            function.
     */
    public void pushNesting(String nestingBeginner) {
        this.nesting.push(nestingBeginner);
        this.currentNesting++;
        if (this.currentNesting > this.maxNesting) {
            this.maxNesting = this.currentNesting;
        }
    }

    /**
     * Pop a nesting of a function once it's finisher is crossed. Decrease
     * function's nesting.
     * 
     * @return Nesting beginner (char or str) of the latest nesting popped from
     *         the function.
     * @throws JFlexException
     *             when trying to pop nesting stack while it's empty.
     */
    public String popNesting() throws JFlexException {
        if (!this.nesting.empty()) {
            this.currentNesting--;
            return this.nesting.pop();
        } else {
            throw new JFlexException(new Exception("Pop is impossible because the stack is empty"));

        }

    }

    /**
     * @param str
     *            String to verify
     * @return whether or not the String is closing current nesting.
     * @throws JFlexException
     *             when no finisher can be recognized from the parameter
     */
    public boolean isNestingFinisher(String str) throws JFlexException {
        return FunctionNesting.finisherOf(this.nesting.peek()).equals(str);
    }

    /**
     * @return Nesting finisher (char or str) of the current nesting.
     * @throws JFlexException
     *             when no finisher can be recognized from the parameter
     */
    public String getNestingFinisher() throws JFlexException {
        return FunctionNesting.finisherOf(this.nesting.peek());
    }

    /**
     * @return the maxNesting
     */
    public final int getMaxNesting() {
        return maxNesting;
    }

    /**
     * @param pMaxNesting
     *            the maxNesting to set
     */
    public final void setMaxNesting(int pMaxNesting) {
        this.maxNesting = pMaxNesting;
    }

    /**
     * @return the currentNesting
     */
    public final int getCurrentNesting() {
        return currentNesting;
    }

    /**
     * @param pCurrentNesting
     *            the currentNesting to set
     */
    public final void setCurrentNesting(int pCurrentNesting) {
        this.currentNesting = pCurrentNesting;
    }

    /**
     * @return the nesting
     */
    public final Stack<String> getNesting() {
        return nesting;
    }

    /**
     * @param pNesting
     *            the nesting to set
     */
    public final void setNesting(Stack<String> pNesting) {
        this.nesting = pNesting;
    }
}
