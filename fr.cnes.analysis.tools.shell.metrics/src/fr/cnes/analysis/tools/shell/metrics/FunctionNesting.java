package fr.cnes.analysis.tools.shell.metrics;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import java.util.Stack;

/**
 * This class is intended to compute Nesting of a function for SHELL language.
 * To use it, please read first {@link Function} documentation.
 * 
 * @since 3.0
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

    public static final boolean isNesting(String str) {
        return str.equals("while") | str.equals("for") | str.equals("until") | str.equals("select")
                | str.equals("if") | str.equals("case");
    }

    public static final String nestingFinisherOf(String str) throws JFlexException {
        String nestingFinisher;
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

    public void pushNesting(String nestingBeginner) {
        this.nesting.push(nestingBeginner);
        this.currentNesting++;
        if (this.currentNesting > this.maxNesting) {
            this.maxNesting = this.currentNesting;
        }
    }

    public String popNesting() throws JFlexException {
        if (!this.nesting.empty()) {
            this.currentNesting--;
            return this.nesting.pop();
        } else {
            throw new JFlexException(new Exception("Pop is impossible because the stack is empty"));

        }

    }

    public boolean isNestingFinisher(String str) throws JFlexException {
        return FunctionNesting.finisherOf(this.nesting.peek()).equals(str);
    }

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
     * @param maxNesting
     *            the maxNesting to set
     */
    public final void setMaxNesting(int maxNesting) {
        this.maxNesting = maxNesting;
    }

    /**
     * @return the currentNesting
     */
    public final int getCurrentNesting() {
        return currentNesting;
    }

    /**
     * @param currentNesting
     *            the currentNesting to set
     */
    public final void setCurrentNesting(int currentNesting) {
        this.currentNesting = currentNesting;
    }

    /**
     * @return the nesting
     */
    public final Stack<String> getNesting() {
        return nesting;
    }

    /**
     * @param nesting
     *            the nesting to set
     */
    public final void setNesting(Stack<String> nesting) {
        this.nesting = nesting;
    }
}
