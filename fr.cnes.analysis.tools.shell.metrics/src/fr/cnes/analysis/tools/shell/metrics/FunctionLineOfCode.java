package fr.cnes.analysis.tools.shell.metrics;

/**
 * This class is intended to count the number of line of a file in SHELL
 * language. To use this class, read the {@link Function} documentation.
 * 
 * @since 3.0
 */
public class FunctionLineOfCode extends Function {
    /** Number of line of code of the function */
    private float lineOfCode;

    /**
     * Constructor mostly for parameter of {@link Function} class. Line of code
     * is set to zero on new instance.
     * 
     * @param pName
     *            of the function
     * @param pBeginLine
     *            of the function
     * @param pStarter
     *            {@link String} of the function's body
     */
    public FunctionLineOfCode(final String pName, final int pBeginLine, final String pStarter) {
        super(pName, pBeginLine, pStarter);
        lineOfCode = 0;
    }

    /**
     * Increment number of line of code.
     */
    public void addLineOfCode() {
        this.lineOfCode++;
    }

    /**
     * @return the lineOfCode
     */
    public final float getLineOfCode() {
        return lineOfCode;
    }

    /**
     * @param lineOfCode
     *            the lineOfCode to set
     */
    public final void setLineOfCode(float lineOfCode) {
        this.lineOfCode = lineOfCode;
    }

}
