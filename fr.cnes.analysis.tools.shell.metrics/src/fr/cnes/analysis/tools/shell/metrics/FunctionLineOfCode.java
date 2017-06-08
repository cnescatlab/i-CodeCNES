package fr.cnes.analysis.tools.shell.metrics;

public class FunctionLineOfCode extends Function {
    private String name;
    private int beginLine;
    private String starter;
    private int lineOfCode;

    public FunctionLineOfCode(final String pName, final int pBeginLine, final String pStarter) {
        super(pName, pBeginLine, pStarter);
        lineOfCode = 0;
    }

    public void addLineOfCode() {
        this.lineOfCode++;
    }

    /**
     * @return the lineOfCode
     */
    public final int getLineOfCode() {
        return lineOfCode;
    }

    /**
     * @param lineOfCode
     *            the lineOfCode to set
     */
    public final void setLineOfCode(int lineOfCode) {
        this.lineOfCode = lineOfCode;
    }

}
