package fr.cnes.analysis.tools.shell.metrics;

import fr.cnes.analysis.tools.analyzer.exception.JFlexException;

public class Function {
    private String name;
    private int beginLine;
    private String starter;
    private int starterRepetition;

    public Function(final String pName, final int pBeginLine, final String pStarter) {
        this.name = pName;
        this.beginLine = pBeginLine;
        this.starter = pStarter;
        this.starterRepetition = 0;
    }

    public void addStarterRepetition() {
        starterRepetition++;
    }

    public void removeStarterRepetition() throws JFlexException {
        if (starterRepetition > 0) {
            starterRepetition--;
        } else {
            throw new JFlexException(
                    new Exception("Count of function's starter closure is negative."));
        }
    }

    public final String getFinisher() throws JFlexException {
        return Function.finisherOf(this.starter);
    }

    public static final String finisherOf(String str) throws JFlexException {
        String functionFinisher;
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
                throw new JFlexException(new Exception("Function's ender unknown."));
        }
        return functionFinisher;
    }

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
     * @param name
     *            the name to set
     */
    public final void setName(String name) {
        this.name = name;
    }

    /**
     * @return the beginLine
     */
    public final int getBeginLine() {
        return beginLine;
    }

    /**
     * @param beginLine
     *            the beginLine to set
     */
    public final void setBeginLine(int beginLine) {
        this.beginLine = beginLine;
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
