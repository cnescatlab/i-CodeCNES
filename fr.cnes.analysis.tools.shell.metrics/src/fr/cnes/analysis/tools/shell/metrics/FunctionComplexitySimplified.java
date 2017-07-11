package fr.cnes.analysis.tools.shell.metrics;

public class FunctionComplexitySimplified extends Function {

    private float complexity;

    public FunctionComplexitySimplified(String pName, int pBeginLine, String pStarter) {
        super(pName, pBeginLine, pStarter);
        this.complexity = 1;
        this.complexity += computeComplexity(pStarter);
    }

    public static float computeComplexity(String pStarter) {
        float complexity;
        switch (pStarter) {
            case "while":
            case "for":
            case "until":
                complexity = 2;
                break;
            case "if":
            case "else":
            case "elif":
                complexity = 1;
                break;
            default:
                complexity = 0;
        }
        return complexity;

    }

    public void computeCase() {
        this.complexity++;
    }

    public void addComplexity(float complexity) {
        this.complexity += complexity;
    }

    public float getComplexity() {
        return this.complexity;
    }

}
