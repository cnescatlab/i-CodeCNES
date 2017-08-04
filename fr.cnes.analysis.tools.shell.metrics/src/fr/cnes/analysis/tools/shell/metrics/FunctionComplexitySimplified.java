/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/
package fr.cnes.analysis.tools.shell.metrics;

/**
 * This class can be used to compute ComplexitySimplified rules function.
 */
public class FunctionComplexitySimplified extends Function {

    /** Complexity computed for the function */
    private float complexity;

    /**
     * @param pName
     *            Function's name
     * @param pBeginLine
     *            Function's line
     * @param pStarter
     *            Function's starter.
     */
    public FunctionComplexitySimplified(String pName, int pBeginLine, String pStarter) {
        super(pName, pBeginLine, pStarter);
        this.complexity = 1;
        this.complexity += computeComplexity(pStarter);
    }

    /**
     * @param pStarter
     *            Structure of control beginner
     * @return complexity associated to <code>pStarter</code>
     */
    public static float computeComplexity(String pStarter) {
        final float complexity;
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

    /**
     * Increase complexity
     */
    public void computeCase() {
        this.complexity++;
    }

    /**
     * Increase complexity by <code>pComplexity</code>.
     * 
     * @param pComplexity
     *            complexity to add.
     */
    public void addComplexity(float pComplexity) {
        this.complexity += pComplexity;
    }

    /**
     * @return complexity
     */
    public float getComplexity() {
        return this.complexity;
    }

}
