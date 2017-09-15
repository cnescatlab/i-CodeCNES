/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/
package fr.cnes.analysis.tools.shell.metrics;

/**
 * This class is intended to compute metric counting line of comment of a
 * function. To use this class read the {@link Function} and
 * {@link FunctionLineOfCode} specification first.
 * 
 * @since 3.0
 */
public class FunctionLineOfComment extends FunctionLineOfCode {
    /** Number of line of comment in the function */
    private float lineOfComment;

    /**
     * Constructor mostly for parameter of {@link Function} class. Line of
     * comment is set to zero on new instance.
     * 
     * @param pName
     *            of the function
     * @param pBeginLine
     *            of the function
     * @param pStarter
     *            {@link String} of the function's body
     */
    public FunctionLineOfComment(final String pName, final int pBeginLine, final String pStarter) {
        super(pName, pBeginLine, pStarter);
        lineOfComment = 0;
    }

    /**
     * Increment the number of lines of comment.
     */
    public void addLineOfComment() {
        this.lineOfComment++;
    }

    /**
     * @return the lineOfComment
     */
    public final float getLineOfComment() {
        return lineOfComment;
    }

    /**
     * @param pLineOfComment
     *            the lineOfComment to set
     */
    public final void setLineOfComment(float pLineOfComment) {
        this.lineOfComment = pLineOfComment;
    }
}
