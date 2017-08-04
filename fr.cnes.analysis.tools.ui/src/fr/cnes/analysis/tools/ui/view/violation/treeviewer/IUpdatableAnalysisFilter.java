/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.view.violation.treeviewer;

public interface IUpdatableAnalysisFilter {

    public void update(String searchString, boolean showInfo, boolean showWarning,
                    boolean showError);
}
