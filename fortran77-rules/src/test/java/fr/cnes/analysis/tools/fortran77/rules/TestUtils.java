/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

import java.util.List;

public final class TestUtils {
	private TestUtils() {
		// do nothing
	}

	public static final String getCheckResults(final List<CheckResult> list) {
		String message = "line(s) -> ";
		for (final CheckResult value : list) {
			message = message + value.getLine().toString() + ", ";
		}
		return message;
	}
}
