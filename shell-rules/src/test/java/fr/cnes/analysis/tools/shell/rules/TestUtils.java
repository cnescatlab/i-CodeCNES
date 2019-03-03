/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/

package fr.cnes.analysis.tools.shell.rules;

import java.util.List;

import fr.cnes.analysis.tools.analyzer.datas.CheckResult;

public final class TestUtils {
	private TestUtils() {
		// do nothing
	}

	public static final String getCheckResults(final List<CheckResult> list) {
		String message = "line(s), location(s) : ";
		for (final CheckResult value : list) {
			message = message + "\n    " + value.getLine().toString() + ", " + value.getLocation();
		}
		return message;
	}
}
