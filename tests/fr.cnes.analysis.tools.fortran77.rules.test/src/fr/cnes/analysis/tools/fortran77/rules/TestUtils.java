/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/

package fr.cnes.analysis.tools.fortran77.rules;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;

import fr.cnes.analysis.tools.analyzer.datas.Violation;

public final class TestUtils {
	private TestUtils() {
		// do nothing
	}

	public static final String getViolations(final List<Violation> list) {
		String message = "line(s) -> ";
		for (final Violation value : list) {
			message = message + value.getLine().toString() + ", ";
		}
		return message;
	}

	public static final IConfigurationElement getContribution(final String id, final String name) {

		final IConfigurationElement contribution = mock(IConfigurationElement.class);

		when(contribution.getAttribute("id")).thenReturn(id);
		when(contribution.getAttribute("name")).thenReturn(name);

		return contribution;
	}
}
