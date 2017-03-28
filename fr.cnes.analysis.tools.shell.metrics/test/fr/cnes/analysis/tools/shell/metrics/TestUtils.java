/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */ 
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/ 

package fr.cnes.analysis.tools.shell.metrics;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.eclipse.core.runtime.IConfigurationElement;

public final class TestUtils {
    private TestUtils() {
        // do nothing
    }

    public static final IConfigurationElement getContribution(final String id,
            final String name) {

        final IConfigurationElement contribution =
                mock(IConfigurationElement.class);

        when(contribution.getAttribute("id")).thenReturn(id);
        when(contribution.getAttribute("name")).thenReturn(name);

        return contribution;
    }
}
