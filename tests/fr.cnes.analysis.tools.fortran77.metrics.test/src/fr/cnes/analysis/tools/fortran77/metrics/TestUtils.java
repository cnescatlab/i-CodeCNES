package fr.cnes.analysis.tools.fortran77.metrics;

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
