/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.utils;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.services.ISourceProviderService;

import fr.cnes.analysis.tools.ui.exception.EmptyProviderException;
import fr.cnes.analysis.tools.ui.preferencepage.LangagePreferenceContainer;
import fr.cnes.analysis.tools.ui.services.LanguageSourceProvider;

/**
 * Contains tools methods for source provider purpose.
 * 
 */
public final class SourceProviderUIUtils {
    /** Logger **/
    private static final Logger LOGGER = Logger.getLogger(SourceProviderUIUtils.class.getName());

    /**
     * Protected constructor. This class should not be instantiated.
     */
    private SourceProviderUIUtils() {
        // do nothing
    }

    /**
     * Get the value of a provider considering a preference id.
     * 
     * @param field
     *            the preference id
     * @return the value of considered provider
     * @throws EmptyProviderException
     *             whenever the provider source is not found
     */
    public static Boolean getLanguageProvider(final String field) throws EmptyProviderException {
        LOGGER.finest("Begin setProviderValue method");

        Boolean value = null;

        // Load active window
        final IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();

        // Load provider service
        final ISourceProviderService service = (ISourceProviderService) window
                .getService(ISourceProviderService.class);

        // Load provider
        final LanguageSourceProvider provider = (LanguageSourceProvider) service
                .getSourceProvider(field);

        // Set enabled depending on field value
        if (provider == null) {
            final EmptyProviderException exception = new EmptyProviderException(
                    "No source provider for " + field);
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            throw exception;
        } else {
            value = provider.getEnabled().get(field);
        }

        LOGGER.finest("End setProviderValue method");
        return value;
    }

    /**
     * Get the value of a provider considering a preference container.
     * 
     * @param field
     *            the preference container
     * @return the value of considered provider
     * @throws EmptyProviderException
     *             whenever the provider source is not found
     */
    public static Boolean getLanguageProvider(final LangagePreferenceContainer field)
            throws EmptyProviderException {
        return getLanguageProvider(field.getPrefId());
    }

    /**
     * Set the value of a provider considering a preference id.
     * 
     * @param providerId
     *            the preference id
     * @param value
     *            the value to set
     * @throws EmptyProviderException
     *             whenever the provider source is not found
     */
    public static void setLanguageProvider(final String providerId, final boolean value)
            throws EmptyProviderException {
        LOGGER.finest("Begin setLanguageProvider method");

        // Load active window
        final IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();

        // Load provider service
        final ISourceProviderService service = (ISourceProviderService) window
                .getService(ISourceProviderService.class);

        // Load provider
        final LanguageSourceProvider provider = (LanguageSourceProvider) service
                .getSourceProvider(providerId);

        // Set enabled depending on field value
        if (provider == null) {
            final EmptyProviderException exception = new EmptyProviderException(
                    "No source provider for " + providerId);
            LOGGER.log(Level.FINER, exception.getClass() + " : " + exception.getMessage(),
                    exception);
            throw exception;
        } else {
            provider.setEnabled(providerId, value);

        }

        LOGGER.finest("End setLanguageProvider method");
    }

    /**
     * Set the value of a provider considering a preference container.
     * 
     * @param field
     *            the preference container
     * @throws EmptyProviderException
     *             whenever the provider source is not found
     */
    public static void setLanguageProvider(final LangagePreferenceContainer field)
            throws EmptyProviderException {
        setLanguageProvider(field.getPrefId(), field.isChecked());
    }
}
