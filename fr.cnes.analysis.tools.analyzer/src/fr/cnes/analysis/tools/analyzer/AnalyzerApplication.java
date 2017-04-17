package fr.cnes.analysis.tools.analyzer;

import java.util.Optional;

import org.eclipse.equinox.app.IApplication;
import org.eclipse.equinox.app.IApplicationContext;

public class AnalyzerApplication implements IApplication {

	private String[] args;

	
	
	@Override
	public Object start(IApplicationContext context) throws Exception {
	
		args = (String[]) context.getArguments().get(IApplicationContext.APPLICATION_ARGS);

		System.out.println("Running Analyzer application");
		for (String s : args)
		{
			System.out.println("\nReceived arg : " + s);
		}
		
		
		return null;
	}

	@Override
	public void stop() {
		// TODO Auto-generated method stub

	}
	
	
	
	/**
	 * Finds an argument's value in the app's command line arguments, branding,
	 * and system properties
	 *
	 * @param argName
	 *            the argument name
	 * @param appContext
	 *            the application context
	 * @param singledCmdArgValue
	 *            whether it's a single-valued argument
	 * @return an {@link Optional} containing the value or an empty
	 *         {@link Optional}, if no value could be found
	 */
	private Optional<String> getArgValue(String argName, IApplicationContext appContext, boolean singledCmdArgValue) {
		// Is it in the arg list ?
		if (argName == null || argName.length() == 0)
			return Optional.empty();

		if (singledCmdArgValue) {
			for (int i = 0; i < args.length; i++) {
				if (("-" + argName).equals(args[i]))
					return Optional.of("true");
			}
		} else {
			for (int i = 0; i < args.length; i++) {
				if (("-" + argName).equals(args[i]) && i + 1 < args.length)
					return Optional.of(args[i + 1]);
			}
		}

		final String brandingProperty = appContext.getBrandingProperty(argName);

		return Optional.ofNullable(brandingProperty).map(brandingPropertyValue -> Optional.of(brandingPropertyValue))
				.orElse(Optional.ofNullable(System.getProperty(argName)));
	}


}
