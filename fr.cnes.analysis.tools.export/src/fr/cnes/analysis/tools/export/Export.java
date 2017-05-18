package fr.cnes.analysis.tools.export;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;

import fr.cnes.analysis.tools.analyzer.datas.FileValue;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.export.exception.ContributorReturnNullResultFromImportException;
import fr.cnes.analysis.tools.export.exception.NoExportClassFoundInContributions;
import fr.cnes.analysis.tools.export.exception.NoImportClassFoundInContributions;
import fr.cnes.analysis.tools.export.exception.NoIndicatedFormatInFilePathException;
 
public class Export {
 
	static final String Export_ExtensionPoint_ID = "fr.cnes.analysis.tools.export";
	static final String Export_ExtensionPoint_Attribute_formatName = "formatName";
	static final String Export_ExtensionPoint_Attribute_formatExtension = "formatExtension";
	static final String Export_ExtensionPoint_Attribute_exportClass = "ExportClass";
	static final String Export_ExtensionPoint_Attribute_importClass = "ImportClass";

	
	
	/**
	 * @return a Map with formatName as key and and formatExtension as value of every format handled by the contributor of the ExtensionPoint {@value #Export_ExtensionPoint_ID}
	 */
	public Map<String, String> getAvailableFormats(){
		Map<String,String> formats = new TreeMap<>();
		for(IConfigurationElement contribution : this.getContributions()){
			formats.put(contribution.getAttribute(Export_ExtensionPoint_Attribute_formatName), contribution.getAttribute(Export_ExtensionPoint_Attribute_formatExtension));
		}
		return formats;
	}
	
	
	
	
	
	/**
	 * @return all configuration elements contributing to ExtensionPoint {@value #Export_ExtensionPoint_ID}
	 */
	private IConfigurationElement[] getContributions(){
		return Platform.getExtensionRegistry().getConfigurationElementsFor(Export_ExtensionPoint_ID);
	}
	
	
	
	public void exportViolation(List<Violation> violations, File file) throws NoExportClassFoundInContributions, NoIndicatedFormatInFilePathException, IOException {
			IExport exporter = this.getExportClass(this.getExtensionFromFilePath(file.getAbsolutePath()));
			exporter.exportViolation(violations, file);
	}


	public List<Violation> importViolations(File file) throws NoImportClassFoundInContributions, NoIndicatedFormatInFilePathException, ContributorReturnNullResultFromImportException {
		List<Violation> violations = null;
		IImport importer = (IImport) this.getImportClass(this.getExtensionFromFilePath(file.getAbsolutePath()));
		violations = importer.ImportViolations(file);
		if(violations == null){
			throw new ContributorReturnNullResultFromImportException();
		}
		return violations;
	}
	
	/**
	 * @param filePath
	 * @return
	 * @throws NoIndicatedFormatInFilePathException
	 */
	private String getExtensionFromFilePath(final String filePath) throws NoIndicatedFormatInFilePathException{
		String extension = "";

		int index = filePath.lastIndexOf('.');
		int parents = Math.max(filePath.lastIndexOf('/'), filePath.lastIndexOf('\\'));

		if (index > parents) {
		    extension = filePath.substring(index+1);
		}else{
			throw new NoIndicatedFormatInFilePathException();
		}
		return extension;
	}
	
	/**
	 * @param formatExtension The extension of the file (without ".")
	 * @return The IImport class found in contribution to import the extension of this format.
	 * @throws NoImportClassFoundInContributions when after browsing contributor no class importing this format was found.
	 */
	private IImport getImportClass(String formatExtension) throws NoImportClassFoundInContributions{
		IImport importClass = null;
		for(IConfigurationElement contribution : this.getContributions()){
			if(contribution.getAttribute(Export_ExtensionPoint_Attribute_formatExtension).equals(formatExtension)){
				try {
					importClass = (IImport) contribution.createExecutableExtension(Export_ExtensionPoint_Attribute_importClass);
				} catch (CoreException e) {
					e.printStackTrace();
				}
			}
		}
		if(importClass == null){
			throw new NoImportClassFoundInContributions();
		}
		return importClass;
	}
	
	/**
	 * @param formatName
	 * @param formatExtension
	 * @return
	 * @throws NoExportClassFoundInContributions
	 */
	private IExport getExportClass(String formatExtension) throws NoExportClassFoundInContributions{
		/*
		 * The export class to return from the contributors of the Extension Point.
		 */
		IExport exportClass = null;
		for(IConfigurationElement contribution : this.getContributions()){
			if(contribution.getAttribute(Export_ExtensionPoint_Attribute_formatExtension).equals(formatExtension)){
				try {
					Object o = contribution.createExecutableExtension(Export_ExtensionPoint_Attribute_exportClass);
					if(o instanceof IExport)
						exportClass = (IExport) o;
					//exportClass = (IExport) contribution.createExecutableExtension(Export_ExtensionPoint_Attribute_exportClass);
				} catch (CoreException e) {
					e.printStackTrace();
				}
			}
		}
		
		if(exportClass == null){
			throw new NoExportClassFoundInContributions();
		}
		return exportClass;
	}

	
}
