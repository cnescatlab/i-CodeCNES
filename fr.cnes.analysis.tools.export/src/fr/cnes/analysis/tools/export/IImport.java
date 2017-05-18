package fr.cnes.analysis.tools.export;

import java.io.File;
import java.util.List;

import fr.cnes.analysis.tools.analyzer.datas.Violation;

public interface IImport {
	
	public List<Violation> ImportViolations(File file);

}
