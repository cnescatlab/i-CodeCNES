package fr.cnes.analysis.tools.export;

import java.io.File;
import java.io.IOException;
import java.util.List;

import fr.cnes.analysis.tools.analyzer.datas.Violation;

public interface IExport {

	public void exportViolation(List<Violation> violations, File file) throws IOException;

}
