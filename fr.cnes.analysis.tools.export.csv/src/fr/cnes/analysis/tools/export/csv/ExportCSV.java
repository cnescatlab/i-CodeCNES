package fr.cnes.analysis.tools.export.csv;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.export.IExport;

public class ExportCSV implements IExport {

	public ExportCSV() {
	}

	@Override
	public void exportViolation(List<Violation> violations, File file) throws IOException {
		final FileWriter out = new FileWriter(file);
        out.write("Rule, File, Location, Value, Criticity\n");
        for (final Violation violation : violations) {
            out.write(violation.getRuleName() + "," + violation.getFile().getAbsolutePath()
                    + "," + violation.getLocation() + "," + violation.getLine().toString() + "\n");
        }

        out.close();
		
	}


}
