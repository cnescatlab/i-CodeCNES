package fr.cnes.analysis.tools.export.csv;

/************************************************************************************************/
/* i-Code CNES is a static code analyzer. */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html */
/************************************************************************************************/

import fr.cnes.analysis.tools.analyzer.datas.Violation;
import fr.cnes.analysis.tools.export.IExport;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

/**
 * This class is an attribute of the {@code ExtensionPoint} implementing {@link IExport} interface
 * of the plugin <i>fr.cnes.analysis.tools.export</i>.
 * <p>
 * This class is also exported in the <i>fr.cnes.analysis.tools.export.csv</i> plugin and could be
 * used as a service from any third.
 * </p>
 * <p>
 * This class is responsible of the export in the format CSV of {@link Violation} elements into a
 * {@link File}.
 * </p>
 * 
 * @since 3.0
 */
public class ExportCsv implements IExport {

  /**
   * Default constructor. Required to execute a class from the contributed extension point.
   */
  public ExportCsv() {
  }

  /*
   * (non-Javadoc)
   * 
   * @see fr.cnes.analysis.tools.export.IExport#export(java.util.List, java.io.File)
   */
  @Override
  public void export(List<Violation> violations, File outputFile) throws IOException {
    final FileWriter writer = new FileWriter(outputFile);
    writer.write("Rule, File, Location, Value, Criticity\n");
    for (final Violation violation : violations) {
      writer.write(violation.getRuleName() + "," + violation.getFile().getAbsolutePath() + ","
          + violation.getLocation() + "," + violation.getLine().toString() + "\n");
    }

    writer.close();

  }

}
