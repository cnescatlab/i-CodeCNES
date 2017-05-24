package fr.cnes.analysis.tools.analyzer;

import fr.cnes.analysis.tools.analyzer.datas.AbstractMetric;
import fr.cnes.analysis.tools.analyzer.datas.FileValue;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

public class CallableMetricAnalyzer implements Callable<List<FileValue>> {

  private AbstractMetric metric;
  private File           file;

  public CallableMetricAnalyzer(AbstractMetric pMetric, File pFile) {
    this.metric = pMetric;
    this.file = pFile;
  }

  @Override
  public List<FileValue> call() throws Exception {
    List<FileValue> results = new ArrayList<>();
    metric.setInputFile(file);
    results.add(metric.run());
    return results;
  }
}
