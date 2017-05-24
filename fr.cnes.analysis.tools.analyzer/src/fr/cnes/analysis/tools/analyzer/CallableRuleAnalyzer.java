package fr.cnes.analysis.tools.analyzer;

import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

public class CallableRuleAnalyzer implements Callable<List<Violation>> {

  private AbstractRule rule;
  private File         file;

  public CallableRuleAnalyzer(AbstractRule rule, File file) {
    this.rule = rule;
    this.file = file;
  }

  @Override
  public List<Violation> call() throws Exception {
    List<Violation> results = new ArrayList<>();
    rule.setInputFile(file);
    results.addAll(rule.run());
    return results;
  }

}
