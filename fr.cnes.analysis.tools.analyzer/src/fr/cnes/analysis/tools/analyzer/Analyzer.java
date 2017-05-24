package fr.cnes.analysis.tools.analyzer;

import fr.cnes.analysis.tools.analyzer.datas.AbstractMetric;
import fr.cnes.analysis.tools.analyzer.datas.AbstractRule;
import fr.cnes.analysis.tools.analyzer.datas.FileValue;
import fr.cnes.analysis.tools.analyzer.datas.Violation;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.PlatformUI;

/**
 * <h1>i-Code CNES analyzer service</h1>
 * <p>
 * This class can be used by any third using {@link File} and {@link String} to run an analysis.
 * <p>
 * <p>
 * Using the methods {@link #applyRules(List, List)} and {@link #computeMetrics(List, File)}, the
 * {@link File} list must contain all files to analyze and the {@link String} list all rules id of
 * different plugin contributing to the
 * </p>
 *
 */
public class Analyzer {

  public static String Analyzer_ExtensionPoint_ID = "fr.cnes.analysis.tools.analyzer";

  /**
   * This method run an analysis using a {@link List} of {@link File} and identifier of i-Code CNES
   * rules contributing to the plugin.
   * 
   * This analysis is being run
   * 
   * @param files
   * @param rulesId
   * @return
   * @throws FileNotFoundException
   */
  public List<Violation> applyRules(List<File> files, List<String> pLanguageIds,
      List<String> pExcludedCheckIds) throws FileNotFoundException, Exception {
    List<String> languageIds = pLanguageIds;
    if (languageIds == null) {
      languageIds = new ArrayList<String>();
    }
    List<String> excludedCheckIds = pExcludedCheckIds;
    if (pExcludedCheckIds == null) {
      excludedCheckIds = new ArrayList<String>();
    }
    List<Violation> analysisResultViolation = new ArrayList<>();
    /*
     * The number of threads could be defined by the number of files or the number of rule or both
     * of them. This is pending how we decide to run the analysis.
     * 
     * TODO : Chose one solution for the number of threads
     */
    ExecutorService service = Executors.newFixedThreadPool(25);
    List<Future<List<Violation>>> analyzers = new ArrayList<Future<List<Violation>>>();
    /*
     
     */
    for (IConfigurationElement analyzerContribution : Platform.getExtensionRegistry()
        .getConfigurationElementsFor(Analyzer.Analyzer_ExtensionPoint_ID)) {
      if (languageIds.contains(analyzerContribution.getAttribute("extensionId"))) {
        /*
         * The current extension is one of the analyzer contribution that will be run. We are now
         * configuring it.
         */
        // 1. Setting files that will be analyzed
        // 1.1. Finding allowed extension from the plugin analyzer

        ArrayList<String> allowedExtension = new ArrayList<>();
        for (IConfigurationElement fileExtension : analyzerContribution
            .getChildren("fileExtension")) {
          allowedExtension.add(fileExtension.getAttribute("name"));
        }
        // 1.2. Restricting analysis only on file that the plugin can handle.
        // Note : The restriction is for file without extension and file that will be already
        // analyzed. This is causing crash from the analysis.
        ArrayList<File> restrictedFiles = new ArrayList<>();
        for (File file : files) {
          if (allowedExtension.contains(this.getFileExtension(file.getAbsolutePath()))
              && !restrictedFiles.contains(file)) {
            restrictedFiles.add(file);
          }
        }
        // 2. Running all rules that are not excluded from the analysis.
        // 2.1 Retrieving all rules from the extension point.
        for (IConfigurationElement contribution : Platform.getExtensionRegistry()
            .getConfigurationElementsFor(analyzerContribution.getAttribute("extensionId"))) {
          // 2.2 If the rule is not excluded, run the analysis.
          if (PlatformUI.getPreferenceStore().contains(contribution.getAttribute("id"))
              && !excludedCheckIds.contains((contribution.getAttribute("id")))) {
            AbstractRule rule;
            /*
             * We are currently to load as much Rule as there is files because the lex files are
             * designed to be run only on one file.
             */
            for (File analyzedFile : restrictedFiles) {
              try {
                rule = (AbstractRule) contribution.createExecutableExtension("class");
                rule.setContribution(contribution);
                CallableRuleAnalyzer callableAnalysis = new CallableRuleAnalyzer(rule,
                    analyzedFile);
                analyzers.add(service.submit(callableAnalysis));
              } catch (CoreException e) {

                // TODO : Define how to warn here of the execution failure without throwing new
                // exception
                e.printStackTrace();
              }
            }
          }
        }
      }
    }

    for (Future<List<Violation>> analysis : analyzers) {
      try {
        analysisResultViolation.addAll(analysis.get());
      } catch (InterruptedException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
      } catch (ExecutionException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }

    }
    return analysisResultViolation;
  }

  /**
   * @param fileName
   * @return The extension name of the file
   */
  private String getFileExtension(String fileName) {
    String extension = null;

    int i = fileName.lastIndexOf('.');
    int p = Math.max(fileName.lastIndexOf('/'), fileName.lastIndexOf('\\'));

    if (i > p) {
      extension = fileName.substring(i + 1);
    }
    return extension;
  }

  public List<FileValue> computeMetrics(List<File> files, List<String> pLanguageIds,
      List<String> pExcludedCheckIds) {
    List<String> languageIds = pLanguageIds;
    if (languageIds == null) {
      languageIds = new ArrayList<String>();
    }
    List<String> excludedCheckIds = pExcludedCheckIds;
    if (pExcludedCheckIds == null) {
      excludedCheckIds = new ArrayList<String>();
    }
    List<FileValue> analysisResultFileValues = new ArrayList<>();
    /*
     * The number of threads could be defined by the number of files or the number of rule or both
     * of them. This is pending how we decide to run the analysis.
     * 
     * TODO : Chose one solution for the number of threads
     */
    ExecutorService service = Executors.newSingleThreadExecutor();
    List<Future<List<FileValue>>> analyzers = new ArrayList<Future<List<FileValue>>>();

    for (IConfigurationElement analyzerContribution : Platform.getExtensionRegistry()
        .getConfigurationElementsFor(Analyzer.Analyzer_ExtensionPoint_ID)) {
      if (languageIds.contains(analyzerContribution.getAttribute("extensionId"))) {
        ArrayList<String> allowedExtension = new ArrayList<>();
        for (IConfigurationElement fileExtension : analyzerContribution
            .getChildren("fileExtension")) {
          allowedExtension.add(fileExtension.getAttribute("name"));
        }
        // 1.2. Restricting analysis only on file that the plugin can handle.
        ArrayList<File> restrictedFiles = new ArrayList<>();
        for (File file : files) {
          if (allowedExtension.contains(this.getFileExtension(file.getAbsolutePath()))
              && !restrictedFiles.contains(file)) {
            restrictedFiles.add(file);
          }
        }
        for (IConfigurationElement contribution : Platform.getExtensionRegistry()
            .getConfigurationElementsFor(analyzerContribution.getAttribute("extensionId"))) {
          if (PlatformUI.getPreferenceStore().contains(contribution.getAttribute("id"))
              && !excludedCheckIds.contains(contribution.getAttribute("id"))) {
            AbstractMetric metric;
            for (File analysisFile : restrictedFiles) {
              try {
                metric = (AbstractMetric) contribution.createExecutableExtension("class");
                metric.setContribution(contribution);
                CallableMetricAnalyzer callableAnalysis = new CallableMetricAnalyzer(metric,
                    analysisFile);
                analyzers.add(service.submit(callableAnalysis));
              } catch (CoreException e) {

                // TODO : Define how to warn here of the execution failure without throwing new
                // exception
                e.printStackTrace();
              }
            }
          }
        }
      }
    }

    for (Future<List<FileValue>> analysis : analyzers) {
      try {
        analysisResultFileValues.addAll(analysis.get());
      } catch (InterruptedException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
      } catch (ExecutionException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
    return analysisResultFileValues;
  }

}
