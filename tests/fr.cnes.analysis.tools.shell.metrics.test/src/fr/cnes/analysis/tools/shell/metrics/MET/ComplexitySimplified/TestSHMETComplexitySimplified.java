/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                               */
/************************************************************************************************/

package fr.cnes.analysis.tools.shell.metrics.MET.ComplexitySimplified;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.eclipse.core.runtime.FileLocator;
import org.junit.Test;

import fr.cnes.analysis.tools.analyzer.datas.AbstractChecker;
import fr.cnes.analysis.tools.analyzer.datas.CheckResult;
import fr.cnes.analysis.tools.analyzer.exception.JFlexException;
import fr.cnes.analysis.tools.shell.metrics.SHMETComplexitySimplified;
import fr.cnes.analysis.tools.shell.metrics.TestUtils;

/**
 * This class aims to test Don.Declaration rule. There are 2 functions in this
 * class. The first one verifies that an error in a file is detected whenever
 * there is one, the other verifies that nothing is detected when there's no
 * error.
 * 
 */
public class TestSHMETComplexitySimplified {

    /**
     * This test verifies that an error can be detected.
     */
    @Test
    public void testRunWithError() {

        try {
            // Initializing rule and getting error file.
            final AbstractChecker metric = new SHMETComplexitySimplified();
            final String fileName = "nvm.sh";
            final File file = new File(
                    FileLocator.resolve(this.getClass().getResource(fileName)).getFile());

            // Defining file in the rule instantiation.
            metric.setContribution(TestUtils.getContribution("", ""));
            metric.setInputFile(file);

            // We verify that the metric value detected is the right one.
            // Get the list and verify each value
            final List<CheckResult> checkResults = metric.run();
            CheckResult fileValue = null;
            for (CheckResult check : checkResults) {
                if (check.getLocation()==null || check.getLocation().isEmpty()) {
                    fileValue = check;
                    checkResults.remove(checkResults.indexOf(check));
                }
            }
            if (fileValue == null) {
                fail("Erreur : Aucun résultat sur le fichier trouvé.");
            } else {
                assertTrue(fileValue.getFile().getName().equals(fileName));
                assertTrue(fileValue.getValue().equals((float) 131.0));

                final List<CheckResult> functionValues = checkResults;
                Map<String, Float> exceptedValues = new TreeMap<>();
                exceptedValues.put("MAIN PROGRAM",(float)5.0);
                exceptedValues.put("nvm_alias",(float)3.0);
                exceptedValues.put("nvm_alias_path",(float)1.0);
                exceptedValues.put("nvm_cd",(float)1.0);
                exceptedValues.put("nvm_clang_version",(float)1.0);
                exceptedValues.put("nvm_command_info",(float)6.0);
                exceptedValues.put("nvm_curl_libz_support",(float)1.0);
                exceptedValues.put("nvm_curl_use_compression",(float)1.0);
                exceptedValues.put("nvm_curl_version",(float)1.0);
                exceptedValues.put("nvm_download",(float)4.0);
                exceptedValues.put("nvm_ensure_version_installed",(float)6.0);
                exceptedValues.put("nvm_err",(float)1.0);
                exceptedValues.put("nvm_find_nvmrc",(float)2.0);
                exceptedValues.put("nvm_find_up",(float)3.0);
                exceptedValues.put("nvm_get_latest",(float)6.0);
                exceptedValues.put("nvm_grep",(float)1.0);
                exceptedValues.put("nvm_has",(float)1.0);
                exceptedValues.put("nvm_has_colors",(float)2.0);
                exceptedValues.put("nvm_has_non_aliased",(float)1.0);
                exceptedValues.put("nvm_has_system_iojs",(float)1.0);
                exceptedValues.put("nvm_has_system_node",(float)1.0);
                exceptedValues.put("nvm_install_latest_npm",(float)16.0);
                exceptedValues.put("nvm_is_alias",(float)1.0);
                exceptedValues.put("nvm_is_valid_version",(float)4.0);
                exceptedValues.put("nvm_is_version_installed",(float)1.0);
                exceptedValues.put("nvm_ls_current",(float)7.0);
                exceptedValues.put("nvm_make_alias",(float)3.0);
                exceptedValues.put("nvm_prepend_path",(float)3.0);
                exceptedValues.put("nvm_print_npm_version",(float)2.0);
                exceptedValues.put("nvm_rc_version",(float)3.0);
                exceptedValues.put("nvm_remote_version",(float)10.0);
                exceptedValues.put("nvm_resolve_alias",(float)11.0);
                exceptedValues.put("nvm_tree_contains_path",(float)4.0);
                exceptedValues.put("nvm_version",(float)5.0);
                exceptedValues.put("nvm_version_dir",(float)5.0);
                exceptedValues.put("nvm_version_greater",(float)1.0);
                exceptedValues.put("nvm_version_greater_than_or_equal_to",(float)1.0);
                exceptedValues.put("nvm_version_path",(float)4.0);
                for(CheckResult metricValue : functionValues){
                	assertTrue("Test do not excepts function : "+metricValue.getLocation()+".",exceptedValues.containsKey(metricValue.getLocation()));
                	assertTrue("Test excepts value of ["+Math.round(exceptedValues.get(metricValue.getLocation()))+"] while metric computed ["+Math.round(metricValue.getValue())+"] for the function "+metricValue.getLocation()+".",Math.round(metricValue.getValue()) == Math.round(exceptedValues.get(metricValue.getLocation())));
                }
                assertTrue("Test excepts "+exceptedValues.size()+" functions computed for the file while the metric computed ["+functionValues.size()+"].",functionValues.size() == exceptedValues.size());

            }
        } catch (final FileNotFoundException e) {
            fail("Erreur d'analyse (FileNotFoundException)");
        } catch (final IOException e) {
            fail("Erreur d'analyse (IOException)");
        } catch (final JFlexException e) {
            fail("Erreur d'analyse (JFlexException)");
        }
    }
}
