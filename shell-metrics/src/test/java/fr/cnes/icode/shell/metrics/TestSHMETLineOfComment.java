/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/

package fr.cnes.icode.shell.metrics;

import fr.cnes.icode.datas.AbstractChecker;
import fr.cnes.icode.datas.CheckResult;
import fr.cnes.icode.exception.JFlexException;
import org.junit.Test;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

/**
 * This class aims to test Don.Declaration rule. There are 2 functions in this
 * class. The first one verifies that an error in a file is detected whenever
 * there is one, the other verifies that nothing is detected when there's no
 * error.
 * 
 */
public class TestSHMETLineOfComment {

    /**
     * This test verifies that an error can be detected.
     */
    @Test
    public void testRunWithError() {

        try {
            // Initializing rule and getting error file.
            final AbstractChecker metric = new SHMETLineOfComment();
            final String fileName = "nvm.sh";
            final File file = new File(this.getClass().getResource(String.format("/%s", fileName)).getFile());

            // Defining file in the rule instantiation.
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
                fail("Error: No issue found in the file.");
            } else {

                assertTrue(fileValue.getFile().getName().equals(fileName));
                Float exceptedFileValue = (float)246;
                assertTrue("Test except a file value of ["+Math.round(exceptedFileValue) +"] while metric computed ["+Math.round(fileValue.getValue())+"].",Math.round(fileValue.getValue()) == Math.round(exceptedFileValue));
                

                final List<CheckResult> functionValues = checkResults;
                Map<String, Float> exceptedValues = new TreeMap<>();
                exceptedValues.put("MAIN PROGRAM",(float)246);
                exceptedValues.put("nvm_add_iojs_prefix",(float)4);
                exceptedValues.put("nvm_alias",(float)4);
                exceptedValues.put("nvm_alias_path",(float)4);
                exceptedValues.put("nvm_binary_available",(float)4);
                exceptedValues.put("nvm_cd",(float)5);
                exceptedValues.put("nvm_clang_version",(float)4);
                exceptedValues.put("nvm_command_info",(float)4);
                exceptedValues.put("nvm_curl_libz_support",(float)4);
                exceptedValues.put("nvm_curl_use_compression",(float)4);
                exceptedValues.put("nvm_curl_version",(float)4);
                exceptedValues.put("nvm_download",(float)6);
                exceptedValues.put("nvm_ensure_version_installed",(float)4);
                exceptedValues.put("nvm_ensure_version_prefix",(float)3);
                exceptedValues.put("nvm_err",(float)4);
                exceptedValues.put("nvm_find_nvmrc",(float)4);
                exceptedValues.put("nvm_find_up",(float)5);
                exceptedValues.put("nvm_format_version",(float)2);
                exceptedValues.put("nvm_get_latest",(float)4);
                exceptedValues.put("nvm_grep",(float)4);
                exceptedValues.put("nvm_has",(float)4);
                exceptedValues.put("nvm_has_colors",(float)4);
                exceptedValues.put("nvm_has_non_aliased",(float)4);
                exceptedValues.put("nvm_has_system_iojs",(float)4);
                exceptedValues.put("nvm_has_system_node",(float)4);
                exceptedValues.put("nvm_install_latest_npm",(float)5);
                exceptedValues.put("nvm_iojs_prefix",(float)4);
                exceptedValues.put("nvm_is_alias",(float)6);
                exceptedValues.put("nvm_is_iojs_version",(float)3);
                exceptedValues.put("nvm_is_valid_version",(float)4);
                exceptedValues.put("nvm_is_version_installed",(float)3);
                exceptedValues.put("nvm_ls_current",(float)4);
                exceptedValues.put("nvm_ls_remote_index_tab",(float)5);
                exceptedValues.put("nvm_ls_remote_iojs",(float)4);
                exceptedValues.put("nvm_make_alias",(float)4);
                exceptedValues.put("nvm_node_prefix",(float)4);
                exceptedValues.put("nvm_normalize_version",(float)3);
                exceptedValues.put("nvm_num_version_groups",(float)4);
                exceptedValues.put("nvm_prepend_path",(float)3);
                exceptedValues.put("nvm_print_alias_path",(float)3);
                exceptedValues.put("nvm_print_formatted_alias",(float)3);
                exceptedValues.put("nvm_print_npm_version",(float)4);
                exceptedValues.put("nvm_rc_version",(float)5);
                exceptedValues.put("nvm_remote_version",(float)4);
                exceptedValues.put("nvm_remote_versions",(float)4);
                exceptedValues.put("nvm_resolve_alias",(float)4);
                exceptedValues.put("nvm_resolve_local_alias",(float)4);
                exceptedValues.put("nvm_strip_iojs_prefix",(float)4);
                exceptedValues.put("nvm_strip_path",(float)3);
                exceptedValues.put("nvm_tree_contains_path",(float)4);
                exceptedValues.put("nvm_version",(float)6);
                exceptedValues.put("nvm_version_dir",(float)4);
                exceptedValues.put("nvm_version_greater",(float)3);
                exceptedValues.put("nvm_version_greater_than_or_equal_to",(float)4);
                exceptedValues.put("nvm_version_path",(float)4);
                exceptedValues.put("vm_ls_remote",(float)4);
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
