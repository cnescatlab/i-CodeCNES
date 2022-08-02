![i-Code logo](https://github.com/cnescatlab/i-CodeCNES/blob/master/img/logo-i-code-cnes.png)

[![Java CI](https://github.com/cnescatlab/sonar-cnes-report/actions/workflows/java-continuous-integration.yml/badge.svg)](https://github.com/cnescatlab/i-CodeCNES/actions/workflows/java-continuous-integration.yml)
[![SonarQube Quality Gate](https://sonarcloud.io/api/project_badges/measure?project=lequal_i-CodeCNES&metric=alert_status)](https://sonarcloud.io/dashboard?id=lequal_i-CodeCNES)
[![SonarQube Bugs](https://sonarcloud.io/api/project_badges/measure?project=lequal_i-CodeCNES&metric=bugs)](https://sonarcloud.io/project/issues?id=lequal_i-CodeCNES&resolved=false&types=BUG)
[![SonarQube Coverage](https://sonarcloud.io/api/project_badges/measure?project=lequal_i-CodeCNES&metric=coverage)](https://sonarcloud.io/component_measures?id=lequal_i-CodeCNES&metric=Coverage)
[![SonarQube Technical Debt](https://sonarcloud.io/api/project_badges/measure?project=lequal_i-CodeCNES&metric=sqale_index)](https://sonarcloud.io/component_measures?id=lequal_i-CodeCNES&metric=Maintainability)

i-Code CNES is a static code analysis tool to help developers write code compliant with CNES coding rules for Fortran 77, Fortran 90 and Shell.

All the information on CNES standards coverage, and rules availabilities and limitations can be read in the [documentation](https://github.com/cnescatlab/i-CodeCNES/wiki). 

## Quick start
- Download latest i-Code version on [GitHub Releases](https://github.com/cnescatlab/i-CodeCNES/releases).
- Unzip i-Code archive where you need it.
- Add `icode` to your path.
- Grant `icode` execution permission.
- Run `icode path/to/project/directory`.

## i-Code products

##### i-Code Core
This is the core library containing all i-Code utilities for code analysis.
##### i-Code Library
This is the full library containing all official checkers. It includes i-Code Core.
##### i-Code App or i-Code CLI
This is the common command line application for i-Code.
##### i-Code IDE
This is the common GUI application for i-Code.
##### i-Code plugin for Eclipse
The Eclipse plugin for i-Code allows to use i-Code from Eclipse IDE.
##### i-Code plugin for SonarQube
The SonarQube plugin for i-Code allows to use i-Code through SonarQube analysis. Please refer to [sonar-icode-cnes-plugin](https://github.com/cnescatlab/sonar-icode-cnes-plugin) for more details.

## Installation
##### i-Code CLI
Just unzip the corresponding archive.

##### i-Code IDE
Just unzip the corresponding archive.

##### i-Code plugin for Eclipse
Refer to Eclipse documentation to know how to install a standard Eclipse plugin.

##### i-Code plugin for SonarQube
Refer to SonarQube documentation to know how to install a standard SonarQube plugin.

## Get help
Use `icode -h` to get the following help about *i-Code*:
```
usage: icode [<FILE> [...]] [-c <arg>] [-e] [-f <arg>] [-h] [-l] [-o <arg>] [-p <arg>] [-q <arg>] [-r] [-v] [-x <arg>]
Analyze Shell, F77 & F90 code to find defects & bugs.

 -c,--checked-languages <arg>        Comma separated list of languages checked during analysis. All by default.
 -e,--exporters                      Display all available exporters.
 -f,--export-format <arg>            Set the format for result file. Default format is XML.
 -h,--help                           Display this message.
 -l,--languages                      Display all available languages.
 -o,--output <arg>                   Set the name for result file. Results are displayed in standard output by default.
 -p,--export-parameters <arg>        Comma separated list of parameters for the export. Format is:
                                     key1=value1,key2=value2,key3=value3. Default values depend on the chosen export plugin.
 -q,--list-export-parameters <arg>   Display all available parameters for the given export.
 -r,--rules                          Display all available rules.
 -v,--version                        Display version information.
 -x,--excluded-rules <arg>           Comma separated list of rules id to exclude from analysis. None by default.


Please report issues at https://github.com/cnescatlab/i-CodeCNES/issues
```

## Build
You can easily rebuild all i-Code products with Maven:
```bash
git clone https://github.com/cnescatlab/i-CodeCNES icode
cd ./icode/
mvn clean install
```

## Extending i-Code with your own plugin
If you need to add some new feature, the easiest way is to implment your own plugin by forking [icode-custom-plugin-example](https://github.com/cnescatlab/icode-custom-plugin-example) and its dedicated [Developer Guide](https://github.com/cnescatlab/icode-custom-plugin-example/wiki/Developer-guide).

## Changelog

#### Release 4.1.2
###### Fixed bugs
- [x] **BUG #226** > Continuous integration on Travis doesn't work anymore

#### Release 4.1.1
###### Fixed bugs
- [x] **BUG #221** > Too many open files
- [x] **BUG #224** > Remove LEQUAL from this repository

#### Release 4.1.0

###### New features
- [x] **FEATURE #198** > i-Code should not crash on checker error
- [x] **FEATURE #201** > Simplify logged information

###### Fixed bugs
- [x] **BUG #131** > Correction erreur COM.xxx
- [x] **BUG #147** > Test files may have a "contains" instead of an "equals" to check the location value
- [x] **BUG #197** > IndexOutOfBoundsException while analyzing an empty Shell script
- [x] **BUG #200** > JAXB is still used but missing in icode-library

#### Release 4.0.0

###### New features
- Complete refactoring of i-Code architecture
- Deletion of RCP in command line
- Add version argument in command line
- Run Jflex through maven #165
- Jflex version update #165
- Transform eclipse plugin into Java plugin #165
- Command line support directory as argument: files inclusion wil be recursively included #161 #157
- Deletion of parallelized checkers running #161
- Refactor test as parametrized tests #165
- Change exe to bat and bash scripts #165
- Allow to load plugins which are dropped in plugins directory #165
- Update packaging of i-Code #165
- Update CI #145
- Reintegrate RCP as a submodule using i-Code Core #165
- This 4.0.0 version integrate a whole new architecture described in https://github.com/cnescatlab/i-CodeCNES/wiki
  - `icode-core`: contains core feature to build analyzer
  - `icode-library`: contains the minimal classes to run i-Code analyzers in a Java application
  - `icode-app`: a standalone command line version of i-Code analyzer
  - `icode-ide`: contains the i-Code IDE version and Eclipse plugin
  - `*-language`, `*-rules` and `*-metrics`: contain analyzer for several languages: Shell, Fortran, ...
- The new documentation is available as a wiki https://github.com/cnescatlab/i-CodeCNES/wiki
- A `Developer Guide` is now available here: https://github.com/cnescatlab/icode-custom-plugin-example/wiki/Developer-guide
- Users are able to add custom plugins by putting their `jar` files into `icode/plugins/` directory
- Bug about recursive analysis is fixed and users can now simply analyze a directory, e.g.: `icode .`
- The continuous integration was enhanced with GitHub Actions (https://github.com/cnescatlab/i-CodeCNES/actions) and SonarCloud(https://sonarcloud.io/dashboard?id=lequal_i-CodeCNES)
- The contributing page and issue templates were updated
- Eclipse RCP was removed from core features of i-Code
- Some other minor enhancements and fixes
- Fix #157: Recursive search of files for analysis command line enhancement
- Fix #145: Connect i-Code build to SonarCloud enhancement
- Fix #142: Add i-Code version to the xml results file enhancement
- Fix #166: Bad support of heredoc notations bug shell
- Fix #165: Refactor i-Code architecture enhancement
- Fix #161: Files handling and recursive analysis issue command line fortran to analyse
- Fix #170: Combined standalone subroutine + module in same file is crashing scanner in FORTRAN file fortran
- Fix #159: SHDESIGNOptions false positive false positive shell
- Fix #168: icode commandline gives incorrect and cryptic error when encountering '))' bug shell
- Fix #158: Inconsistency between "RNC shell SH.FLOW.CheckUser example" and "I-code COMDESIGNActiveWait LEX" false positive shell
- Fix #186: Links in README point to sonar-icode bug documentation
- Fix #187: Consolidate community documentation documentation

#### Release 3.1.0

###### New features
* New command line #133 
* New parsing error handling, a violation named "Parser error" is added instead of suspend the analysis. #154
* New rules (Shell)
  * COM.DATA.Initialisation ( fix #113 )
  * COM.DATA.Invariant ( fix #114 )
  * COM.FLOW.FilePath ( fix #115 )
  * COM.FLOW.Recursion ( fix #116 )
  * COM.INST.BoolNegation ( fix #117 )
  * COM.NAME.Homonymy ( fix #118 )
  * COM.PRES.Indent ( fix #119 )
  * COM.PRES.LengthLine ( fix #120 )
  * SH.FLOW.CheckCodeReturn ( fix #121 )
  * SH.Ref.Export ( fix #122 #52 #138 #137 )
  * SH.SYNC.Signals #123 
* New metrics
  * SH.MET.LineOfComment
  * F77.MET.LineOfComment
  * F90.MET.LineOfComment

###### Fixes 
* Shell 
  * All checkers :
    * Function correction on FUNCSTART and FNAME #138 #137 #150
  * COM.FLOW.CaseSwitch : 
    * Case handling fixed #135
    * Function localization fixed #52
  * COM.DATA.LoopCondition
    * Function localization fixed #52
  * COM.DESIGN.ActiveWait
    * Function localization fixed #52
  * COM.FLOW.Abort 
    * Function localization fixed #52
  
#### Release 3.0.1

* Fix of Eclipse's plug-in performances #101

#### Release 3.0.0

###### New features
* Command line for Windows, MacOS & Linux #64 
* Standalone version i-Code CNES IDE #1 
* New Extension Points  
  * To add languages #32   
  * To add checkers #23   
  * To add configurations   
  * To add exports #19 #26 
* API  
  * To run analysis #16   
  * To export analysis #19  #26   
  * To reach configurations & preferences 
* Shells metrics (*SH.MET.LineOfCode*, *SH.MET.RatioComment*, *SH.MET.Nesting*, *SH.MET.ComplexitySimplified*) #30 
* Automated build #1

###### Bug fixes & enhancements
* Analysis performances improvements  #14 
* User Interface preference page improvements  #36 
* Improvements of analysis failure notifications #50 
* XML and CSV export improvements #69  #19 

*Minor fixes and other enhancements : [milestone 3.0.0](https://github.com/cnescatlab/i-CodeCNES/milestone/1).*

### Previous Releases
* [Release 3.1.0](https://github.com/cnescatlab/i-CodeCNES/releases/tag/v3.1.0)
* [Release 3.0.1](https://github.com/cnescatlab/i-CodeCNES/releases/tag/v3.0.1)
* [Release 3.0.0](https://github.com/cnescatlab/i-CodeCNES/releases/tag/v3.0.0)
* [Release 2.0.0](https://github.com/cnescatlab/i-CodeCNES/releases/tag/v2.0.0)
* [Release 1.0.0](https://github.com/cnescatlab/i-CodeCNES/releases/tag/v1.0.0)


### Feedback and Support
Contact : catlab@cnes.fr

Bugs and feature requests: https://github.com/cnescatlab/i-CodeCNES/issues 

### How to contribute
If you experienced a problem with the plugin please open an issue. Inside this issue please explain us how to reproduce this issue and paste the log.

If you want to do a PR, please put inside of it the reason of this pull request. If this pull request fix an issue please insert the number of the issue or explain inside of the PR how to reproduce this issue.

### License
Copyright 2022 CATLab.

This software is licensed under the terms in the file named "LICENSE" in this directory.

The software used Java files, generated with JFlex (http://.jflex.de). The terms of this library license are available here after : http://jflex.de/copying.html
