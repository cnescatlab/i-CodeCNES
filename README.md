<img src="https://github.com/lequal/i-CodeCNES/blob/master/img/logo-i-code-cnes.png" width="300"/>

[![Build Status](https://travis-ci.org/lequal/i-CodeCNES.svg?branch=master)](https://travis-ci.org/lequal/i-CodeCNES)

i-Code CNES is a static code analysis tool to help developers write code compliant with CNES coding rules for Fortran 77, Fortran 90 and Shell.

All the informations on CNES standards coverage, and rules availabilities and limitations can be read in the [documentation](https://github.com/lequal/i-CodeCNES/tree/master/documentation). 

Release 3.1.0
=============
## New features
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
  * SH.Ref.Export ( fix #122 #52 #138 #137)
  * SH.SYNC.Signals #123 
* New metrics
  * SH.MET.LineOfComment
  * F77.MET.LineOfComment
  * F90.MET.LineOfComment

## Fixes 
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
  
Release 3.0.1
=============
* Fix of Eclipse's plug-in performances #101

Release 3.0.0
=============

## New features
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

## Bug fixes & enhancements
* Analysis performances improvements  #14 
* User Interface preference page improvements  #36 
* Improvements of analysis failure notifications #50 
* XML and CSV export improvements #69  #19 

*Minor fixes and other enhancements : [milestone 3.0.0](https://github.com/lequal/i-CodeCNES/milestone/1).*

Previous Releases
=================
* [Release 2.0.0](https://github.com/lequal/i-CodeCNES/releases/tag/v2.0.0)
* [Release 1.0.0](https://github.com/lequal/i-CodeCNES/releases/tag/v1.0.0)


Feedback and Support
====================
Contact : L-lequal@cnes.fr

Bugs and Feature requests: https://github.com/lequal/i-CodeCNES/issues 

Licensing
=========
This software is licensed under the terms in the file named "LICENSE" in this directory.

The software used Java files, generated with JFlex (http://.jflex.de). The terms of this library license are available here after : http://jflex.de/copying.html
