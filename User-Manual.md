## Table of contents
+ [1. Introduction](#1-introduction)
+ [2. i-Code CNES products](#2-i-code-cnes-products)
  + [2.1. i-Code App or i-Code CLI](#21-i-code-app-or-i-code-cli)
  + [2.2. i-Code IDE](#22-i-code-ide)
  + [2.3. i-Code plugin for Eclipse](#23-i-code-plugin-for-eclipse)
  + [2.4. i-Code plugin for SonarQube](#24-i-code-plugin-for-sonarqube)
  + [2.5. i-Code Core](#25-i-code-core)
  + [2.6. i-Code Library](#26-i-code-library)
+ [3. Rules description](#3-rules-description)
  + [3.1. Common rules](#31-common-rules)
  + [3.2. Specific rules](#32-specific-rules)
    + [3.2.1. Shell rules](#321-shell-rules)
    + [3.2.2. Fortran 77 rules](#322-fortran-77-rules)
    + [3.2.3. Fortran 90 rules](#323-fortran-90-rules)
+ [4. Metrics description](#4-metrics-description)
+ [5. Limitations](#5-limitations)
+ [6. References](#6-references)

## 1. Introduction

This document is the i-Code CNES user manual. It describes how to use i-Code CNES.

Before using i-Code CNES, you should:
+ Know eclipse. Eclipse documentation is available here [ [R1] ].
+ Read CNES coding rules [ [R2] ] [ [R3] ] [ [R4] ] [ [R5] ].
+ Installation instructions are available in the installation manual [ [R6] ].

## 2. i-Code CNES products
### 2.1. i-Code App or i-Code CLI
### 2.2. i-Code IDE
### 2.3. i-Code plugin for Eclipse
### 2.4. i-Code plugin for SonarQube
### 2.5. i-Code Core
### 2.6. i-Code Library

## 3. Rules description

### 3.1. Common rules

### 3.2. Specific rules

#### 3.2.1. Shell rules
#### 3.2.2. Fortran 77 rules
#### 3.2.3. Fortran 90 rules

## 4. Metrics description



## 5. Limitations

|                  LIMITATION                  | ALTERNATIVE |     LINKED ISSUE    |
|----------------------------------------------|-------------|---------------------|
| Powershell does not allow to analysis launch | Use cmd.exe | [#73](../issues/73) |
| Shell: strings without quotes are not well supported | Use quotes for strings | [#31](../issues/31) |
| Files without identified extension are not analyzed | Analysis on project are based on file extension to attribute the good file to the good analyzer. Which mean, it's not required to filter file's extension when launching an analysis on a project, however it's important (especially in shell) that files that should be analyzed have an appropriate extension. <br/> Extension supported by languages: <br/> - Shell: sh, ksh, bash <br/> - Fortran: f, f77, f90, F, F90, F77, fortran <br/> **A Shell file for instance `no_extension` should be renamed `no_extension.sh` in order to be analyzed.**| [#00](../issues/00) |
|                                              |             |                     |

## 6. References
[R1]: #6-references
[R2]: #6-references
[R3]: #6-references
[R4]: #6-references
[R5]: #6-references
[R6]: #6-references

+ [ [R1] ] [Eclipse Documentation](http://www.eclipse.org/documentation/)
+ [ [R2] ] RNC-CNES-Q-HB-80-505 - Coding rules Fortran 77 Version 7
+ [ [R3] ] RNC-CNES-Q-HB-80-517 - Coding rules Fortran 90 Version 5
+ [ [R4] ] RNC-CNES-Q-HB-80-501 - Common coding rules Version 5
+ [ [R5] ] RNC-CNES-Q-HB-80-516 - Coding rules SHELL Version 6
+ [ [R6] ] [i-Code Installation Manual](installation-manual)