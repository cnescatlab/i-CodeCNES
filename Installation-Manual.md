## Table of contents
+ [1. Introduction](#1-introduction)
+ [2. Installation guidelines](#2-installation-guidelines)
  + [2.1. Download i-Code](#21-download-i-code)
  + [2.2. Install i-Code CLI](#22-install-i-code-cli)
  + [2.3. Install i-Code IDE](#23-intall-i-code-ide)
  + [2.4. Install i-Code plugin for Eclipse](#24-install-i-code-plugin-for-eclipse)
  + [2.5. Install i-Code plugin for SonarQube](#25-install-i-code-plugin-for-sonarqube)
+ [3. References](#3-references)

## 1. Introduction

This document explains how to install i-Code CNES. Documentation on how to use i-Code is in the user manual [ [R1] ].

This tool provides four products:
+ A standalone version with GUI named **i-Code CNES IDE**
+ A command line version named **i-Code CLI** or **i-Code App**
+ An Eclipse plugin
+ A SonarQube plugin [ [R3] ]

Other products are available for developers:
+ **i-Code Core**: a library containing core features for developing new i-Code plugins, see [ [R4] ] for more information
+ **i-Code Library**: a library allowing to analyze code in your own application

## 2. Installation guidelines
### 2.1. Download i-Code

i-Code CNES products are available on the GitHub Release page of [i-CodeCNES](/lequal/i-CodeCNES) repository: [[https://github.com/lequal/i-CodeCNES/releases]].

For each release, the following products are available:
+ i-Code plugin for Eclipse
  + `fr.cnes.icode.repository-X.Y.Z-SNAPSHOT.zip`
+ i-Code CLI
  + `icode-X.Y.Z.zip`
+ i-Code IDE
  + `icode-ide.product-linux.gtk.x86.zip`
  + `icode-ide.product-linux.gtk.x86_64.zip`
  + `icode-ide.product-macosx.cocoa.x86_64.zip`
  + `icode-ide.product-win32.win32.x86.zip`
  + `icode-ide.product-win32.win32.x86_64.zip`

### 2.2. Install i-Code CLI

Download the latest i-Code CNES CLI version on [GitHub Releases page](https://github.com/lequal/i-CodeCNES/releases). Then put the archive in your chosen location.

#### On linux
```sh
unzip icode-X.Y.Z.zip
alias icode=$(pwd)/icode/icode
``` 

#### On Windows (PowerShell)
Extract the archive, then: 
```ps
Set-Alias icode c:\path\to\icode\icode.bat
```

### 2.3. Install i-Code IDE
### 2.4. Install i-Code plugin for Eclipse

1. Download the eclipse plugin archive of the [last release](https://github.com/lequal/i-CodeCNES/releases). 

1. Then, launch Eclipse and select the **Help > Install new Software**. 

1. On the wizard, select **Add** and **Archive...** and select i-Code CNES archive.

1. Restart Eclipse.

### 2.5. Install i-Code plugin for SonarQube

All information about i-Code plugin for SonarQube is available on the official [project's repository](/lequal/sonar-icode-cnes-plugin).

There are two ways for installing the i-Code plugin:

+ Through the official SonarQube Marketplace
+ Manually by downloading the plugin on [its GitHub Release page](/lequal/sonar-icode-cnes-plugin/releases) and then following the installation guide in [ [R5] ]

In any case, follow instructions of official SonarQube documentation for properly install this plugin [ [R5] ].

## 3. References
[R1]: #3-references
[R2]: #3-references
[R3]: #3-references
[R4]: #3-references
[R5]: #3-references

+ [ [R1] ] [i-Code User Manual](http://www.eclipse.org/documentation/)
+ [ [R2] ] [Eclipse Documentation](http://www.eclipse.org/documentation/)
+ [ [R3] ] [SonarQube plugin for i-Code](https://github.com/lequal/sonar-icode-cnes-plugin)
+ [ [R4] ] [i-Code Developer Guide](https://github.com/lequal/icode-custom-plugin-example/wiki)
+ [ [R5] ] [Install a SonarQube plugin](https://docs.sonarqube.org/latest/setup/install-plugin/)