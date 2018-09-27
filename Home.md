# i-Code CNES Wiki

i-Code CNES is a static analysis tools for Fortran and Shell, it's verifying code compliancy with CNES standards (RNC). 

# Documentation
* [User Manual](https://github.com/lequal/i-CodeCNES/blob/master/documentation/i-Code%20CNES%20-%20User%20Manual.pdf)
* [Installation Manual](https://github.com/lequal/i-CodeCNES/blob/master/documentation/i-Code%20CNES%20-%20Installation%20Manual%20-%20EN.pdf)
* [[Developper Guide]]

# Quick install 
## i-Code CNES Eclipse plugin 
Download the eclipse plugin archive of the last release [link](https://github.com/lequal/i-CodeCNES/releases). 

Then, launch Eclipse and select the **Help > Install new Software**. On the wizard, select **Add** and **Archive..** and select i-Code CNES archive.

Restart Eclipse.

## i-Code CNES IDE

Download and unzip the latest IDE version in a folder.

## i-Code CNES CLI
Download the latest i-Code CNES CLI version [link](https://github.com/lequal/i-CodeCNES/releases). Then put the archive in your choosen location.

**On linux**
```sh
unzip i-CodeCNES-3.1.0-CLI-linux.gtk.x86.zip
alias icode=$(pwd)/i-CodeCNES-3.1.0-CLI-linux/icode

``` 

**On Windows (powershell)**
Extract the archive, then : 
```ps
Set-Alias icode c:\user\tools\icode.exe
```

# Quick analysis
## i-Code CNES IDE and Eclipse plugin
### Preferences
Select `Window > Preferences > i-Code CNES`.

You can set the severity configuration of the analysis using the scrollbar from RNC A to D, and choose rules to enable or disable in the different tables. You can also edit Metrics threeshold.

### Launching an analysis
Select files to analysis, one or several, and then click on `i-Code CNES > Run analysis`.
## i-Code CNES CLI

In your project folder, use the following command to analyze all files : 
```sh
# Analyse whole project
icode '**/*'

# Analyse whole project and make an xml output of the analysis
icode '**/*' -f xml -o output.xml

# List all functionality and description
icode --help
```


