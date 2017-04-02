#!/bin/sh
############################################################################################
#Script : Launch_daily_TMArchive_tar.sh
# Build Tar file for TMArchive folder in GFS_Backup to prevent operating system files overflow
# This script build a tar file containing every files and folder named with a specific date
# This script is part of a serie of 3 scripts doing the same thing for differents folders:
# DailyBackup / TMArchive / TMArchive
# Author: Arnaud Poulain
# Version :V2.0
#Change methode to tar files (not gbin) and add separtely files server_start and server_stop to the archive
#Checking tar content is done on comparaing  TM / TC / Server_start and stop number with directory
#
# Version :V1.1
# Evolution: Change line 52 to line 53 (find method to ls) cause find was too long.
# Date : 24/03/2014
############################################################################################

FILETYPE="TMArchive"
PARAMETERS=$@

display_usage() {
        echo -e "\n*********************************************************"
        echo -e "**                      << USAGE >>                    **"
        echo -e "*********************************************************"
        echo -e "\n!!!!!!!!!This script must be run as gaia_backup user!!!!!!!!\n" 
        echo -e " Description : This script will built a Tar file for each day passed as argument within yyyymmdd format, at least one argument\n"
        echo -e "\n $0 [yyyymmdd]{1,}"
        echo -e "\n -h  will provide this usage information"
        echo -e "\n -p (periode) will create a tar file per day between the two arguments"

        echo -e "\n              ***Exemple***"
        echo -e "\n $0 20140212 -- will create one file ${FILETYPE}_0212.tar containing all files and directory within 20140212"
        echo -e "\n $0 20140212 20140310 20140212 -- will create three diferents tar files for all those days"
        echo -e "\n $0 -p 20140212 20140214 will generate three tar files : "$FILETYPE"_0212.tar ${FILETYPE}_0213.tar ${FILETYPE}_0214.tar \n"
        #echo -e "-ns or --not secure, avoid Tar file testing after building, the script will only build a tar and won't check tar integrity with the current architecture\n"
        echo -e "\n              ***Logs and paths***\n"
        echo -e "\n ${FILETYPE}_yyyymmdd.log: This file contain all informations related to the tar, execution time,number of file, size, tar command verbose and final result² \n"
        echo -e " ${FILETYPE}_yyyymmdd.err this file show all errors relative to the check of tar integrity with achitecture\n"
        echo -e "*********************************************"  
        }
function tarList(){
        for param in "$@"

                do

                        ################### WORKING DIRECTORY ####################
#                        echo -e "\n********************* Working Directories  *****************\n" 

#                        cd /GFS_SAGA/DPCCTools/TarTools/testTMArchive               #SET Folder containing architecture needed to be Tar
#			echo /GFS_SAGA/DPCCTools/TarTools/testTMArchive >> $LOGFILE
#                        LOGFILE=/GFS_GBIN/tests/testTar/tBackup/Backup/log/${FILETYPE}_$param.log     #SET log detination
#			echo "/GFS_GBIN/tests/testTar/tBackup/Backup/log/${FILETYPE}_$param.log" >> $LOGFILE
#                        ERRORFILE=/GFS_GBIN/tests/testTar/tBackup/Backup/log/${FILETYPE}_$param.err      #SET Log only when an error occure
#			echo "/GFS_GBIN/tests/testTar/tBackup/Backup/log/${FILETYPE}_$param.err" >> $LOGFILE


                        LOGFILE=/GFS_SAGA/DPCCTools/TarTools/log/TMArchive/${FILETYPE}_$param.log                    #SET log destination
                        echo "Run Date    -- `date +%Y-%m-%d_%H-%M-%S`" | tee $LOGFILE
                        echo "SCRIPT INFO -- $0 Parameters -- $PARAMETERS" | tee -a $LOGFILE            #date to Log
                        echo -e "\n********************* Working Directories  *****************\n" | tee -a $LOGFILE
                        cd /GFS_BACKUP/Aspera/OPS/reception/untagged/TMArchive                                               #SET Folder containing architecture needed to be Tar
                        echo "'CD' -- /GFS_BACKUP/Aspera/OPS/reception/untagged/TMArchive" | tee -a $LOGFILE
                        echo "'LOGFILE -- '/GFS_SAGA/DPCCTools/TarTools/log/TMArchive/${FILETYPE}_$param.log" | tee -a $LOGFILE
                        ERRORFILE=/GFS_SAGA/DPCCTools/TarTools/log/TMArchive/${FILETYPE}_$param.err                  #SET Log only when an error occure
                        echo "'ERRORFILE -- '/GFS_SAGA/DPCCTools/TarTools/log/TMArchive/${FILETYPE}_$param.err" | tee -a $LOGFILE
                        echo -e "\n***********************************************\n" | tee -a $LOGFILE
			chmod 750 $LOGFILE
                        ###########################################################

#			nbFileTotal=$( find . -name "$param*" | wc -l)                 # Check if the date existe in the destination folder
                        nbFile=$( ls -R $param* | wc -l)
			nbServer=$( ls server_*_$param*.xml | wc -l)
			nbFileTotal=$(($nbFile+$nbServer))
			echo $nbFileTotal
                        if [ $nbFileTotal -gt 0 ]                                       # If more than 0 file found then proceed to the tar
                        then
                        echo "Starting to build tar file for day -- $param" | tee -a $LOGFILE
                        nbFileTC=$(ls -R $param* | grep PARC_TC | wc -l)  
			nbFileTM=$(ls -R $param* | grep PARC_TM | wc -l)  
                        echo "total file --  $nbFileTotal" | tee -a $LOGFILE                  #Number of gbin occurence to the log
                        echo "$param number TM -- $nbFileTM" | tee -a $LOGFILE      		#Number TM files to the log
                        echo "$param number TC -- $nbFileTC" | tee -a $LOGFILE 	     	#Number TC files to the log

                        echo -e "Dump size(Mo) -- \n`du -sm $param*`"| tee -a $LOGFILE        #Size of each folder to Tar

                        echo -e "\n********************* Tar Content *****************\n" | tee -a $LOGFILE
                        tar cvf ${FILETYPE}_${param: -4}.tar $param* --exclude *$param*.tar | tee -a $LOGFILE        #Building tar file within directories and files containing date format yyyymmdd and write log
                                                                                                                #exculding any *date*.tar file, in case the tar had already been built
                        echo -e "\n********************* Tar servers files  *****************\n" | tee -a $LOGFILE
			tar rvf ${FILETYPE}_${param: -4}.tar server_*_$param*.xml --exclude *$param*.tar | tee -a $LOGFILE 

                        echo -e "\n********************* Tar Diff *****************\n" | tee -a $LOGFILE
                        tar -dvf ${FILETYPE}_${param: -4}.tar | tee -a $LOGFILE                                                  #Check tar archive with the current architecture and write the Log file 
                        nbFileTar=$(tar tvf ${FILETYPE}_${param: -4}.tar | wc -l)                                          #count gbin inside the tar file
                        echo "Total file in the Tar --  $nbFileTar" | tee -a $LOGFILE

			echo -e "\n********************* Tar content *****************\n" | tee -a $LOGFILE
                        nbTMtar=$(tar -tf ${FILETYPE}_${param: -4}.tar | grep _TM_ | wc -l)
			echo "Number TM in the tar -- $nbFileTM" | tee -a $LOGFILE
			
			nbTCtar=$(tar -tf ${FILETYPE}_${param: -4}.tar | grep _TC_ | wc -l)
                        echo "Number TC in the tar -- $nbFileTC" | tee -a $LOGFILE

                        nbServertar=$(tar -tf ${FILETYPE}_${param: -4}.tar | grep server_ | wc -l)
                        echo "Number start & Stop in the tar -- $nbServertar" | tee -a $LOGFILE

			grep tar: $LOGFILE |tee $ERRORFILE

                                if  [ $nbFileTar -eq 0 ]; then                                                                     #Check gbin files number > 0 
                                echo "Number of file equal to 0" | tee -a $LOGFILE $ERRORFILE
                                fi                                                                              #Check log for ':' char meaning there was error and write error log in case.
                                if [ $nbTMtar -eq $nbFileTM  ]; then                                                                     #Check gbin files gap between Tar and architecture
                                        if test ! -s $ERRORFILE && test $nbTCtar -eq $nbFileTC && test $nbServer -eq $nbServertar; then       #Check numbers and size of errorlog (should be empty)
                                                if [ -f $LOGFILE ]  && [ -f $ERRORFILE ]; then                                                  #Verifie que les logs existes

                                                        echo -e "\n********************* Tar Status  *****************\n" | tee -a $LOGFILE                   #Report success or failed process base on "tar -d" and "Gbin count"
                                                        echo "${FILETYPE}_${param: -4}.tar successfull" | tee -a $LOGFILE
                                                        rm -f $ERRORFILE

                                                else
                                                        echo Tar wrong | tee -a $LOGFILE
                                                        echo "Error with log path please check program log link" | tee -a $LOGFILE $ERRORFILE
                                                fi
                                        else
                                        echo Tar wrong | tee -a $LOGFILE
                                        echo "Number of file with TM, TC and stop and start in directory -- $nbFileTM , $nbFileTC , $nbServer" | tee -a $LOGFILE $ERRORFILE
                                        echo "Number of file with TM, TC and stop and start in tar -- $nbTMtar , $nbTCtar , $nbServertar" | tee -a $LOGFILE $ERRORFILE
					echo "if following numbers are equal then an error present in errorFile: $ERRORFILE" | tee -a $LOGFILE $ERRORFILE
                                        fi
                                else
                                echo Tar wrong | tee -a $LOGFILE
                                echo "Number of file with TM        -- $nbFileTM" | tee -a $LOGFILE $ERRORFILE
                                echo "Number of file with TM in tar -- $nbTMtar" | tee -a $LOGFILE $ERRORFILE
				echo "Number of TM files is different between TAR and architecture" | tee -a $LOGFILE $ERRORFILE
                                fi
                        echo -e "\n***************************************************\n" | tee -a $LOGFILE
                        echo "END DATE -- `date +%Y-%m-%d_%H-%M-%S`" | tee -a $LOGFILE 
                        else
                                echo "WARN--no file found within $param" | tee -a $LOGFILE $ERRORFILE
                        fi
        done

chmod 750 $ERRORFILE
}
if [ "$USER" != "gaia_backup" ];then						#Force User to be logged as gaia_backup
	echo "You have to be log as gaia_backup to run this script"		
	exit
fi
if test  $# -eq 0								#Force User to add paramenters
	then
		display_usage
	exit
fi
while getopts "p:hs:" arg;do							#Check Option p h and s
case $arg in
p) if test  $# -eq 3 && [[ "$2" =~ [2][0][1-2][0-9][0-1][0-9][0-3][0-9] ]] && [[ "$3" =~ [2][0][1-2][0-9][0-1][0-9][0-3][0-9] ]] ; then
         
                START=$(date -d ${2:0:4}-${2:4:2}-${2:6:2} "+%y%m%d")				#Convert first parmeter after option to date yymmdd
                STOP=$(date -d ${3:0:4}-${3:4:2}-${3:6:2} "+%y%m%d")				#Convert second parmeter after option to date yymmdd
                typeset -i nbDays=0
                        if [ $STOP -ge $START ]							#Check if start date is before End dated
                                then
                                while (( $STOP >= $START ))					
                                        do
                                                tabDate[$nbDays]=$(date +%Y%m%d -d "$START")	#built table containing every day between Start and Stop Date
                                                let nbDays++					#table compteur +1
                                                START=$(date +%y%m%d -d "$START + 1 day")	#Day +1
                                        done
                                tarList ${tabDate[@]} 						# Send builted table with date to tarList function as argument
                        fi
                else
                echo -e "Error--Wrong usage, please check usage (-h)\n"				
                fi
		exit;;
s)echo "Not coded yet"
exit;;
:)echo "Error--Option -$OPTARG need arguments " ; exit ;;
h)display_usage
exit;; 
/?)exit;;
esac
done

case $1 in

[2][0][1-2][0-9][0-1][0-9][0-3][0-9] )	for parameters in "$@"								#Check if every parameters contain 8 numeric char with kind of date format
					do
						if ! [[ "$parameters" =~ [2][0][1-2][0-9][0-1][0-9][0-3][0-9] ]]; then
							echo -e "ERROR--Wrong format, please check usage (-h)\n"
							exit
						fi
					done
					tarList "$@";;									#If fitting send parameters to function tarList
*) echo -e "ERROR--Wrong entry, please check usage (-h)\n";;								#Any other parameters (letters, wrong format) return error
esac
