#!/bin/sh
############################################################################################
#Script : Delete_TMArchive.sh
# Delete all files to prevent operating system files overflow after tar of 
# This script build a tar file containing every files and folder named with a specific date
# This script is part of a serie of 3 scripts doing the same thing for differents folders:
# DailyBackup / DailyNominal / TMArchive
# Author: Arnaud Poulain
# Version :V0
# Date : 24/03/2014
############################################################################################
FILETYPE=TMArchive
PARAMETERS=$@
display_usage() {
        echo -e "\n*********************************************************"
        echo -e "**                      << USAGE >>                    **"
        echo -e "*********************************************************"
	echo -e "\n!!!!!!!!!This script must be run as gaia_backup user!!!!!!!!\n" 
        echo -e " Description : This script will delete file for each day passed as argument within yyyymmdd format, at least one argument. Some check are done prior deleting:\n"
        echo -e " Files: ${FILETYPE}_yyyymmdd.del or/and ${FILETYPE}_yyyymmdd.err or/and ${FILETYPE}_yyyymmdd.log or/and ${FILETYPE}_yyyymmdd.tar or/and *yyyymmdd*.tar not present, ${FILETYPE}_mmdd.log ${FILETYPE}_yyyymmdd.log found\n"
        echo -e "\n $0 [yyyymmdd]{1,}"
        echo -e "\n -h  will provide this usage information"
        echo -e "\n -p (periode) will delete files per day between the two arguments"

        echo -e "\n              ***Exemple***"
        echo -e "\n $0 20140212 -- will delete all files and folder containing 20140212 in the name"
        echo -e "\n $0 20140212 20140310 20140212 -- will delete all files containing 20140212 or  20140310 or 20140212 in the name"
        echo -e "\n $0 -p 20140212 20140214 will delete all folders and files containing 20140212 or 20140213\n"
        #echo -e "-ns or --not secure, avoid Tar file testing after building, the script will only build a tar and won't check tar integrity with the current architecture\n"
        echo -e "\n              ***Logs and paths***\n"
        echo -e "\n ${FILETYPE}_yyyymmdd.log: This file contain all informations related to the tar, execution time,number of file, size, tar command verbose and final result \n"
        echo -e " ${FILETYPE}_yyyymmdd.err this file show all errors relative to the check of tar integrity with achitecture\n"
        echo -e "*********************************************"  
        }

function tarList(){
        for param in "$@"

                do
                        ################### WORKING DIRECTORY ####################

#                        cd /GFS_GBIN/tests/testTar/tBackup/Backup               #SET Folder containing architecture needed to be Tar
#                        LOGFILE=/GFS_GBIN/tests/testTar/tBackup/Backup/log/${FILETYPE}_$param.log     #SET log detination
#                        ERRORFILE=/GFS_GBIN/tests/testTar/tBackup/Backup/log/${FILETYPE}_$param.err      #SET Log only when an error occure

#                        cd /GFS_BACKUP/Aspera/OPS/reception/daily/backup                               #SET Folder containing architecture needed to be Tar
#                        LOGFILE=/GFS_SAGA/DPCCTools/TarTools/log/DailyBackup/${FILETYPE}_$param.log    #SET log detination
#                       ERRORFILE=/GFS_SAGA/DPCCTools/TarTools/log/DailyBackup/${FILETYPE}_$param.err   #SET Log only when an error occure
                        LOGDELETE=/GFS_SAGA/DPCCTools/TarTools/log/TMArchive/${FILETYPE}_$param.del   
			LOGFILE=/GFS_SAGA/DPCCTools/TarTools/log/TMArchive/${FILETYPE}_$param.log 	#SET log destination
                        cd /GFS_BACKUP/Aspera/OPS/reception/untagged/TMArchive/                               #SET Folder containing architecture needed to be Tar
                        ERRORFILE=/GFS_SAGA/DPCCTools/TarTools/log/TMArchive/${FILETYPE}_$param.err   #SET Log only when an error occure

                        ###########################################################
                        
                if [ -f $LOGDELETE ]; then

                        echo "date -- `date +%Y-%m-%d_%H-%M-%S`"  | tee -a $LOGDELETE             #date to Log
                        echo "SCRIPT -- $0 Parameters : $PARAMETERS"  | tee -a $LOGDELETE
                        echo "ERROR--Found an delete log file for this date :$param. Files might have been deleted already. Action: Stopped and nothing deleted" | tee -a $LOGDELETE 
                else
                        echo "date -- `date +%Y-%m-%d_%H-%M-%S`" >> $LOGDELETE             #date to Log
                        chmod 750 $LOGDELETE                                            #give right to group saga_ope to check log
                        echo "SCRIPT -- $0 Parameters : $PARAMETERS"  | tee -a $LOGDELETE
                        echo "[OK] : No deletion log found about previous deletion on date $param" | tee -a $LOGDELETE
                        if [ -f $LOGFILE ]; then
                                echo "[OK] : Found log file for date $param, previous script were runned"  | tee -a $LOGDELETE
                                if [ -f $ERRORFILE ]; then
                                echo "WARN--Found an error log for this date :$param. Action: Skipped and nothing deleted" | tee -a $LOGDELETE
                                else
                                echo "[OK] : No error file found for date $param, Tar runned without error" | tee -a $LOGDELETE

                                        if [ -f ${FILETYPE}_${param: -4}.tar ]; then
                                                echo "[OK] : ${FILETYPE}_${param: -4}.tar file was found"  | tee -a $LOGDELETE
                                                if [ -f ${FILETYPE}_${param}.tar ]; then

                                                echo "WARN--${FILETYPE}_${param}.tar file found, removal might already been applied. Action: Skipped and nothing deleted" | tee -a $LOGDELETE
                                                else
                                                        echo "[OK] : ${FILETYPE}_${param}.tar file not found, deletion file and renaming were not applied yet"  | tee -a $LOGDELETE
                                                        nbTar=$( ls -R "$param*.tar" | wc -l)
                                                        if [ $nbTar -eq 0 ];then

                                                                nbFile=$( ls -R $param* | wc -l)
                                                                if [ $nbFile -gt 0 ]
                                                                then
                                                                nbTMFiles=$(ls -R $param* | grep _TM_ | wc -l)
                                                                echo "Number of TM files in current architecture before deletion : $nbTMFiles"  | tee -a $LOGDELETE
                                                                nbTarTMFiles=$(tar tvf ${FILETYPE}_${param: -4}.tar | grep _TM_ |  wc -l)
                                                                echo "Number of TM file in the tar : $nbTarTMFiles"  | tee -a $LOGDELETE

                                                                nbTCFiles=$(ls -R $param* | grep _TC_ | wc -l)
                                                                echo "Number of TC files in current architecture before deletion : $nbTCFiles"  | tee -a $LOGDELETE
                                                                nbTarTCFiles=$(tar tvf ${FILETYPE}_${param: -4}.tar | grep _TC_ |  wc -l)
                                                                echo "Number of TC file in the tar : $nbTarTCFiles"  | tee -a $LOGDELETE


                                                                nbServerFiles=$(ls -R *$param* | grep server | wc -l)
                                                                echo "Number of server files in current architecture before deletion : $nbServerFiles"  | tee -a $LOGDELETE
                                                                nbTarServerFiles=$(tar tvf ${FILETYPE}_${param: -4}.tar | grep server |  wc -l)
                                                                echo "Number of server file in the tar : $nbTarServerFiles"  | tee -a $LOGDELETE
                                                                        if test $nbTarTMFiles -eq $nbTMFiles && test $nbTarTCFiles -eq $nbTCFiles && test $nbServerFiles -eq $nbTarServerFiles; then
                                                                                rm -rf $param*
                                                                                echo "[OK] : CLEAR--Action : File within $param* removed "  | tee -a $LOGDELETE

                                                                                rm -rf server_*_$param*.xml
                                                                                 echo "[OK] : CLEAR--Action : File within server_*_$param*.xml removed "  | tee -a $LOGDELETE

                                                                                mv ${FILETYPE}_${param: -4}.tar ${FILETYPE}_${param}.tar
                                                                                echo "[OK] : CLEAR--Action : file ${FILETYPE}_${param: -4}.tar renamed ${FILETYPE}_${param}.tar"  | tee -a $LOGDELETE
                                                                        else
                                                                        echo "WARN--difference of Server file or TM number beetween tar file and architecture for day : *$param*. Action: Skipped and nothing deleted"  | tee -a $LOGDELETE
                                                                        fi
                                                                else
                                                                echo "WARN--no file with *$param* found. Action: Skipped and nothing deleted"  | tee -a $LOGDELETE

                                                                fi
                                                        else
                                                        echo "WARN--Tar Archive found with format: *$param*.tar. Rename the file or delete it manualy to continue, script avoid erasing tar file with date in it. Action: Skipped and nothing deleted"  | tee -a $LOGDELETE
                                                        fi
                                                fi
                                        else
                                        echo "WARN--${FILETYPE}_${param: -4}.tar not found. Action: Skipped and nothing deleted"  | tee -a $LOGDELETE
                                        fi
                                fi
                        else
                        echo "WARN--No log file found for this date :$param. Action: Skipped and nothing deleted"  | tee -a $LOGDELETE
                        fi
                fi
        done
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
