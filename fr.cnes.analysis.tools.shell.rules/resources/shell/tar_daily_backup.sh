#!/bin/sh
############################################################################################
#Script : Launch_daily_backup_tar.sh
# Build Tar file for Daily Backup folder to prevent operating system files overflow
# This script build a tar file containing every files and folder named with a specific date
# This script is part of a serie of 3 scripts doing the same thing for differents folders:
# DailyBackup / DailyBackup / TMArchive
# Author: Arnaud Poulain
# Version :V1.2
# V1.2: Modification de la création du tar *date* en date* pour les sessions car prise en compte de session n'ayant aucun rapport.
# Ajout tar rvf  daily_start_date*.xml, daily_stop_date*.xml et deliveryform_date* pour les ajouter à l'archive
# Evolution: Change line 52 to line 53 (find method to ls) cause find was too long.
# Date : 24/03/2014
############################################################################################
FILETYPE="DailyBackup"
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
			
#			cd /GFS_GBIN/tests/testTar/tBackup/Backup              				#SET Folder containing architecture needed to be Tar
#                        LOGFILE=/GFS_GBIN/tests/testTar/tBackup/Backup/log/${FILETYPE}_$param.log	#SET log detination
#                        ERRORFILE=/GFS_GBIN/tests/testTar/tBackup/Backup/log/${FILETYPE}_$param.err     #SET Log only when an error occure

                         LOGFILE=/GFS_SAGA/DPCCTools/TarTools/log/DailyBackup/${FILETYPE}_$param.log                    #SET log destination
                         echo "Run Date    -- `date +%Y-%m-%d_%H-%M-%S`" | tee $LOGFILE
                         echo "SCRIPT INFO -- $0 Parameters -- $PARAMETERS" | tee -a $LOGFILE            #date to Log
                         echo -e "\n********************* Working Directories  *****************\n" | tee -a $LOGFILE
                         cd /GFS_BACKUP/Aspera/OPS/reception/daily/backup                                               #SET Folder containing architecture needed to be Tar
                         echo "'CD' -- /GFS_BACKUP/Aspera/OPS/reception/daily/backup" | tee -a $LOGFILE
                         echo "'LOGFILE -- '/GFS_SAGA/DPCCTools/TarTools/log/DailyBackup/${FILETYPE}_$param.log" | tee -a $LOGFILE
                         ERRORFILE=/GFS_SAGA/DPCCTools/TarTools/log/DailyBackup/${FILETYPE}_$param.err                  #SET Log only when an error occure
                         echo "'ERRORFILE -- '/GFS_SAGA/DPCCTools/TarTools/log/DailyBackup/${FILETYPE}_$param.err" | tee -a $LOGFILE
                         echo -e "\n***********************************************\n" | tee -a $LOGFILE
			chmod 750 $LOGFILE

			###########################################################

#                       nbFileTotal=$( find . -name "*$param*" | wc -l)                 # Check if the date existe in the destination folder
                        nbSession=$( ls $param* | wc -l)
                         if [ $nbSession -gt 0 ]                                       # If more than 0 file found then proceed to the tar
                         then
                         echo "Starting to build tar file for day -- $param" | tee -a $LOGFILE
                         nbGbin=$(ls -R $param* | grep gbin | wc -l)                             #Number of gbin files
                         nbServer=$(ls daily_*_$param*.xml | wc -l)
                         echo "total Gbins -- $nbGbin" | tee -a $LOGFILE                         #Number of gbin occurence to the log
                         echo "Nb  session -- $nbSession" | tee -a $LOGFILE              #Number of occurence with the date to the log
                         echo "Start Stop  -- $nbServer" | tee -a $LOGFILE
                         echo -e "Dump size(Mo) -- \n`du -sm $param*`"  | tee -a $LOGFILE      #Size of each folder to Tar
 
 
                         echo -e "\n********************* Tar Content *****************\n" | tee -a $LOGFILE
                         tar cvf ${FILETYPE}_${param: -4}.tar $param* --exclude *$param*.tar | tee -a $LOGFILE        #Building tar file within directories and files containing date format yyyymmdd and write log
                                                                                                                 #exculding any *date*.tar file, in case the tar had already been built
 
                         echo -e "\n********************* Append Tar daily_start_stop  *****************\n" | tee -a $LOGFILE
                         tar rvf ${FILETYPE}_${param: -4}.tar daily_*_$param*.xml | tee -a $LOGFILE
 
                         echo -e "\n********************* Tar Diff *****************\n" | tee -a $LOGFILE
                         tar -dvf ${FILETYPE}_${param: -4}.tar | tee -a $LOGFILE                                                               #Check tar archive with the current architecture and write the Log file
 
                         echo -e "\n********************* Tar content *****************\n" | tee -a $LOGFILE
                         nbTarGbin=$(tar tf ${FILETYPE}_${param: -4}.tar | grep gbin |  wc -l)                                          #count gbin inside the tar file
                         echo "Total Gbins in the Tar --  $nbTarGbin" | tee -a $LOGFILE
 
                         nbTarStartStop=$(tar tf ${FILETYPE}_${param: -4}.tar | grep daily_ |  wc -l)                                    #count START/STOP inside the tar file
                         echo "Total daily start and stop in the Tar --  $nbTarStartStop" | tee -a $LOGFILE
                         grep : $LOGFILE > $ERRORFILE                                                                            #Check log for ':' char meaning there was error and write error log in case.

                                if  [ $nbGbin -eq 0 ]; then                                                                     #Check gbin files number > 0 
                                echo "Number of Gbin equal to 0" | tee -a $ERRORFILE
                                fi

                                if [ $nbTarGbin -eq $nbGbin ]; then                                                                             #Check gbin files gap between Tar and architecture

                                        if test ! -s $ERRORFILE && test $nbServer -eq $nbTarStartStop; then     #Check start stop numbers and size of errorlog (should be empty)

                                                if [ -f $LOGFILE ]  && [ -f $ERRORFILE ]; then                                                  #Verifie que les logs existes

                                                        echo -e "\n********************* Tar Status  *****************\n" | tee -a $LOGFILE     #Report success or failed process base on "tar -d" and "Gbin count"
                                                        echo "${FILETYPE}_${param: -4}.tar successfull" | tee -a $LOGFILE
                                                        rm -f $ERRORFILE

                                                else
                                                        echo Tar wrong >> $LOGFILE
                                                        echo "Error with log path please check program log link" | tee -a $LOGFILE $ERRORFILE
                                                        echo Tar wrong | tee -a $LOGFILE
                                                fi
                                        else
                                        echo Tar wrong | tee -a $LOGFILE
                                        echo "Error occure during tar, check error log: $ERRORFILE" | tee -a $LOGFILE
                                        echo "Number of start stop file different between TAR and architecture OR Errorlog size not empty (should be empty)" >> $ERRORFILE
                                        fi
                                else
                                echo Tar wrong >> $LOGFILE
                                echo "Number of Gbin different between TAR and architecture -- TAR: $nbTarGbin -- directory : $nbGbin" | tee -a $ERRORFILE
                                fi
                        echo -e "\n***************************************************\n" | tee -a $LOGFILE
                        echo "date -- `date +%Y-%m-%d_%H-%M-%S`" | tee -a $LOGFILE
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
