#!/bin/sh

#$Header$
# ------------------------------------------------------------------ #
#
#                              GAIA
#
# ------------------------------------------------------------------ #
# Project      : DPCCTools
#
# Component   : synchro-tools
# File        : syncTMArchive.sh
#
# Author      : B. Frezouls
# Company     : CNES
#
# Langage     : Shell
# Comment : This script allows the synchronization of the TMArchive data
# received in the OPS channel of the OPS platform.
#
# ------------------------------------------------------------------ #
# HISTORY
# VERSION       DATE            AUTHOR          COMMENT
# V2.2          06/05/2014      POULAIN         Remove error re-routing to void in order to show all error see IM:
# V2.1		22/04/2014	POULAIN		Change REMOTEBACKUP and BACKUPDATA path OPE --> OPS
# V2.0          06/02/2014      FREZOULS        Launchable via crontab      
# 												Changement de la politique de suppression des donn�es=
# V1.0          06/01/2014      FREZOULS        Creation         
# END-HISTORY
# ------------------------------------------------------------------*/



#set -x
WHERE=$(pwd)

# blabla blabla blabla blabla
REMOTEHOST=user@1.1.1.1
# blabla blabla blabla blabla
REMOTEBACKUP=/Aspera/OPS/reception/untagged/TMArchive 				#### MODIF 2.1 ####
# directory in which the TMArchive files are backuped in the backup area
BACKUPDATA=/GFS_BACKUP/Aspera/OPS/reception/untagged/TMArchive  		#### MODIF 2.1 ####

TARGETRATE=300000   # The highest rate the Aspera sync will try to achieve

#TTE= blabla blabla blabla blabla blabla blabla blabla blabla 
                 #    blabla blabla blabla blabla 

TRUE=1
FALSE=0


#export blabla blabla blabla blabla blabla blabla blabla 

# create the destination directory
mkdir -p $BACKUPDATA

# syncFolder - returns $SUCCESS -eq $TRUE if it passes $FALSE if it fails
syncfolder()
{
#    ascp -k2 -l $TARGETRATE -p -P 987654 -Q -o Overwrite=always $REMOTEHOST":"$REMOTEBACKUP"/"$SOURCE "$BACKUPDATA" 2>/dev/null
    ascp -k2 -l $TARGETRATE -i /home/gaia_dex/.ssh/id_rsa -P 987654 -Q -o Overwrite=always $REMOTEHOST":"$REMOTEBACKUP"/"$SOURCE "$BACKUPDATA"
    if [ $? -eq 0 ]; then
        SUCCESS=$TRUE
    else
        echo "$0: ascp failed."
        SUCCESS=$FALSE
    fi

    SOURCE=""
}



STARTTIME=$(date)
echo "TMArchive synchronization process started on $STARTTIME"
# get listing of remote directory
Liste=$(ssh -p 987654 $REMOTEHOST ls $REMOTEBACKUP/server_stop\*.xml 2>/dev/null)

if [[ "$Liste" != "" ]];then
    for ficstop in $Liste
    do
        num=$(echo $ficstop| awk -F "_" '{print $3}' | awk -F "." '{print $1}')
        nomstart=server_start_"$num".xml
        nomstop=server_stop_"$num".xml
        nomrep="$num"

        echo SessionStart: $nomstart
		SOURCE=$nomstart
        syncfolder
        if [ $SUCCESS -eq $TRUE ]; then
            echo Session directory: $nomrep
			SOURCE=$nomrep
            syncfolder
            if [ $SUCCESS -eq $TRUE ]; then
                echo SessionStop: $nomstop
				SOURCE=$nomstop
                syncfolder
                if [ $SUCCESS -ne $TRUE ]; then
                    	echo "Failure synchronizing $nomstop"
		else
			ssh -p 987654 $REMOTEHOST rm -rf $REMOTEBACKUP/$nomstart
			ssh -p 987654 $REMOTEHOST rm -rf $REMOTEBACKUP/$nomrep
			ssh -p 987654 $REMOTEHOST rm -rf $REMOTEBACKUP/$nomstop 
			
              		echo "Synchronization of session $num successful"
		fi
           else
              echo "Failure synchronizing $nomrep"
            fi
        else
            echo "Failure synchronizing $nomstart"
        fi

    done

else
  echo "No file to synchronize"
fi

ENDTIME=$(date)
echo "$ENDTIME - TMArchive synchronization finished"




