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
# File        : syncbackup.sh
#
# Author      : B. Frezouls
# Company     : CNES
#
# Langage     : Shell
# Comment : This script allows the synchronization of the backup data
# (not used in DPCC), received in the OPS channel of the OPS platform.
#
# ------------------------------------------------------------------ #
# HISTORY
# VERSION       DATE            AUTHOR          COMMENT
# V2.1		06/05/2014      POULAIN         Remove error re-routing to void in order to show all error see IM:
# V2.0          06/02/2014      FREZOULS        Launchable via crontab
# 												Changement de la politique de suppression des donn�es
# V1.0          06/01/2014      FREZOULS        Creation         
# END-HISTORY
# ------------------------------------------------------------------*/


#set -x
WHERE=$(pwd)

REMOTEHOST=user@1.1.1.1
# ajout de blabla
REMOTEBACKUP=/Aspera/OPS/reception/daily/backup
# directory in which MDB files not processed in DPCC are backuped 
BACKUPDATA=/GFS_BACKUP/Aspera/OPS/reception/daily/backup

TARGETRATE=300000   # The highest rate the Aspera sync will try to achieve

#TCF= #Optional : If blabla set blabla
                  # blablablablablabla
TRUE=1
FALSE=0


#export blabla
mkdir -p $BACKUPDATA

# syncFolder - returns $SUCCESS -eq $TRUE if it passes $FALSE if it fails
syncfolder()
{
#    ascp -k2 -l $TARGETRATE -p -P 987654 -Q -o RemoveAfterTransfer=yes,RemoveEmptyDirectories=yes,Overwrite=always $REMOTEHOST":"$REMOTEBACKUP"/"$SOURCE "$BACKUPDATA" 2>/dev/null
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
echo "Synchronization process started on $STARTTIME"
# get listing of remote directory
Liste=$(ssh -p 987654 $REMOTEHOST find $REMOTEBACKUP/daily_stop\*.xml 2>/dev/null)

if [[ "$Liste" != "" ]];then
    for ficstop in $Liste
    do
        num=$(echo $ficstop| awk -F "_" '{print $3}' | awk -F "." '{print $1}')
        nomstart=daily_start_"$num".xml
        nomstop=daily_stop_"$num".xml
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
echo "$ENDTIME - Backup synchronization finished"
