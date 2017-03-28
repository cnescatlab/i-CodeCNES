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
# File	      : syncbackupnominal.sh
#
# Author      : B. Frezouls
# Company     : CNES
#
# Langage     : Shell
# Comment : This script allows the synchronization and
# backup of data used by DPCC processings, received in the OPS channel
# of the OPS platform.
#
# ------------------------------------------------------------------ #
# HISTORY
# VERSION	DATE		AUTHOR		COMMENT
# V2.3		06/05/2014	POULAIN		Remove error re-routing to void in order to show all error see IM:
# V2.2		16/04/2014	FREZOULS	Correct synchronisation of nominal again (PATH where referring to backup)	 
# V2.1		09/04/2014	FREZOULS	Correct synchronisation of nominal	 
# V2.0		06/02/2014	FREZOULS	Launchable via crontab	 
# 									Changement de la politique de suppression des donn�es 
# V1.0		06/01/2014	FREZOULS	Creation	 
# END-HISTORY
# ------------------------------------------------------------------*/

#set -x
WHERE=$(pwd)

# blabla blabla blabla blabla
REMOTEHOST=user@1.1.1.1
# blabla blabla blabla blabla
REMOTENOMINAL=/Aspera/OPS/reception/daily/nominal
# directory in which are synchronized the files to be processed
PROCESSDATA=/GFS_GBIN/Aspera/OPS/reception/daily/nominal
# directory in which are backuped the files to be processed in DPCC
BACKUPDATA=/GFS_BACKUP/Aspera/OPS/reception/daily/nominal

TARGETRATE=300000   # blabla blabla blabla blabla blabla e

#TTE= blabla blabla blabla blabla blabla blabla blabla blabla 
                  #    blabla blabla blabla blabla 

TRUE=1
FALSE=0


#export blabla blabla blabla blabla blabla blabla blabla blabla 

mkdir -p $PROCESSDATA
mkdir -p $BACKUPDATA

# syncFolder - returns $SUCCESS -eq $TRUE if it passes $FALSE if it fails
syncfoldernominal()
{
#     ascp -k2 -l $TARGETRATE -p -P 987654 -Q -o Overwrite=always $REMOTEHOST":"$REMOTENOMINAL"/"$SOURCE "$PROCESSDATA" 2>/dev/null
    ascp -k2 -l $TARGETRATE -i /home/gaia_dex/.ssh/id_rsa -P 987654 -Q -o Overwrite=always $REMOTEHOST":"$REMOTENOMINAL"/"$SOURCE "$PROCESSDATA"
   if [ $? -eq 0 ]; then
        SUCCESS=$TRUE
    else
        echo "$0: ascp failed."
        SUCCESS=$FALSE
    fi

    SOURCE=""
}

# syncFolder - returns $SUCCESS -eq $TRUE if it passes $FALSE if it fails
syncfolderbackup()
{
     ascp -k2 -l $TARGETRATE -i /home/gaia_dex/.ssh/id_rsa -P 987654 -Q -o Overwrite=always $REMOTEHOST":"$REMOTENOMINAL"/"$SOURCE "$BACKUPDATA" 
    if [ $? -eq 0 ]; then
        SUCCESS=$TRUE
    else
        echo "$0: ascp failed."
        SUCCESS=$FALSE
    fi

    SOURCE=""
}


STARTTIME=$(date
)
echo "Synchronization process started on $STARTTIME"
# get listing of remote directory
Liste=$(ssh -p 987654 $REMOTEHOST ls $REMOTENOMINAL/daily_stop\*.xml 2>/dev/null)

# BF : is the next command useful ?
#cd $LOCALDATA

if [[ "$Liste" != "" ]];then
    for ficstop in $Liste
    do
        num=$(echo $ficstop| awk -F "_" '{print $3}' | awk -F "." '{print $1}')
        nomstart=daily_start_"$num".xml
        nomstop=daily_stop_"$num".xml
        nomrep="$num"

        echo SessionStart: $nomstart
	SOURCE=$nomstart
        syncfolderbackup
        if [ $SUCCESS -eq $TRUE ]; then
	    SOURCE=$nomstart
            syncfoldernominal
            if [ $SUCCESS -eq $TRUE ]; then
            	echo Session directory: $nomrep
            	SOURCE=$nomrep
            	syncfolderbackup
            	if [ $SUCCESS -eq $TRUE ]; then
            	    SOURCE=$nomrep
            	    syncfoldernominal
            	    if [ $SUCCESS -eq $TRUE ]; then
                        echo SessionStop: $nomstop
		        SOURCE=$nomstop
                        syncfolderbackup
                        if [ $SUCCESS -ne $TRUE ]; then
                    	    echo "Failure synchronizing $nomstop to Backup"
		        else
		            SOURCE=$nomstop
			    syncfoldernominal
                            if [ $SUCCESS -ne $TRUE ]; then
                    	        echo "Failure synchronizing $nomstop to Nominal"
			    else
			        ssh -p 987654 $REMOTEHOST rm -rf $REMOTENOMINAL/$nomstart 
			        ssh -p 987654 $REMOTEHOST rm -rf $REMOTENOMINAL/$nomrep 
			        ssh -p 987654 $REMOTEHOST rm -rf $REMOTENOMINAL/$nomstop
                       	    echo "Synchronization of session $num successful"
		            fi
			fi
		    else
              	        echo "Failure synchronizing $nomrep to Nominal"
		    fi
      	        else
              	    echo "Failure synchronizing $nomrep to Backup"
            	fi
	    else
	        echo "Failure synchronizing $nomstart to Nominal"
	    fi
        else
            echo "Failure synchronizing $nomstart to Backup"
        fi
    done
else
  echo "No file to synchronize"
fi

echo "Synchronization finished"


ENDTIME=$(date)

echo "$ENDTIME - BackupNominal synchronization finished"

