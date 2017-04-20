#!/bin/sh
echo -e "************DF GFS****************\n"
df -h | grep GFS
echo "**********************************"
echo "**********LOG SYNC CRON***********"
echo -e "**********************************\n"
tail -n 3 /GFS_SAGA/DPCCTools/logs/SynchroTools/Sync*
echo "**********************************"
echo "**********Process JAVA************"
echo -e "**********************************\n"
ps -eaf | grep java
echo "**********************************"
echo "**********SORTED DATA*************"
echo -e "**********************************\n"
ls -lrt /GFS_GBIN/SortedData/IDT | tail
echo "**********************************"
echo "*********DDM NOTIFICATION*********"
echo -e "**********************************\n"
tail -5 /GFS_SAGA/DPCCTools/logs/DDM/DDMNotification.log
echo "**********************************"
echo "************DDM UTILS*************"
echo -e "**********************************\n"
tail -5 /GFS_SAGA/DPCCTools/logs/DDM/DDMUtils.log
echo "**********************************"
echo "**********DB DDM BACKUP***********"
echo -e "**********************************\n"
tail -5 /GFS_SAGA/DPCCTools/logs/DDM/Backup-SessionsDBOpe.log

