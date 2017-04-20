#!/bin/sh
#set -x
FOUND=0

SYNCHROTOOLS_PATH=/GFS_SAGA/DPCCTools/SynchroTools
START_TIME=$(date)

pgrep syncTMArchive
retval=$?
if [ $retval -eq $FOUND ]; then
	echo "$START_TIME : syncTMArchive.sh is already running. It has not been relaunched !"
else
	cd $SYNCHROTOOLS_PATH	
	./syncTMArchive.sh
fi
