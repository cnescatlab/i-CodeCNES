#!/bin/sh
#set -x
FOUND=0

USER=$(whoami)
echo "Executed as $USER"
SYNCHROTOOLS_PATH=/GFS_SAGA/DPCCTools/SynchroTools

pgrep syncbackupnomin
retval=$?
if [ $retval -eq $FOUND ]; then
	echo "Syncbackupnominal.sh is already running. It has not been relaunched !"
else
	cd $SYNCHROTOOLS_PATH
	./syncbackupnominal.sh
fi
