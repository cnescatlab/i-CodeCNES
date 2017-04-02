#!/bin/sh
#set -x
FOUND=0

USER=$(whoami)
SYNCHROTOOLS_PATH=/GFS_SAGA/DPCCTools/SynchroTools
echo "Executed as $USER"

pgrep syncbackup.sh
retval=$?
if [ $retval -eq $FOUND ]; then
	echo "Syncbackup.sh is already running. It has not been relaunched !"
else
	cd $SYNCHROTOOLS_PATH
	./syncbackup.sh
fi
