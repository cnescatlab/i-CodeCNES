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
# File	      : deactivatesyncbackup.sh
#
# Author      : B. Frezouls
# Company     : CNES
#
# Langage     : Shell
# Comment : This script deactivates synchronization and
# backup of data used by DPCC processings, received in the OPS channel
# of the OPS platform.
#
# ------------------------------------------------------------------ #
# HISTORY
# VERSION	DATE		AUTHOR		COMMENT
# V1.0		10/04/2014	FREZOULS	Creation	 
# END-HISTORY
# ------------------------------------------------------------------*/


STARTTIME=$(date)


echo "$STARTTIME : Deactivation of Backup synchronization"

if [ -f ./fakesyncbackup.sh ]; then
        if [ -f ./truesyncbackup.sh ]; then
                echo "Incoherent state : ./fakesyncbackup.sh and ./truesyncbackup.sh should not be both present at same time !"
        else
		mv ./syncbackup.sh truesyncbackup.sh
		mv ./fakesyncbackup.sh syncbackup.sh
		echo "Backup synchronization deactivated !"
        fi
else
	if [ -f ./truesyncbackup.sh ]; then
		echo "Backup synchronization already deactivated !"
	else
                echo "Incoherent state : neither ./fakesyncbackup.sh nor ./truesyncbackup.sh is present !"
	fi
fi




