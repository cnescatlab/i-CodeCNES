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
# File	      : activatesyncbackupnominal.sh
#
# Author      : B. Frezouls
# Company     : CNES
#
# Langage     : Shell
# Comment : This script activates the synchronization and
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


echo "$STARTTIME : Activation of Backup/Nominal synchronization"

if [ -f ./fakesyncbackupnominal.sh ]; then
        if [ -f ./truesyncbackupnominal.sh ]; then
                echo "Incoherent state : ./fakesyncbackupnominal.sh and ./truesyncbackupnominal.sh should not be both present at same time !"
        else
		echo "Backup/Nominal synchronization already activated !"
        fi
else
	if [ -f ./truesyncbackupnominal.sh ]; then
		mv ./syncbackupnominal.sh fakesyncbackupnominal.sh
		mv ./truesyncbackupnominal.sh syncbackupnominal.sh
		echo "Backup/Nominal synchronization activated"
	else
                echo "Incoherent state : neither ./fakesyncbackupnominal.sh nor ./truesyncbackupnominal.sh is present !"
	fi
fi




