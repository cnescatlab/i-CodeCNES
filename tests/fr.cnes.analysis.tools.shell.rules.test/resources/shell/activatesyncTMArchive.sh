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
# File	      : activatesyncbackup.sh
#
# Author      : B. Frezouls
# Company     : CNES
#
# Langage     : Shell
# Comment : This script activates the synchronization of TMArchive 
# data used by DPCC processings, received in the OPS channel
# of the OPS platform.
#
# ------------------------------------------------------------------ #
# HISTORY
# VERSION	DATE		AUTHOR		COMMENT
# V1.0		10/04/2014	FREZOULS	Creation	 
# END-HISTORY
# ------------------------------------------------------------------*/


STARTTIME=$(date)


echo "$STARTTIME : Activation of TMArchive synchronization"

if [ -f ./fakesyncTMArchive.sh ]; then
        if [ -f ./truesyncTMArchive.sh ]; then
                echo "Incoherent state : ./fakesyncTMArchive.sh and ./truesyncTMArchive.sh should not be both present at same time !"
        else
		echo "TMArchive synchronization already activated !"
        fi
else
	if [ -f ./truesyncTMArchive.sh ]; then
		mv ./syncTMArchive.sh fakesyncTMArchive.sh
		mv ./truesyncTMArchive.sh syncTMArchive.sh
		echo "TMArchive synchronization activated"
	else
                echo "Incoherent state : neither ./fakesyncTMArchive.sh nor ./truesyncTMArchive.sh is present !"
	fi
fi




