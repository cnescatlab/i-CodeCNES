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
# File	      : deactivatesyncTMArchive.sh
#
# Author      : B. Frezouls
# Company     : CNES
#
# Langage     : Shell
# Comment : This script deactivates synchronization of TMArchive 
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


echo "$STARTTIME : Deactivation of TMArchive synchronization"

if [ -f ./fakesyncTMArchive.sh ]; then
        if [ -f ./truesyncTMArchive.sh ]; then
                echo "Incoherent state : ./fakesyncTMArchive.sh and ./truesyncTMArchive.sh should not be both present at same time !"
        else
		mv ./syncTMArchive.sh truesyncTMArchive.sh
		mv ./fakesyncTMArchive.sh syncTMArchive.sh
		echo "TMArchive synchronization deactivated !"
        fi
else
	if [ -f ./truesyncTMArchive.sh ]; then
		echo "TMArchive synchronization already deactivated !"
	else
                echo "Incoherent state : neither ./fakesyncTMArchive.sh nor ./truesyncTMArchive.sh is present !"
	fi
fi




