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
# File        : syncTMArchive.sh
#
# Author      : B. Frezouls
# Company     : CNES
#
# Langage     : Shell
# Comment : This script allows the synchronization of the TMArchive data
# received in the OPS channel of the OPS platform.
#
# ------------------------------------------------------------------ #
# HISTORY
# VERSION       DATE            AUTHOR          COMMENT
# V2.0          06/02/2014      FREZOULS        Launchable via crontab         
# V1.0          06/01/2014      FREZOULS        Creation         
# END-HISTORY
# ------------------------------------------------------------------*/


#set -x
USER=$(whoami)

echo "Executed as $USER"

STARTTIME=$(date)
echo "$STARTTIME : TMArchive synchronization deactivated"



