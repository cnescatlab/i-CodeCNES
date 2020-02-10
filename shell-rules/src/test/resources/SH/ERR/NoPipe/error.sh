#!/bin/bash

echo "------------------"
echo "- SH.ERR.NoPipe  -"
echo "-    KO          -"
echo "------------------"

#set -x
#set -e

#
# --- MAUVAIS EXEMPLE
#

# Lorsqu'une commande presente dans un pipe echoue, sans precaution particuliere, le code retour de cette commande est perdu

user=www-data
ps -u $user elf | grep APACHE_PID_FILE | awk '{print $17}' | cut -d= -f2 | head -1
echo "code retour pipe=$?"

user=apache
ps -u $user elf | grep APACHE_PID_FILE | awk '{print $17}' | cut -d= -f2 | head -1
echo "code retour pipe=$?"

grep tar: $LOGFILE |tee $ERRORFILE

# Of the next 3 lines, only the 2nd should generate a violation
printf "| %-14s|%5s|%4s|%1s| %-19s| %s\n" DATE BANDE POS. N PARTITION FICHIER >>/tmp/results.$$
printf "| %-20s:\t\t%6d (MB)\n" $MACHINE_S $TAILLE | tee -a $ENTETE_FILE
sed -i 's|/usr/X11R6|'"${rep_inst_elec}"/'misc|' ./gen/configure

case $suer in
            # option T non supportee en dessous de solaris 8
            5.4|5.5|5.5.1|5.6|5.7)
              DUMP_OPTIONS="ulaf"
              cat "$LOGFILE" | grep "error";
            ;;
            *)
              DUMP_OPTIONS="ulTaf 72h"
            ;;
esac
