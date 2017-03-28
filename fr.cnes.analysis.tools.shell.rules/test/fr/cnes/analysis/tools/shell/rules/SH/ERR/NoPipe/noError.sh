#!/bin/bash

echo "------------------"
echo "- SH.ERR.NoPipe  -"
echo "-    OK          -"
echo "------------------"

#set -x
#set -e

#
# --- BON EXEMPLE
#

# Lorsqu'une commande dans un pipe echoue, sans precaution particuliere, le code retour de cette commande est perdu
#    En positionnant l'option (bash) pipefail, on obtient le code de retour de la commande la plus a droite qui a echoue

# A double pipe (OR) should not generate a violation
if [[ "X""${RIMS_TYPE}" = "X""A" || "X""${RIMS_TYPE}" = "X""a" || "X""${RIMS_TYPE}" = "X""B" || "X""${RIMS_TYPE}" = "X""b" || "X""${RIMS_TYPE}" = "X""N" || "X""${RIMS_TYPE}" = "X""n" ]]
then
	out=${RIMS_DIR}/${FILE_OUT}_${CONS_3}
else
	out=${RIMS_DIR}/${FILE_OUT}
fi

set -o pipefail

user=www-data
ps -u $user elf | grep APACHE_PID_FILE | awk '{print $17}' | cut -d= -f2 | head -1
echo "code retour pipe=$?"

user=apache
ps -u $user elf | grep APACHE_PID_FILE | awk '{print $17}' | cut -d= -f2 | head -1
echo "code retour pipe=$?"
case $user in
            # version user
            5.4|5.5|5.5.1|5.6|5.7)
              DUMP_OPTIONS="ulaf"
            ;;
            *)
              DUMP_OPTIONS="ulTaf 72h"
            ;;
esac
