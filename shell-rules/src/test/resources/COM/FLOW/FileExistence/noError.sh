#!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.FILEEXISTENCE"
echo "Fichier OK de TU"
echo "------------------------------------------"

# Si le fichier n'existe pas, on le cree
if [ ! -f $fichier ]
then
   touch $fichier
   echo "coucou" > $fichier
   date > $fichier
fi
rm -f $fichier

if [ -s $dirbdtle/bd_objet/$n1/$n2/RCS.txt ] ; then   
     echo "test" > $dirbdtle/bd_objet/$n1/$n2/RCS.txt 
fi

if [ -e ${TAPE_DEV_REP}/${DATE_EXE}_${TYPE_ACTION}_${NIVEAU_SAUVEGARDE}_${MACHINE_S}.tar.gz ] ; then   
   cat "${TAPE_DEV_REP}/${DATE_EXE}_${TYPE_ACTION}_${NIVEAU_SAUVEGARDE}_${MACHINE_S}.tar.gz" | Execute_Commande_C_Retour -d "cat - > ${TAPE_DEV_REP}/${DATE_EXE}_${TYPE_ACTION}_${NIVEAU_SAUVEGARDE}_${MACHINE_S}.tar.gz"
fi

if [ -e $elec_export/fcf/machines.txt ] ; then   
   echo "${nom_machine} ${nb_proc}" > "${elec_export}"/fcf/machines.txt
fi

while (( $STOP >= $START ))
    do
        echo "STOP --> START"
        echo " => begin next"
    done
	
grep -v "P" $fichier | awk '($9>0.0) {print $0}'
