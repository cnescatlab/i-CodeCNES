#!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.FilePath"
echo "Fichier OK de test"
echo "------------------------------------------"

if [[ -n $REP_FICH_CONF ]]; then
    echo "Le répertoire contenant le fichier de configuration est :
    $REP_FICH_CONF"
fi
