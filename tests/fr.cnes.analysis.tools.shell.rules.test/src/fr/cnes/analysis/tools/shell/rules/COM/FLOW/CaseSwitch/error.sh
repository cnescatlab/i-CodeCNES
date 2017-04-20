#!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.CASESWITCH"
echo "Fichier KO de TU"
echo "------------------------------------------"

# --- Extraction des valeurs des parametres
echo "What do you prefer: beach or moutain"
read res
case "$res" in
 "beach")
   echo "Let's go to surfing"
 ;;
 "mountain" )
  echo "Let's go to skiing"
 ;;
esac
