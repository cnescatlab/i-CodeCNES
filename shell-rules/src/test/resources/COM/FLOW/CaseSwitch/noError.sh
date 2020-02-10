#!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.CASESWITCH"
echo "Fichier OK de TU"
echo "------------------------------------------"

# --- Extraction des valeurs des parametres 
echo "What do you prefer: beach or moutain"
read res
case "$res" in
	 "beach")
		 echo "Let's go to surfing"
	 ;;
	 "montain" )
		 echo "Let's go to skiing"
	 ;;
	 *)
		 echo "No answer?"
	 ;;
esac 
