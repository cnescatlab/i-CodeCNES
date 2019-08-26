#!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.BOOLEANEXPRESSION"
echo "Fichier OK de TU"
echo "------------------------------------------"

echo "Do you play some intrument?"
read reponse

   if [ ${reponse} == "y" ] || [ ${reponse} == "Y" ] || [ ${reponse} == "yes" ]; then
      echo "Very good!"
   else 
	  echo "Time to learn!"
   fi 
 
