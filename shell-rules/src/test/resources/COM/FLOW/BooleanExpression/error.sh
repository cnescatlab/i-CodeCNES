#!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.BOOLEANEXPRESSION"
echo "Fichier KO de TU"
echo "------------------------------------------"

test_func()
{
   echo "Do you play guitar?"
   read reponse

   if [ ${reponse} == "N" ] || [ ${reponse} == "Non" ] || [ ${reponse} == "n" ] || [ ${reponse} == "non" ] || [ ${reponse} == "no" ] || [ ${reponse} == "NO" ] || [ ${reponse} == "No" ]; then
      echo "You said no :("
   fi 
   
   if [ ${reponse} == "Y" ] || [ ${reponse} == "Yes" ] || [ ${reponse} == "y" ] || [ ${reponse} == "yes" ]  || 
   	  [ ${reponse} == "si" ] || [ ${reponse} == "s" ] || [ ${reponse} == "S" ]; then
      echo "You said yes :)"
   fi 
}

function test_func2()
{
   echo "Do you play guitar?"
   read reponse

   if [ ${reponse} == "N" ] || [ ${reponse} == "Non" ] || [ ${reponse} == "n" ] || [ ${reponse} == "non" ] || [ ${reponse} == "no" ] || [ ${reponse} == "NO" ] || [ ${reponse} == "No" ]; then
      echo "You said no :("
   fi 
   
   if [ ${reponse} == "Y" ] || [ ${reponse} == "Yes" ] || [ ${reponse} == "y" ] || [ ${reponse} == "yes" ]  || 
   	  [ ${reponse} == "si" ] || [ ${reponse} == "s" ] || [ ${reponse} == "S" ]; then
      echo "You said yes :)"
   elif  [ ${reponse} == "rock" ] || [ ${reponse} == "ROCK" ] || [ ${reponse} == "r" ] || [ ${reponse} == "ro" ] || [ ${reponse} == "roc" ] || [ ${reponse} == "rocK" ] || [ ${reponse} == "rokc" ]; then 
   	  echo "You said rock guitar !"   	  
   fi 
}

   echo "Do you play guitar?"
   read reponse

   if [ ${reponse} == "N" ] || [ ${reponse} == "Non" ] || [ ${reponse} == "n" ] || [ ${reponse} == "non" ] || [ ${reponse} == "no" ] || [ ${reponse} == "NO" ] || [ ${reponse} == "No" ]; then
      echo "You said no :("
   fi 
   
   if [ ${reponse} == "Y" ] || [ ${reponse} == "Yes" ] || [ ${reponse} == "y" ] || [ ${reponse} == "yes" ]  || 
   	  [ ${reponse} == "si" ] || [ ${reponse} == "s" ] || [ ${reponse} == "S" ]; then
      echo "You said yes :)"
   fi 
