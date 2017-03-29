#!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.EXIT"
echo "Fichier OK de TU"
echo "------------------------------------------"

function getopts_internal()
{
   echo "Insert a number between 1 to 10"
   read num
   if [ $num == 1 ]
   then
		echo "You choose the minimum"
   elif [ $num == 10 ]
   then
		echo "You choose the maximum"
   else
		echo "You choose $num"
   fi
   return 
}

getopts_internal

