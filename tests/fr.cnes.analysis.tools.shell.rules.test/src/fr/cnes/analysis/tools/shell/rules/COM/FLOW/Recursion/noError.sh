#!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.RECURSION"
echo "Fichier OK de TU"
echo "------------------------------------------"

function non_recursive()
{
   echo "Insert num between 1 to 10: magic num=5"
   read num
   while [ $num -ne 5 ]
   do
		echo "Try again... Insert num between 1 to 10"
		read num
   done 
   echo ${#pythonBinTests[cov]}
}

non_recursive

