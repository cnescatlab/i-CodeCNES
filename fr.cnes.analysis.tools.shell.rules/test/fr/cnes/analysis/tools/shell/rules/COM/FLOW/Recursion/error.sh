#!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.RECURSION"
echo "Fichier KO de TU"
echo "------------------------------------------"

recursive_directe()
{
   echo "Insert num between 1 to 10: magic num=5"
   read num
   if [ $num -eq 5 ]
   then
	echo "Bingo!"
   else 
	echo "Try again"
	recursive_directe
   fi 
}

function recursive_indirecte1()
{
	echo "Insert num between 1 to 10: magin num=1"
	recursive_indirecte2
}
function recursive_indirecte2()
{
	read num
	if [ $num -eq 1 ]
    then
		echo "Bingo!"
	else 
		recursive_indirecte3
	fi 
}
function recursive_indirecte3()
{
	echo "Try again"
	recursive_indirecte1
}

recursive_directe
recursive_indirecte1
