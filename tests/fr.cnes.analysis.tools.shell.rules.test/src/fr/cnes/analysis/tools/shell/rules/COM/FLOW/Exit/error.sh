!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.EXIT"
echo "Fichier KO de TU"
echo "------------------------------------------"

function getopts_internal()
{
   echo "Insert a number between 1 to 10"
   read num
   if [ "$num" -eq 1 ]
   then
		echo "You choose the minimum"
		return
   elif [ "$num" -eq 10 ]
   then
		echo "You choose the maximum"
		return
   else
		echo "You choose $num"
		return
   fi
   
}

getopts_external()
{
   echo "Insert a number between 10 to 20"
   read num
   if [ "$num" -eq 10 ]
   then
		echo "You choose the minimum"
		return
   elif [ "$num" -eq 20 ]
   then
		echo "You choose the maximum"
		return
   else
		echo "You choose $num"
		return
   fi
   
}

getopts_internal
getopts_external
