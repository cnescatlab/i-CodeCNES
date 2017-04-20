#!/bin/bash
echo "------------------------------------------"
echo "COM.INST.BOOLEANNEGATION"
echo "Fichier KO de TU"
echo "------------------------------------------"

echo "Insert num:"
read a
if ! [[ ! $a == 0 && ! $a == 1 ]]; then
   echo "You insert 0 or 1"
fi

if ! [$a -ne 0] ; then
   echo "You insert 0 or 1"
fi

if ! [[ $a == 0] -a ! [$a == 1 ]]
then 
	echo "You insert 0 or 1"
fi

if ! [[ $a == 0] -o  [$a != 1 ]]
then 
	echo "You insert 0 or 1"
fi

if ! [[ ! ( "$a" == 0 ) ]]
then 
	echo "You insert 0 or 1"
fi

test1() {
   if ! [[ ( "$a" == 1 || ! "$a" == 0 ) ]]
   then 
      echo "You insert 0 or 1"
   fi
}

function test() {
   if ! { [ ! "$a" == 0 ] || [ "$a" == 1 ] ; }
   then 
      echo "You insert 0 or 1"
   fi
}

if ! { [ "$a" == 1 ] || [ ! "$a" == 0 ] ; }
then 
	echo "You insert 0 or 1"
fi