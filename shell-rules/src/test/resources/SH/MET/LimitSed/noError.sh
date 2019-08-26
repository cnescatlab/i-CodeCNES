#!/bin/bash
echo "--------------------"
echo "- SH.MET.LimitSED  -"
echo "-    OK            -"
echo "--------------------"

#!/bin/bash
#set -x
#set -v


$ sed -e 's/tata/test/g' >"${newFile}" 
$ sed -e 's/tete/test/g' >"${newFile}" 
$ sed -e 's/titi/test/g' >"${newFile}" 
$ sed -e 's/toto/test/g' >"${newFile}" 
$ sed -e 's/tutu/test/g' >"${newFile}" 
$ sed -e 's/tyty/test/g' >"${newFile}" 

cat $1 | sed "s/\t/ /g;s/  / /g;s/^ //;s/\r//g" \
| awk 'BEGIN { FS = " "; StartType = 0; StartEnum = 0; typeName=""; }
		$1 == "typedef" && $2 == "enum"	{ 
			StartType = 1			
			if ( $3 ~ /^[0-9A-Z_][0-9a-zA-Z_]*$/ ) { typeName=$3 }
			if ( $4 ~ /^{$/ ) { StartEnum=1 }
			print $1 " " $2 ":" $3
		}
		$1 ~ /^\/\*$/ { print "commentaire... " $0 }
		$1 ~ /^{$/ { if ( StartType == 1 ) { StartEnum=1 ; print "debut: " $1 } }
		$1 ~ /^}$/ { if ( StartEnum == 1 ) { StartType=0 ; StartEnum=0 ; print "fin: " $1 } }
		{ print "Verif: [" $1 "]" } '
		exit
		$1 ~ /^[0-9A-Z_][0-9a-zA-Z_]*$/ { if (StartEnum == 1) print typeName ":" $1 }
		$1 ~ /^[0-9A-Z_][0-9a-zA-Z_]*,$/ { if (StartEnum == 1) print typeName ":" $1 }
		END { }'


cat $1 | sed "s/\t/ /g;s/  / /g;s/^ //;s/\r//g"

exit
