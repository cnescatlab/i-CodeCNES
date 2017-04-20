#!/bin/bash
echo "--------------------"
echo "- SH.MET.LimitAWK  -"
echo "-    KO            -"
echo "--------------------"

#!/bin/bash
#set -x
#set -v


sed -e 's/titi/test/g' \
-e 's/tutu/test/g' \
-e 's/tata/test/g' \
-e 's/tete/test/g' \
-e 's/tyty/test/g' \
-e 's/toto/test/g' \
>"${newFile}" 

cat $1 | sed "s/\t/ /g;s/  / /g;s/^ //;s/\r//g;s/a/A/g;s/e/E/g"

exit
