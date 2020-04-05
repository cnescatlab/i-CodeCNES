!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.ACTIVEWAIT"
echo "Fichier OK de test"
echo "------------------------------------------"

echo "Iterate 5 times"
i=1
while [ $i -le 5 ]
do
   echo "Iteration num $i"
   i=$(($i+1))
done

while IFS= read -r line
do
   ## take some action on $line
  echo "$line"
done < <(ps -edf)

while IFS= read line
do
   ## take some action on $line
  echo "$line"
done < <(ps -edf)

while read line
do
   ## take some action on $line
  echo "$line"
done < <(ps -edf)