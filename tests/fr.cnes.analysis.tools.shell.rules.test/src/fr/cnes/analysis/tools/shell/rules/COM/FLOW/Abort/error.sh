#!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.ABORT"
echo "Fichier KO de TU"
echo "------------------------------------------"

# Find process with name ruby
PID=`ps aux | grep -i "ruby" | cut -d ' ' -f 1`

#kill process  with id in PID variable
if [[ $PID -eq 10 ]]
then
  kill $PID
  killall -9 $PID
fi

# kill process directly in one line
#ps -ef | grep "ruby" | awk '{print $2}' | xargs kill

function testFunction()
{
	kill $PID
}

killall -9 $PID

