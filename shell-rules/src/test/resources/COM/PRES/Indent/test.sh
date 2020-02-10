#!/bin/sh
while getopts "p:hs:" arg;do
case $arg in
p) if test  $# -eq 3 && [[ "$2" =~ [2][0][1-2][0-9][0-1][0-9][0-3][0-9] ]] && [[ "$3" =~ [2][0][1-2][0-9][0-1][0-9][0-3][0-9] ]] ; then
         
                START=$(date -d ${2:0:4}-${2:4:2}-${2:6:2} "+%y%m%d")		
                STOP=$(date -d ${3:0:4}-${3:4:2}-${3:6:2} "+%y%m%d")		
                typeset -i nbDays=0
                        if [ $STOP -ge $START ]							
                                then
                                while (( $STOP >= $START ))					
                                        do
                                                tabDate[$nbDays]=$(date +%Y%m%d -d "$START")	#built table containing every day between Start and Stop Date
                                                let nbDays++					#table compteur +1
                                                START=$(date +%y%m%d -d "$START + 1 day")	#Day +1
                                        done
                                tarList ${tabDate[@]} 						# Send builted table with date to tarList function as argument
                        fi
                else
                echo -e "Error--Wrong usage, please check usage (-h)\n"				
                fi
		exit;;
s)echo "Not coded yet"
exit;;
:)echo "Error--Option -$OPTARG need arguments " ; exit ;;
h)display_usage
exit;; 
/?)exit;;
esac
done

case $1 in

[2][0][1-2][0-9][0-1][0-9][0-3][0-9] )	for parameters in "$@"								
					do
						if ! [[ "$parameters" =~ [2][0][1-2][0-9][0-1][0-9][0-3][0-9] ]]; then
							echo -e "ERROR--Wrong format, please check usage (-h)\n"
							exit
						fi
					done
					tarList "$@";;									
*) echo -e "ERROR--Wrong entry, please check usage (-h)\n";;							
esac