#!/bin/bash
echo "------------------------------------------"
echo "COM.INST.LINE"
echo "Fichier KO de TU"
echo "------------------------------------------"

while :
do
	read -p "Saisir deux nombres ( - 1 pour terminer ) : " a b
	if [ $a -eq -1 ] ; then echo 1; else echo 0; fi
	total=$(( a + b )); echo "le resultat de l'addition de $a + $b = $total"
done


while getopts vhli:o:s:p: options; do

    case "$options" in
        s) file_size="$OPTARG" ;;
        i) input_dir="$OPTARG" ;;
        o) output_dir="$OPTARG" ;;
        p) file_pattern="$OPTARG" ;;
        v) verbose=on ;;
        h) usage ;;
        l) more $0; exit 1 ;;
       \?) echo "invalid argument, type -h for help"; exit 1 ;;

    esac

done