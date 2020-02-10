#!/bin/bash
echo "------------------------------------------"
echo "COM.INST.BRACE"
echo "Fichier KO de TU"
echo "------------------------------------------"

echo "Insert three numbers"
read a
read b
read c
echo "interface appel fonction ok : a=$a bx=$b c=$c"

# calcul
let y=$a+$b*$c 
echo "a+b*c = $y"
