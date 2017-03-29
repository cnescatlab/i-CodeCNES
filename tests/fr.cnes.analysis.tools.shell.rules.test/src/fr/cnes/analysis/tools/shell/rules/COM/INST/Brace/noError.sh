#!/bin/bash
echo "------------------------------------------"
echo "COM.INST.BRACE"
echo "Fichier OK de TU"
echo "------------------------------------------"

echo "Insert three numbers"
read a
read b
read c
echo "interface appel fonction ok : a=$a b=$b c=$c"

# calcul	
let y=($a+$b)*$c 
printf "(a+b)*c = $y"
