#!/bin/bash

echo "-------------------"
echo "- SH.DATA.Integer -"
echo "-       OK        -"
echo "-------------------"

echo "Si ... Rayon=2"
# --- On fixe le type de la variable 'rayon' qui est un entier 
typeset -i rayon=2

# --- On utilise let pour le calcul 
let diametre=$rayon*2
echo "Diametre=$diametre"
