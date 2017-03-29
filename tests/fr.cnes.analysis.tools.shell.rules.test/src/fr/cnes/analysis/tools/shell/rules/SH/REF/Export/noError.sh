#!/bin/bash
#set -x
#set -v
echo "--------------------"
echo "- SH.REF.Export    -"
echo "-    OK            -"
echo "--------------------"

#
# --- Ce script appelle un sous-shell qui effectue lors de son init un 'source' afin de declarer un jeu de fonctions .
# --- La seule dependance entre shell principal et sous-shell est constituee par une variable exportee : MAX_SHELL_LEVEL
#

# --- Export d'une constante 
typeset -r MAX_SHELL_LEVEL=2
export MAX_SHELL_LEVEL

# --- Variables locales 
nom_script=$(basename $0)
echo "Variable exportee au $nom_script s'appelle MAX_SHELL_LEVEL"