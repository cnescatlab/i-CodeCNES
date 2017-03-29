#!/bin/bash
#set -x
#set -v
echo "--------------------"
echo "- SH.REF.Export    -"
echo "-    OK            -"
echo "--------------------"

verifie_seuil_shell()
{
	echo "Do nothing"
}

# --- Export d'une constante 
typeset -r MAX_SHELL_LEVEL=2
export MAX_SHELL_LEVEL
# --- Export d'une certaine fonctions 
export -f verifie_seuil_shell

# --- Variables locales 
nom_script=$(basename $0)
echo "Variable exportee au $nom_script s'appelle MAX_SHELL_LEVEL"
echo "Founction exportee au $nom_script s'appelle verifie_seuil_shell"