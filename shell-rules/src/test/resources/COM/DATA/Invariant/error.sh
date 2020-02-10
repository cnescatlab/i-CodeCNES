#!/bin/bash
echo "------------------------------------------"
echo "COM.DATA.INVARIANT"
echo "Fichier KO de TU"
echo "------------------------------------------"

# Exemple avec un nombre
x=2
y=$((3+$x))
echo "Mauvais exemple avec un nombre : "${y}

# Exemple avec une chaine de caractère
chaine="Mauvais exemple "
chaine_test="$chaine ko"
echo "Mauvais exemple avec une chaîne de caractère : "${chaine_test}
chaine_test="Nouvelle valeur"
echo "------------------------------------------"

function fonction ()
{
  local position=5
  echo "Variable locale 'position' dans la fonction : $position"
}

fonction2 ()
{
  typeset pos=3
}

position2=5;
