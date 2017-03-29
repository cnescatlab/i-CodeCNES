#!/bin/bash
echo "--------------------------"
echo "-   COM.NAME.Homonymy    -"
echo "--------------------------"

index=0

fonction ()
{
  local position_local=5
  echo "Variable locale 'position' dans la fonction : $position_local"

  echo "Variable globale 'index' dans la fonction : $index"
}

fonction_globale ()     
{
  echo 'Fontion globale'
}

fonction

fonction_globale

echo "Le valeur de la variable fonction_globale est: $fonction_globale"


position_global=8
echo "Variable globar 'position' dehors la fonction $position_global"
