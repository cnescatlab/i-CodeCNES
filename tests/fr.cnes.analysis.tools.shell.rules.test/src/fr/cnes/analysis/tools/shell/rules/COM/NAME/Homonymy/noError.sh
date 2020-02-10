#!/bin/bash
echo "--------------------------"
echo "-   COM.NAME.Homonymy    -"
echo "--------------------------"

index=0
_dir="test"
c=1

fonction ()
{
  local position_local=5
  echo "Variable locale 'position' dans la fonction : $position_local"
  position_local=6
  index_=3
  local test_var="test"
  echo $test_var
  local _c=5
  _dir="test_fonction"
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
