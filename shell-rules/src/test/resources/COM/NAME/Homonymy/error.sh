#!/bin/bash
echo "--------------------------"
echo "-   COM.NAME.Homonymy    -"
echo "--------------------------"

index=0
declare -i f

fonction ()
{
  local position=5
  echo "Variable locale 'position' dans la fonction : $position"

  echo "Variable globale 'index' dans la fonction : $index"
}

fonction_globale ()     
{
  local index=3
  echo 'Fontion globale'
  echo "Index : $index"
}

fonction

# erreur -> variable avec le meme qu'une fonctio
fonction_globale='x'

echo "Le valeur de la variable fonction_globale est: $fonction_globale"

# erreur -> variable global avec le meme nom que la locale
position=8
echo "Variable globar 'position' dehors la fonction $position"

function test1() 
{
   local varTest1=5
   function test2()
   {
      # erreur -> variable locale avec le meme nom que la globale
      local index=3
	  # erreur -> variable globale avec le meme nom que la locale de test1
      local varTest1=2
	  varTest2=5
   }
}

function test3()
if [ $a -eq 1 ] ; then
  # erreur -> variable locale avec le meme nom que la globale
  local index=5
  local varTest1=0
  # erreur -> variable locale avec le meme nom que la globale de test1
  local varTest2=3
fi 
 
function test4()
while [ $a -eq 1 ] ; do
  # erreur -> variable locale avec le meme nom que la globale
  local index=5
  for module in $myModulesInput
  do
    echo "test des ouvertures de même famille"
  done
  local varTest1=0
  # erreur -> variable locale avec le meme nom que la globale de test1
  local varTest2=3
  # erreur -> variable locale avec le meme nom que la globale du declare
  local f=5
fi 
 
set var=val #tcsh
_dir="test"

function test5() 
{
   # erreur -> variable locale avec le meme nom que la variable tcsh
   local var=5
   local _dir="fonction_test5"
}