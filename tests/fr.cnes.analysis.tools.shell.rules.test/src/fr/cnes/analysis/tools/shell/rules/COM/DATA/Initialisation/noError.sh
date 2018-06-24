#!/bin/bash
echo "------------------------------------------"
echo "COM.DATA.INITIALISATION"
echo "Fichier OK de TU"
echo "------------------------------------------"

x=1
y=2
z=$x+$y

function function1 ()
{
  b = 2
  c = 3
  a = $b+$c
}

# main
xx=y

function2 ()
{
  b = 2
  a = $b+$xx
  function function3 ()
  {
    e = $a
  }
  
  
}

yy=$xx