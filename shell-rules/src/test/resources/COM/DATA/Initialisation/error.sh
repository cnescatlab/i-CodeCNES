#!/bin/bash
echo "------------------------------------------"
echo "COM.DATA.INITIALIZATION"
echo "Fichier KO de TU"
echo "------------------------------------------"

x=1
# the y variable is not initialized
z=$x+$y

function function1 ()
{
  b = 2
  # the c variable is not initialized
  a = $b+$c
}

# main - the y variable is not initialized

xx=$y

function2 ()
{
  # the b variable is not initialized
  a = $b
  # the y variable is not initialized
  d = $y
  
  function function3 ()
  {
    # the y variable is not initialized
    e = $y
  }
}

# the y variable is not initialized
yy=$y
