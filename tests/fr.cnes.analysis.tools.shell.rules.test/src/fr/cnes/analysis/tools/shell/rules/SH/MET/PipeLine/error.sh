#!/bin/bash
echo "--------------------"
echo "- SH.MET.PipeLine  -"
echo "-    KO            -"
echo "--------------------"


 ls -l | sed -e "s/[eiou]/a/g"  

compress=1

function test ()
{
   test1 ()
      if [$compress]; then
        tar tvf ./archive.tar | awk '{print $3, $6}' | egrep '\.c$' | sort -nr | head -1
      fi
   test1
    ls -l | sed -e "s/[eiou]/a/g"  
}