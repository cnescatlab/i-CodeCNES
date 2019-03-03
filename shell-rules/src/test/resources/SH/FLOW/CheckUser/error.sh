#!/bin/bash

repertoire=/tmp
script=$(basename $0)

current_user="$(id -u -n)"

echo "! Execution avec les droits de $current_user !"
echo "- Suppression de tous les fichiers -"
echo "- rm -rf /tmp/*" 
rm -rf /tmp/*
