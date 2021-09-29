#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -t token"
   echo -e "\t-h Host name"
   echo -e "\t-p Port number"
   echo -e "\t-t User's token"
   exit 1 # Exit script after printing help
}

while getopts "h:p:t:" opt
do
   case "$opt" in
      h ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      t ) token="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

url=http://$host:$port/getTags?token=$token
curl ${url}