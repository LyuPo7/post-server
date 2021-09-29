#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -t token -p port -h host -n tag"
   echo -e "\t-h Host name"
   echo -e "\t-p Port number"
   echo -e "\t-t User's token"
   echo -e "\n-n Tag's title"
   exit 1 # Exit script after printing help
}

while getopts "h:p:t:n:" opt
do
   case "$opt" in
      h ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      t ) token="$OPTARG" ;;
      n ) tag="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

opt="token=$token&title=$tag"
url=http://localhost:3000/removeTag?$opt
curl ${url}