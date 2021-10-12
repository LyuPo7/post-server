#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -t token -p port -h host -n new_title -o old_title"
   echo -e "\t-h Host name"
   echo -e "\t-p Port number"
   echo -e "\t-t User's token"
   echo -e "\n-n New Tag's title"
   echo -e "\n-o Old Tag's title"
   exit 1 # Exit script after printing help
}

while getopts "h:p:t:n:o:" opt
do
   case "$opt" in
      h ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      t ) token="$OPTARG" ;;
      n ) newTag="$OPTARG" ;;
      o ) oldTag="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

opt="new_title=$newTag&old_title=$oldTag&token=$token"
url=http://localhost:3000/editTag?$opt
curl ${url}
