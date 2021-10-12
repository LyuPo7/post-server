#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -h host -p port -t token -i user_id -d description"
   echo -e "\t-h Host name"
   echo -e "\t-p Port number"
   echo -e "\t-t User's token"
   echo -e "\t-i User's id"
   echo -e "\t-d Author's description"
   exit 1 # Exit script after printing help
}

while getopts "h:p:t:i:d:" opt
do
   case "$opt" in
      h ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      t ) token="$OPTARG" ;;
      i ) id="$OPTARG" ;;
      d ) description="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

opt="token=$token&id=$id&description=$description"
url=http://$host:$port/createAuthor?$opt
curl ${url}