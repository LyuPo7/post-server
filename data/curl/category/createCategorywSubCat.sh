#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -h host -p port -t token -n title -s subcategory"
   echo -e "\t-h Host name"
   echo -e "\t-p Port number"
   echo -e "\t-t User's token"
   echo -e "\t-n Category's title"
   echo -e "\t-s SubCategory's title"
   exit 1 # Exit script after printing help
}

while getopts "h:p:t:n:s:" opt
do
   case "$opt" in
      h ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      t ) token="$OPTARG" ;;
      n ) title="$OPTARG" ;;
      s ) subTitle="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

opt="token=$token&title=$title&subcategory=$subTitle"
url=http://$host:$port/createCategory?$opt
curl ${url}