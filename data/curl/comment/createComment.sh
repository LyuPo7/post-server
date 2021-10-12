#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -h host -p port -t token -i post_id -c text"
   echo -e "\t-h Host name"
   echo -e "\t-p Port number"
   echo -e "\t-t User's token"
   echo -e "\t-i Post's id"
   echo -e "\t-c Comment's text"
   exit 1 # Exit script after printing help
}

while getopts "h:p:t:i:c:" opt
do
   case "$opt" in
      h ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      t ) token="$OPTARG" ;;
      i ) postId="$OPTARG" ;;
      c ) text="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

opt="token=$token&post_id=$postId&text=$text"
url=http://$host:$port/createComment?$opt
curl ${url}