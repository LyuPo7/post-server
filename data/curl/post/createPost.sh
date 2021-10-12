#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -h host -p port -t token -n title -b text -c category_id -m tag_ids"
   echo -e "\t-h Host name"
   echo -e "\t-p Port number"
   echo -e "\t-t User's token"
   echo -e "\t-n Post's title"
   echo -e "\t-b Post's text"
   echo -e "\t-c Post's category_id"
   echo -e "\t-m Post's tag_ids"
   exit 1 # Exit script after printing help
}

while getopts "h:p:t:n:b:c:m:" opt
do
   case "$opt" in
      h ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      t ) token="$OPTARG" ;;
      n ) title="$OPTARG" ;;
      b ) text="$OPTARG" ;;
      c ) categoryId="$OPTARG" ;;
      m ) tagIds="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

opt="token=$token&title=$title&text=$text&category_id=$categoryId&tag_ids=$tagIds"
url=http://$host:$port/createPost?$opt
curl ${url}