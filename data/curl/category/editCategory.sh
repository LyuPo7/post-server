#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -h host -p port -t token -i id -n title -s subcategory"
   echo -e "\t-h Host name"
   echo -e "\t-p Port number"
   echo -e "\t-t User's token"
   echo -e "\t-i Category's id"
   echo -e "\t-n New Category's title"
   echo -e "\t-s SubCategory's title"
   exit 1 # Exit script after printing help
}

while getopts "h:p:t:i:n:s:" opt
do
   case "$opt" in
      h ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      t ) token="$OPTARG" ;;
      i ) id="$OPTARG" ;;
      n ) title="$OPTARG" ;;
      s ) subTitle="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

if [ -z "${token+x}" ]
then opt=""
else opt="token=$token"
fi

if [ -z "${title+x}" ]
then opt="$opt"
else opt="$opt&title=$title"
fi

if [ -z "${subTitle+x}" ]
then opt="$opt"
else opt="$opt&subcategory=$subTitle"
fi

if [ -z "${id+x}" ]
then opt="$opt"
else opt="$opt&id=$id"
fi

url=http://$host:$port/editCategory?$opt
curl ${url}