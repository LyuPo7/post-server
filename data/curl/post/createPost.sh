#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -y host -p port -t token -n title -b text -c category_id -m tag_ids"
   echo -e "\t-y Host name"
   echo -e "\t-p Port number"
   echo -e "\t-t User's token"
   echo -e "\t-n Post's title"
   echo -e "\t-b Post's text"
   echo -e "\t-c Post's category_id"
   echo -e "\t-m Post's tag_ids"
   exit 1 # Exit script after printing help
}

while getopts "y:p:t:n:b:c:m:" opt
do
   case "$opt" in
      y ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      t ) token="$OPTARG" ;;
      n ) title="$OPTARG" ;;
      b ) text="$OPTARG" ;;
      c ) categoryId="$OPTARG" ;;
      m ) tagIds="$OPTARG" ;;
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

if [ -z "${text+x}" ]
then opt="$opt"
else opt="$opt&text=$text"
fi

if [ -z "${categoryId+x}" ]
then opt="$opt"
else opt="$opt&category_id=$categoryId"
fi

if [ -z "${tagIds+x}" ]
then opt="$opt"
else opt="$opt&tag_ids=$tagIds"
fi

url=http://$host:$port/createPost?$opt
curl ${url}