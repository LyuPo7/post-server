#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -t token -p port -y host -n new_title -o old_title"
   echo -e "\t-y Host name"
   echo -e "\t-p Port number"
   echo -e "\t-t User's token"
   echo -e "\n-n New Tag's title"
   echo -e "\n-o Old Tag's title"
   exit 1 # Exit script after printing help
}

while getopts "y:p:t:n:o:" opt
do
   case "$opt" in
      y ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      t ) token="$OPTARG" ;;
      n ) newTag="$OPTARG" ;;
      o ) oldTag="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

if [ -z "${token+x}" ]
then opt=""
else opt="token=$token"
fi

if [ -z "${newTag+x}" ]
then opt="$opt"
else opt="$opt&new_title=$newTag"
fi

if [ -z "${oldTag+x}" ]
then opt="$opt"
else opt="$opt&old_title=$oldTag"
fi

url=http://localhost:3000/editTag?$opt
curl ${url}
