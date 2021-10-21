#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -y host -p port -f first_name -l last_name -k password -n login"
   echo -e "\t-y Host name"
   echo -e "\t-p Port number"
   echo -e "\t-f User's first name"
   echo -e "\t-l User's last name"
   echo -e "\t-k User's password"
   echo -e "\t-n User's login"
   exit 1 # Exit script after printing help
}

while getopts "y:p:f:l:k:n:" opt
do
   case "$opt" in
      y ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      f ) fn="$OPTARG" ;;
      l ) ln="$OPTARG" ;;
      k ) pass="$OPTARG" ;;
      n ) login="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

if [ -z "${fn+x}" ]
then opt=""
else opt="first_name=$fn"
fi

if [ -z "${ln+x}" ]
then opt="$opt"
else opt="$opt&last_name=$ln"
fi

if [ -z "${login+x}" ]
then opt="$opt"
else opt="$opt&login=$login"
fi

if [ -z "${pass+x}" ]
then opt="$opt"
else opt="$opt&password=$pass"
fi

url=http://$host:$port/createUser?$opt
curl ${url}