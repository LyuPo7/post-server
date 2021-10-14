#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -h host -p port -t token -o order -f find_in_title -e find_in_text -a find -c category_id -n [tag_id] -i tag__in -k tag__all -l author -q created_at -g created_at__gt -j created_at__gt"
   echo -e "\t-h Host name"
   echo -e "\t-p Port number"
   echo -e "\t-t User's token"
   echo -e "\t-o Order: one of ['order_by_date', 'order_by_author', 'order_by_category', 'order_by_photo']"
   echo -e "\t-f Find string for search in Post's Title"
   echo -e "\t-e Find string for search in Post's Text"
   echo -e "\t-a Find string for search in (Post's Title and Post's Text) & (Author's First name and Author's Last name) & (Category's Title) & (Tag's Title)"
   echo -e "\t-c Search Posts with exact Category's Id"
   echo -e "\t-c Search Posts with exact Tag's Id [tag_id] (lenght [tag_id] == 1)"
   echo -e "\t-i Search Posts with Tag's Id in [tag_id] - tag__in"
   echo -e "\t-k Search Posts with All Tag's Id in [tag_id] - tag__all"
   echo -e "\t-l Search Posts by Author name"
   echo -e "\t-q Search Posts by date of creation - 'created_at'"
   echo -e "\t-g Search Posts created later than specified date - 'created_at__gt'"
   echo -e "\t-j Search Posts created earlier than specified date - 'created_at__lt'"
   exit 1 # Exit script after printing help
}

while getopts "h:p:t:o:f:e:a:c:n:i:k:l:q:g:j:" opt
do
   case "$opt" in
      h ) host="$OPTARG" ;;
      p ) port="$OPTARG" ;;
      t ) token="$OPTARG" ;;
      o ) order="$OPTARG" ;;
      f ) findInTitle="$OPTARG" ;;
      e ) findInText="$OPTARG" ;;
      a ) find="$OPTARG" ;;
      c ) category="$OPTARG" ;;
      n ) tag="$OPTARG" ;;
      i ) tagIn="$OPTARG" ;;
      k ) tagAll="$OPTARG" ;;
      l ) author="$OPTARG" ;;
      q ) createdAt="$OPTARG" ;;
      g ) createdAtGT="$OPTARG" ;;
      j ) createdAtLT="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

if [ -z "${order+x}" ]
then opt="token=$token"
else opt="token=$token&$order=true"
fi

if [ -z "${findInTitle+x}" ]
then opt="$opt"
else opt="$opt&find_in_title=$findInTitle"
fi

if [ -z "${findInText+x}" ]
then opt="$opt"
else opt="$opt&find_in_text=$findInText"
fi

if [ -z "${find+x}" ]
then opt="$opt"
else opt="$opt&find=$find"
fi

if [ -z "${category+x}" ]
then opt="$opt"
else opt="$opt&category=$category"
fi

if [ -z "${tag+x}" ]
then opt="$opt"
else opt="$opt&tag=$tag"
fi

if [ -z "${tagIn+x}" ]
then opt="$opt"
else opt="$opt&tag__in=$tagIn"
fi

if [ -z "${tagAll+x}" ]
then opt="$opt"
else opt="$opt&tag__all=$tagAll"
fi

if [ -z "${author+x}" ]
then opt="$opt"
else opt="$opt&author=$author"
fi

if [ -z "${createdAt+x}" ]
then opt="$opt"
else opt="$opt&created_at=$createdAt"
fi

if [ -z "${createdAtGT+x}" ]
then opt="$opt"
else opt="$opt&created_at__gt=$createdAtGT"
fi

if [ -z "${createdAtLT+x}" ]
then opt="$opt"
else opt="$opt&created_at__lt=$createdAtLT"
fi

url=http://$host:$port/getPosts?$opt
curl ${url}