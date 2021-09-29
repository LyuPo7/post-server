#!/bin/bash

#var to store output file name
outputFile="data/curl/req/category/category.sh"

#clear output file before writing
echo "#!/bin/bash" > $outputFile
echo "### Categories" >> $outputFile

# title
title[1]="sport"
title[2]="crossfit"
title[3]="box"



#token
token="2a874271-f5e5-451b-bb9f-fe6a281414bf"

# getCategories
echo "## getCategories" >> $outputFile
echo "curl \"http://localhost:3000/getCategories?token=$token\"" >> $outputFile

# createCategory wo subCategory
echo "## createCategory" >> $outputFile
for i in 1 2 3
do
  echo "curl \"http://localhost:3000/createCategory?title=${title[$i]}&token=$token\"" >> $outputFile
done

# createCategory with subCategory
echo "## createCategory" >> $outputFile
for i in 1 2 3
do
  echo "curl \"http://localhost:3000/createCategory?title=${title[$i]}&subcategory=${title[1]}&token=$token\"" >> $outputFile
done