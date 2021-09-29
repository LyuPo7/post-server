#!/bin/bash

#var to store output file name
outputFile="data/curl/req/tag/tag.sh"

#clear output file before writing
echo "#!/bin/bash" > $outputFile
echo "### Tags" >> $outputFile

#token
token="2a874271-f5e5-451b-bb9f-fe6a281414bf"

# tags
tag1="sport"
tag2="crossfit"
tag3="power"
tag4="football"
tag5="nutrition"
tag6="wod"

tagnew1="sport2"
tagnew2="crossfit2"
tagnew3="power2"
tagnew4="football2"
tagnew5="nutrition2"
tagnew6="wod2"

# getTags
echo "## getTags" >> $outputFile
echo "curl \"http://localhost:3000/getTags?token=$token\"" >> $outputFile

# createTag
echo "## createTag" >> $outputFile
for tag in $tag1 $tag2 $tag3 $tag4 $tag5 $tag6
do
  echo "curl \"http://localhost:3000/createTag?title=$tag&token=$token\"" >> $outputFile
done

# getTags
echo "## getTags" >> $outputFile
echo "curl \"http://localhost:3000/getTags?token=$token\"" >> $outputFile

# editTag
echo "## editTag" >> $outputFile
for tag in $tag1 $tag2 $tag3 $tag4 $tag5 $tag6
do
  for newtag in $tagnew1 $tagnew2 $tagnew3 $tagnew4 $tagnew5 $tagnew6
  do
    echo "curl \"http://localhost:3000/editTag?new_title=$newtag&old_title=$tag&token=$token\"" >> $outputFile
  done
done

# getTags
echo "## getTags" >> $outputFile
echo "curl \"http://localhost:3000/getTags?token=$token\"" >> $outputFile

# removeTag
echo "## removeTag" >> $outputFile
for tag in $tagnew1 $tagnew2 $tagnew3 $tagnew4 $tagnew5 $tagnew6
do
  echo "curl \"http://localhost:3000/removeTag?title=$tag&token=$token\"" >> $outputFile
done