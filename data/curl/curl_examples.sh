## In this example:
# lyupo is in admin list (in config.json file)
# others are not in admin list (in config.json file)
# !!!! You need to change tokens in all curl to new value !!!!

# Create Admin User
user/createUser.sh -y localhost -p 3000 -f Lyuba -l Portnova -k Wtynehbjy -n lyupo

# Create Non-Admin Users
user/createUser.sh -y localhost -p 3000 -f Tom -l Richards -n tomRich -k 1010101
user/createUser.sh -y localhost -p 3000 -f Lyuba -l Portnova -n lyupo -k 1010101 # must return error (this user already exists)
user/createUser.sh -y localhost -p 3000 -f Ann -l Perry -n anni -k dFgh56THhf
user/createUser.sh -y localhost -p 3000 -f Frank -l Jerome -n frank@ -k 00000

# Get new token
account/login.sh -y localhost -p 3000 -k Wtynehbjy -n lyupo
account/login.sh -y localhost -p 3000 -k 00000 -n frank@ #"7a4707d7-fe83-4d75-be9d-e8a319f59e0e"

# Tags
tag/getTags.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 5

tag/createTag.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -n sport
tag/createTag.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -n crossfit
tag/createTag.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -n box
tag/createTag.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -n crossfit
tag/createTag.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -n news
tag/createTag.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -n football
tag/createTag.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -n football2

tag/removeTag.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -n football2

tag/editTag.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o news -n sport_news
tag/editTag.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o sport_news -n football

# Users
user/getUsers.sh -y localhost -p 3000 -t "7a4707d7-fe83-4d75-be9d-e8a319f59e0e" -o 2 -- not admin
user/getUsers.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 2 -- admin

user/setUserPhoto.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -l /home/lyupo/Descargas/dog.png
user/setUserPhoto.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -l /home/lyupo/Descargas/dog4.png
user/setUserPhoto.sh -y localhost -p 3000 -t "7a4707d7-fe83-4d75-be9d-e8a319f59e0e" -l /home/lyupo/Descargas/dog4.png

author/createAuthor.sh -y localhost -p 3000 -t "7a4707d7-fe83-4d75-be9d-e8a319f59e0e" -i 1 -d main%20author -- not admin
author/createAuthor.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -i 1 -d new%20author - admin

author/getAuthors.sh -y localhost -p 3000 -t "7a4707d7-fe83-4d75-be9d-e8a319f59e0e" -o 1 -- not admin
author/getAuthors.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 0 -- admin

user/removeUser.sh -y localhost -p 3000 -t "7a4707d7-fe83-4d75-be9d-e8a319f59e0e" -i 2 -- not admin
user/removeUser.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -i 1 -- admin

author/editAuthor.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -i 1 -d main%20author -- admin

author/removeAuthor.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -i 2

category/getCategories.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 0
category/createCategory.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -n box
category/createCategory.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -n sport
category/createCategory.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -n sport6 -s box
category/editCategory.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -i 6
category/editCategory.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -i 6 -n new
category/editCategory.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -i 6 -s new
category/editCategory.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -i 5 -s new
category/editCategory.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -i 5 -s sport -n newnew

comment/createComment.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -i 1 -c Hi!

draft/getDrafts.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 0
draft/createDraft.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -i 2 -b Text?
draft/editDraft.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -i 2 -b Text?!
draft/removeDraft.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -i 22
draft/publishDraft.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -i 22

post/getPosts.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 0
post/createPost.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -n First%20post? -b First%20post%text -c 2 -m [1]
post/removePost.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -i 11
post/setPostMainPhoto.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -i 1 -l /home/lyupo/Descargas/dog.png
post/setPostAddPhoto.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -i 1 -l /home/lyupo/Descargas/dog.png
post/getPosts.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 0 -s order_by_date
post/getPosts.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 0 -s order_by_category
post/getPosts.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 0 -s order_by_author
post/getPosts.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 0 -s order_by_photos
post/getPosts.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 0 -s order_by_date -f %Lad%
post/getPosts.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 0 -s order_by_date -e %po%
post/getPosts.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 0 -s order_by_date -a %po% -e %last%
post/getPosts.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 0 -s order_by_date -a %po% -e %text% -n [2]
post/getPosts.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 0 -s order_by_date -a %po% -e %text% -i [2]
post/getPosts.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 0 -s order_by_date -a %po% -e %text% -i %5B2,4%5D
post/getPosts.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 0 -s order_by_date -a %po% -e %text% -k %5B2,4%5D
post/getPosts.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 0 -s order_by_date -a %po% -e %text% -i %5B2,4%5D -l Lyuba%20Portnova
post/getPosts.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 0 -s order_by_date -a %po% -e %text% -i %5B2,4%5D -l Lyuba%20Portnova -q 10.10.21
post/getPosts.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 0 -s order_by_date -a %po% -e %text% -i %5B2,4%5D -l Lyuba%20Portnova -q 14.10.21
post/getPosts.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 0 -s order_by_date -a %po% -e %text% -i %5B2,4%5D -l Lyuba%20Portnova -q 14.10.21 -g 13.10.21
post/getPosts.sh -y localhost -p 3000 -t "32d1c72f-e962-48c5-9b32-5c386e6f0ec9" -o 0 -s order_by_date -a %po% -e %text% -i %5B2,4%5D -l Lyuba%20Portnova -q 14.10.21 -g 13.10.21 -j 15.10.21
