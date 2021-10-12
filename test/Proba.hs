module Test where

user0 = User {
  user_id = 1010, -- Unique identifier for this User.
  user_isAdmin = False, -- True, if this user is a Admin.
  user_firstName = "Jhon", -- User's first name.
  user_lastName = "Thomson", -- User's last name.
  user_photo = Nothing
}

author0 = Author {
  author_user = user0, -- User.
  author_description = "Adventures" -- Author's description.
}

cat0 = Category {
  category_id = 0, -- Unique identifier for this Category.
  category_title = "Sport", -- Title of Category.
  category_subcategory = Just cat1
}

cat1 = Category {
  category_id = 1, -- Unique identifier for this Category.
  category_title = "Crossfit", -- Title of Category.
  category_subcategory = Just cat2
}

cat2 = Category {
  category_id = 1, -- Unique identifier for this Category.
  category_title = "Crossfit Games", -- Title of Category.
  category_subcategory = Nothing
}

tag0 = Tag {
  tag_id = 0,
  tag_title = "sport"
}

tag1 = Tag {
  tag_id = 1,
  tag_title = "crossfit"
}

post0 = Post {
  post_id = 0, -- Unique identifier for this Announcement.
  post_author = author0 , -- Author of Announcement.
  post_title = "Crossfit Games 2021", -- Title of Announcement.
  post_createdAt = "03.08.21", -- Date when the Announcement was created.
  post_category = cat0, -- Category of Announcement.
  post_tags = Just [tag0,tag1], -- Array of Tag of Announcement.
  post_text = "Yesterday was the last day of competitions of the 2021 Crossfit Games", -- Text of Announcement.
  post_mainPhoto = Nothing, -- Main Photo of Announcement.
  post_addPhotos = Nothing, -- Array of Additional Photos of Announcement.
  post_comments = Just [com0, com1] -- Array of Comments of Announcement.
}

com0 = Comment {
  comment_id = 2, -- Unique identifier for this Comment.
  comment_text = "Happy!"
}

com1 = Comment {
  comment_id = 3, -- Unique identifier for this Comment.
  comment_text = "Very Happy!"
}

draft0 = Draft {
  draft_id = 0 , -- Unique identifier for this Draft.
  draft_text = "No text"
}

user0 = User {
  user_id = 1010, -- Unique identifier for this User.
  user_isAdmin = False, -- True, if this user is a Admin.
  user_firstName = "Jhon", -- User's first name.
  user_lastName = "Thomson", -- User's last name.
  user_photo = Nothing
}

author0 = Author {
  author_user = user0, -- User.
  author_description = "Adventures" -- Author's description.
}

cat0 = Category {
  category_id = 0, -- Unique identifier for this Category.
  category_title = "Sport", -- Title of Category.
  category_subcategory = Just cat1
}

cat1 = Category {
  category_id = 1, -- Unique identifier for this Category.
  category_title = "Crossfit", -- Title of Category.
  category_subcategory = Just cat2
}

cat2 = Category {
  category_id = 1, -- Unique identifier for this Category.
  category_title = "Crossfit Games", -- Title of Category.
  category_subcategory = Nothing
}

tag0 = Tag {
  tag_id = 0,
  tag_title = "sport"
}

tag1 = Tag {
  tag_id = 1,
  tag_title = "crossfit"
}

post0 = Post {
  post_id = 0, -- Unique identifier for this Announcement.
  post_author = author0 , -- Author of Announcement.
  post_title = "Crossfit Games 2021", -- Title of Announcement.
  post_createdAt = "03.08.21", -- Date when the Announcement was created.
  post_category = cat0, -- Category of Announcement.
  post_tags = Just [tag0,tag1], -- Array of Tag of Announcement.
  post_text = "Yesterday was the last day of competitions of the 2021 Crossfit Games", -- Text of Announcement.
  post_mainPhoto = Nothing, -- Main Photo of Announcement.
  post_addPhotos = Nothing, -- Array of Additional Photos of Announcement.
  post_comments = Just [com0, com1] -- Array of Comments of Announcement.
}

com0 = Comment {
  comment_id = 2, -- Unique identifier for this Comment.
  comment_text = "Happy!"
}

com1 = Comment {
  comment_id = 3, -- Unique identifier for this Comment.
  comment_text = "Very Happy!"
}

draft0 = Draft {
  draft_id = 0 , -- Unique identifier for this Draft.
  draft_text = "No text"
}