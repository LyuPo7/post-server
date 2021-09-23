{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Post.DB.DBQuery where

import Post.DB.DBQuerySpec (Handle(..))
import qualified Post.DB.DBSpec as DBSpec
import qualified Post.Logger as Logger
import qualified Post.DB.Post as DBP
import qualified Post.DB.User as DBU
import qualified Post.DB.Author as DBA
import qualified Post.DB.Category as DBC
import qualified Post.DB.Tag as DBT
import qualified Post.DB.Draft as DBD
import qualified Post.DB.Photo as DBPh
import qualified Post.DB.Account as DBAc

withHandleIO :: Logger.Handle IO -> DBSpec.Handle IO -> DBSpec.Config -> (Handle IO -> IO a) -> IO a
withHandleIO logger dbh config f = do
  let handle = Handle {
    hLogger = logger,
    hDB = dbh,
    cDB = config,
  
    createPost = DBP.createPost dbh,
    getPosts = DBP.getPosts dbh,
    getPost = DBP.getPost dbh,
    removePost = DBP.removePost dbh,
    setPostMainPhoto = DBP.setPostMainPhoto dbh,
    setPostAddPhoto = DBP.setPostAddPhoto dbh,
    getPostMainPhoto = DBP.getPostMainPhoto dbh,
    getPostAddPhotos = DBP.getPostAddPhotos dbh,
    getPostComments = DBP.getPostComments dbh,
    getPostAuthorId = DBP.getPostAuthorId dbh,
    getPostCategoryId = DBP.getPostCategoryId dbh,
    getPostTagsIds = DBP.getPostTagsIds dbh,
    getPostDraftId = DBP.getPostDraftId dbh,
    createPostAuthorDep = DBP.createPostAuthorDep dbh,
    createPostCatDep = DBP.createPostCatDep dbh,
    createPostTagDep = DBP.createPostTagDep dbh,
    createPostDraftDep = DBP.createPostDraftDep dbh,
    removePostAuthorDep = DBP.removePostAuthorDep dbh,
    removePostCatDep = DBP.removePostCatDep dbh,
    removePostTagDep = DBP.removePostTagDep dbh,
    newPost = DBP.newPost dbh,
  
    createUser = DBU.createUser dbh,
    getUsers = DBU.getUsers dbh,
    getUser = DBU.getUser dbh,
    removeUser = DBU.removeUser dbh,
    setUserPhoto = DBU.setUserPhoto dbh,
    getUserPhoto = DBU.getUserPhoto dbh,
    newUser = DBU.newUser dbh,
  
    createAuthor = DBA.createAuthor dbh,
    createAuthorUserDep = DBA.createAuthorUserDep dbh,
    getAuthors = DBA.getAuthors dbh,
    getAuthor = DBA.getAuthor dbh,
    getAuthorUserId = DBA.getAuthorUserId dbh,
    removeAuthor = DBA.removeAuthor dbh,
    removeAuthorUserDep = DBA.removeAuthorUserDep dbh,
    editAuthor = DBA.editAuthor dbh,
    newAuthor = DBA.newAuthor dbh,
  
    createCat = DBC.createCat dbh,
    checkCatExists = DBC.checkCatExists dbh,
    getCats = DBC.getCats dbh,
    getCat = DBC.getCat dbh,
    getCatwSub = DBC.getCatwSub dbh,
    removeCat = DBC.removeCat dbh,
    editCat = DBC.editCat dbh,
    newCat = DBC.newCat dbh,
    newCatNull = DBC.newCatNull,
    getSub = DBC.getSub dbh,
  
    createTag = DBT.createTag dbh,
    getAllTags = DBT.getAllTags dbh,
    getTags = DBT.getTags dbh,
    removeTag = DBT.removeTag dbh,
    editTag = DBT.editTag dbh,
    newTag = DBT.newTag,

    createDraft = DBD.createDraft dbh,
    getDraft = DBD.getDraft dbh,
    removeDraft = DBD.removeDraft dbh,
    removePostDraftDep = DBD.removePostDraftDep dbh,
    editDraft = DBD.editDraft dbh,
    publishDraft = DBD.publishDraft dbh,
    getDraftText = DBD.getDraftText dbh,
    newDraft = DBD.newDraft,

    savePhoto = DBPh.savePhoto dbh,
    getPhoto = DBPh.getPhoto dbh,
    newPhoto = DBPh.newPhoto,

    getToken = DBAc.getToken dbh,
    checkAdminPerm = DBAc.checkAdminPerm dbh,
    checkUserPerm = DBAc.checkUserPerm dbh,
    checkAuthorWritePerm = DBAc.checkAuthorWritePerm dbh,
    checkAuthorReadPerm = DBAc.checkAuthorReadPerm dbh,
    getUserId = DBAc.getUserId dbh,
    getAuthorId = DBAc.getAuthorId dbh
  }
  f handle