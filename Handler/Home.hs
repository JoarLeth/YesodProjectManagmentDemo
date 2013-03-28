{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import Yesod.Form.Jquery

instance YesodJquery App

getHomeR :: Handler RepHtml
getHomeR = do
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

projectForm :: Form Project
projectForm = renderBootstrap $ Project
    <$> areq textField "Name" Nothing
    <*> areq textareaField "Description" Nothing
    <*> areq (jqueryDayField def) "StartDate" Nothing
    <*> areq (jqueryDayField def) "EndDate" Nothing


getProjectsR = do
    projects <- runDB $ selectList [] [Asc ProjectName]
    (formWidget, enctype) <- generateFormPost projectForm
    defaultLayout $(widgetFile "projects")

postProjectsR = do
    ((result, _), _) <- runFormPost projectForm
    case result of
        FormSuccess project -> do
            projectId <- runDB $ insert project
            setMessage "Your project has been added"
            redirect $ ProjectR projectId
        _ -> do
            setMessage "Invalid input"
            redirect ProjectsR
    return ()

getProjectR :: ProjectId -> Handler RepHtml
getProjectR projectId = do
    project <- runDB $ get404 projectId
    tickets <- runDB $ selectList [TicketProject ==. projectId] [Asc TicketName]
    defaultLayout $(widgetFile "project")

ticketForm :: ProjectId -> Form Ticket
ticketForm projectId = renderBootstrap $ Ticket
    <$> pure projectId
    <*> areq textField "Name" Nothing
    <*> areq textareaField "Description" Nothing
    <*> areq (jqueryDayField def) "StartDate" Nothing
    <*> areq (jqueryDayField def) "EndDate" Nothing

getTicketsR projectId = do
    tickets <- runDB $ selectList [] [Asc TicketName]
    (formWidget, enctype) <- generateFormPost (ticketForm projectId)
    defaultLayout $(widgetFile "tickets")

postTicketsR projectId = do
    ((result, _), _) <- runFormPost (ticketForm projectId)
    case result of
        FormSuccess ticket -> do
            ticketId <- runDB $ insert ticket
            setMessage "Your ticket has been added"
            redirect $ HomeR
        _ -> do
            setMessage "Invalid input"
            redirect ProjectsR
    return ()

getTicketR :: TicketId -> Handler RepHtml
getTicketR ticketId = do
    ticket <- runDB $ get404 ticketId
    defaultLayout $(widgetFile "ticket")