module Handler.Chat where

import           Conduit
import           Control.Exception.Enclosed (tryAny)
import           Control.Monad              (forever, void)
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Import
import           Yesod.Form.Bootstrap3
import           Yesod.WebSockets

data Message = Message { content :: Text}

messageForm :: Html -> MForm Handler (FormResult Message, Widget)
messageForm = renderBootstrap3 BootstrapInlineForm $ Message
    <$> areq textField (withPlaceholder "Send message" $ bfs ("Message" :: Text)) Nothing

getChatR :: Text -> Handler Html
getChatR cName = do webSockets $ chatSocket cName
                    (widget, enctype) <- generateFormPost messageForm
                    defaultLayout $ do setTitle "Haskell Chat"
                                       $(widgetFile "chat")

chatSocket :: Text -> WebSocketsT Handler ()
chatSocket c = do app <- getYesod
                  sendTextData ("broadcast\nWelcome to the chat server, please enter your name." :: Text)
                  name <- receiveData
                  sendTextData $ T.append "broadcast\n" $ ("Welcome, " :: Text) <> name
                  let eEvt uf msg = do rooms <- readTVar $ chatRooms app
                                       let rc = broadcast app
                                       rooms' <- uf name c rooms
                                       (room, _) <- chatRoom c rooms'
                                       mc <- dupTChan $ channel room
                                       writeTVar (chatRooms app) rooms'
                                       writeTChan rc $ roomsToText rooms'
                                       let uMsg = "users\n" <> T.intercalate "\n" (users room)
                                           jMsg = "broadcast\n" <> name <> msg
                                       writeTChan (channel room) uMsg
                                       writeTChan (channel room) jMsg
                                       return (room, mc)
                  (_, mc) <- atm $ eEvt addUserToChatRoom " has joined the channel"
                  _ <- tryAny $ race_
                    (forever $ atm (readTChan mc) >>= sendTextData)
                    (sourceWS $$ mapM_C (\msg -> atm $ do let msg' = "message\n" <> name <> "\n" <> msg
                                                          writeTChan mc msg'))
                  void $ atm $ eEvt removeUserFromChatRoom " has left the channel"
