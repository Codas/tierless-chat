{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Home where

import           Conduit
import           Control.Monad         (forever, void)
import qualified Data.Map              as M
import           Import
import           Yesod.Form.Bootstrap3
import           Yesod.WebSockets

data ChannelName = ChannelName { name :: Text}

channelForm :: Html -> MForm Handler (FormResult ChannelName, Widget)
channelForm = renderBootstrap3 BootstrapInlineForm $ ChannelName
    <$> areq textField (withPlaceholder "Room Name" $ bfs ("Room Name" :: Text)) Nothing

getHomeR :: Handler Html
getHomeR = do app <- getYesod
              webSockets (homeSocket app)
              (widget, enctype) <- generateFormPost channelForm
              rooms <- atm $ readTVar $ chatRooms app
              let roomInformation = M.toAscList $ channelInformation rooms
              defaultLayout $ do setTitle "Haskell Chat"
                                 $(widgetFile "home")


homeSocket :: App -> WebSocketsT Handler ()
homeSocket app = do
    readChan <- atm $ dupTChan (broadcast app)
    race_ -- execute both, exit when one finishes
      (forever $
        atm (readTChan readChan) >>= sendTextData)
      (sourceWS $$ mapM_C createNew)
  where createNew :: MonadIO m => Text -> m ()
        createNew c = atm (newRoom c)
        newRoom :: Text -> STM ()
        newRoom c = do
            let chan = broadcast app
                room = chatRooms app
            rooms' <- do rooms <- readTVar room
                         addChatRoom c rooms
            writeTVar room rooms' -- update
            writeTChan chan (roomsToText rooms')
