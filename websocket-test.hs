{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forever)
import Control.Exception (finally)
import Data.IORef
import Data.Text (Text)
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.Status (status200)
import Network.Wai (Application, responseFile)
import Network.Wai.Handler.WebSockets (websocketsOr)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS

type Client = (Int, WS.Connection)

broadcast :: Text -> [Client] -> IO ()
broadcast msg = mapM_ (flip WS.sendTextData msg) . map snd

addClient :: WS.Connection -> [Client] -> ([Client], Int)
addClient conn cs = let i = if null cs then 0 else maximum (map fst cs) + 1
                    in  ((i, conn):cs, i)

removeClient :: Int -> [Client] -> ([Client], ())
removeClient i cs = (filter (\c -> fst c /= i) cs, ())

chat :: IORef [Client] -> WS.ServerApp
chat ref pending = do
    conn <- WS.acceptRequest pending
    identifier <- atomicModifyIORef ref (addClient conn)
    flip finally (disconnect identifier) $ forever $ do
        msg <- WS.receiveData conn
        conns <- readIORef ref
        broadcast msg conns
    where
    disconnect identifier = atomicModifyIORef ref (removeClient identifier)

app :: Application
app req respond = respond $ responseFile status200 [(hContentType, "text/html")] "index.html" Nothing

main :: IO ()
main = do
    let port = 3000
    let setting = Warp.setPort port Warp.defaultSettings
    putStrLn $ "Your server is listening at http://localhost:" ++ show port ++ "/"
    ref <- newIORef []
    Warp.runSettings setting $ websocketsOr WS.defaultConnectionOptions (chat ref) app