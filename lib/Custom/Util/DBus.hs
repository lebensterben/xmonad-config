module Custom.Util.DBus (dbusOutput, mkDbusClient) where

import qualified Codec.Binary.UTF8.String                as UTF8
import qualified DBus                                    as D
import qualified DBus.Client                             as D

mkDbusClient :: IO D.Client
mkDbusClient = do
    client <- D.connectSession
    _      <- D.requestName client busName opts
    return client
  where
    busName = D.busName_ "org.xmonad.log"
    opts    = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

dbusOutput :: D.Client -> String -> IO ()
dbusOutput client str =
    let opath  = D.objectPath_ "/org/xmonad/Log"
        iname  = D.interfaceName_ "org.xmonad.Log"
        mname  = D.memberName_ "Update"
        body   = map (D.toVariant . UTF8.decodeString) $ lines str
        signal = (D.signal opath iname mname) { D.signalBody = body }
    in  D.emit client signal
