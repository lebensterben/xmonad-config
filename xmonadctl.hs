import           Graphics.X11.Xlib                        ( allocaXEvent
                                                          , clientMessage
                                                          , defaultScreen
                                                          , internAtom
                                                          , openDisplay
                                                          , rootWindow
                                                          , sendEvent
                                                          , structureNotifyMask
                                                          , sync
                                                          )
import           Graphics.X11.Xlib.Extras                 ( currentTime
                                                          , setClientMessageEvent
                                                          , setEventType
                                                          )
import           System.Environment                       ( getArgs
                                                          , getProgName
                                                          )
import           System.IO                                ( hPutStrLn
                                                          , isEOF
                                                          , stderr
                                                          )

main :: IO ()
main = parse True "XMONAD_COMMAND" =<< getArgs

parse :: Bool -> String -> [String] -> IO ()
parse input addr args = case args of
    ["--"] | input     -> repl addr
           | otherwise -> return ()
    ("--"        : xs) -> sendAll addr xs
    ("-a" : a    : xs) -> parse input a xs
    ("-h"        : _ ) -> showHelp
    ("--help"    : _ ) -> showHelp
    ("-?"        : _ ) -> showHelp
    (a@('-' : _) : _ ) -> hPutStrLn stderr ("Unknown option " ++ a)

    (x           : xs) -> sendCommand addr x >> parse False addr xs
    [] | input     -> repl addr
       | otherwise -> return ()

repl :: String -> IO ()
repl addr = do
    e <- isEOF
    if e
        then return ()
        else do
            sendCommand addr =<< getLine
            repl addr

sendAll :: String -> [String] -> IO ()
sendAll addr = foldr (\a b -> sendCommand addr a >> b) (return ())

sendCommand :: String -> String -> IO ()
sendCommand addr s = do
    d  <- openDisplay ""
    rw <- rootWindow d $ defaultScreen d
    a  <- internAtom d addr False
    m  <- internAtom d s False
    allocaXEvent $ \e ->
        setEventType e clientMessage
            >> setClientMessageEvent e rw a 32 m currentTime
            >> sendEvent d rw False structureNotifyMask e
            >> sync d False

showHelp :: IO ()
showHelp = getProgName >>= \x ->
    putStrLn
        $ "Send commands to a running instance of xmonad. xmonad.hs must be configured with XMonad.\
         \Hooks.ServerMode to work.\n-a atomname can be used at any point in the command line \
         \arguments to change which atom it is sending on.\nIf sent with no arguments or only -a \
         \atom arguments, it will read commands from stdin.\nEx:\n"
        ++ x
        ++ " cmd1 cmd2\n"
        ++ x
        ++ " -a XMONAD_COMMAND cmd1 cmd2 cmd3 -a XMONAD_PRINT hello world\n"
        ++ x
        ++ " -a XMONAD_PRINT # will read data from stdin.\nThe atom defaults to XMONAD_COMMAND."
