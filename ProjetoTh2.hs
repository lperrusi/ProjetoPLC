import Control.Concurrent
import GHC.Conc.Sync
import System.Random
import Control.Monad

withDelay = True


threadteamA :: Double -> TVar Double -> MVar Bool -> IO()
threadteamA force ropeCenter endVar = do

    power <- randomRIO (0 :: Double, 1)
    let powerForce = power * force
  
    didend <- atomically $ do
        valueCenter <- readTVar ropeCenter
        if not (threadCheck valueCenter) then do
            writeTVar ropeCenter (valueCenter - powerForce)
            return False
        else
            return True

    if not didend then do
        threadteamA force ropeCenter endVar
    else do
        putMVar endVar True


threadteamB :: Double -> TVar Double -> MVar Bool -> IO()
threadteamB force ropeCenter endVar = do

    power <- randomRIO (0 :: Double, 1)

    let powerForce = power * force
    
    didend <- atomically $ do
        valueCenter <- readTVar ropeCenter
        if not (threadCheck valueCenter) then do
            writeTVar ropeCenter (valueCenter + powerForce)
            return False
        else
            return True

    if not didend then do
        threadteamB force ropeCenter endVar
    else do
        putMVar endVar True



threadCheck :: Double -> Bool
threadCheck valueCenter = valueCenter > 20.0 || valueCenter < 0.0

createThreadsA :: [Char] -> Int -> (Double -> TVar Double -> MVar Bool -> IO()) -> TVar Double -> MVar Bool -> IO()
createThreadsA team n th tvar endMvar = do

    --A forca da thread varia entre 1 e 4
    mbDouble <- randomRIO (1 :: Double, 5)
    putStrLn (team ++ "Thread " ++ show n ++ " Força: " ++ show (floor mbDouble))

    forkIO $ th  ((fromIntegral (floor mbDouble)) :: Double) tvar endMvar
    
    if n > 1 then
        createThreadsA team (n-1) th tvar endMvar
    else
        threadDelay 1

createThreadsB :: [Char] -> Int -> Double -> (Double -> TVar Double -> MVar Bool -> IO()) -> TVar Double -> MVar Bool -> IO()
createThreadsB team n level th tvar endMvar = do

    --A forca da thread varia entre 1 e 4

    mbDouble <- randomRIO (1 :: Double, 5)
    let forca = mbDouble + level
    putStrLn (team ++ "Thread " ++ show n ++ " Força: " ++ show (floor forca))

    forkIO $ th ((fromIntegral (floor forca)) :: Double) tvar endMvar
    
    if n > 1 then
        createThreadsB team (n-1) level th tvar endMvar
    else
        threadDelay 1

mainTvar :: IO()
mainTvar = do

    ropeCenter <- atomically $ newTVar 10.0
    endMVar <- newEmptyMVar

    putStrLn "Escolha um nível - (1) (2) (3)"
    
    input <- getLine
    let level = (read input :: Double)
    if level == 1.0 then do
        putStrLn "Nível Um Escolhido"
    
    else if level == 2.0 then do
        putStrLn "Nível Dois Escolhido"
    
    else if level == 3.0 then do
        putStrLn "Nível Tres Escolhido"
    else do
        putStrLn "Escolha entre (1) (2) (3)"
        mainTvar

    putStrLn "APERTE START!"
    inp <- getLine
    createThreadsA "Time A " 3 threadteamA ropeCenter endMVar
    createThreadsB "Time B " 3  level threadteamB ropeCenter endMVar

    end <- takeMVar endMVar
    value <- readTVarIO ropeCenter
    if value > 20.0 then do
        print "______________"
        print "TIME B GANHOU!" 
    else do
        print "______________"
        print "TIME A GANHOU!"
    return ()
    



    

    
