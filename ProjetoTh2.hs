import Control.Concurrent
import GHC.Conc.Sync
import System.Random
import Control.Monad

withDelay = True


threadteamA :: Double -> TVar Double -> MVar Bool -> IO()
threadteamA force ropeCenter endVar = do
    --print "A"
   -- threadDelay 1000000

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
    --print "B"
  --  threadDelay 1000000

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

createThreads :: [Char] -> Int -> (Double -> TVar Double -> MVar Bool -> IO()) -> TVar Double -> MVar Bool -> IO()
createThreads team n th mvar endMvar = do

    --A forca da thread varia entre 1 e 4
    mbDouble <- randomRIO (1 :: Double, 5)
    putStrLn (team ++ "Thread " ++ show n ++ " Força: " ++ show (floor mbDouble))

    forkIO $ th ((fromIntegral (floor mbDouble)) :: Double) mvar endMvar
    
    if n > 1 then
        createThreads team (n-1) th mvar endMvar
    else
        threadDelay 1

main :: IO()
main = do

    ropeCenter <- atomically $ newTVar 10.0
    endMVar <- newEmptyMVar


    putStrLn "APERTE START!"
    level <- getLine
    createThreads "Time A " 3 threadteamA ropeCenter endMVar
    createThreads "Time B " 3 threadteamB ropeCenter endMVar


   -- createThreads "Time A " 3 threadteamA ropeCenter endMVar
   -- createThreads "Time B " 3 threadteamB ropeCenter endMVar

    end <- takeMVar endMVar
    value <- readTVarIO ropeCenter
    if value > 20 then do
        print "______________"
        print "TIME B GANHOU!" 
        print ""
        print "Score"
        print (value - 10)

    else do
        print "______________"
        print "TIME A GANHOU!"
        print ""
        print "Score"
        print (value - 10)
    return ()
    



    

    
