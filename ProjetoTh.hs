import Control.Concurrent
import System.Random
import Control.Monad

withDelay = True
 
threadteamA :: Double -> MVar Double -> MVar Bool -> IO()
threadteamA force ropeCenter endMVar = do
   
    valueCenter <- takeMVar ropeCenter

    power <- randomRIO (0 :: Double, 1)
    let powerForce = power * force
    --print "Team A:"
    --print (powerForce - 10.0)

    let printStr = printRope 0 $ floor (valueCenter - powerForce)
    putStrLn printStr

    endCheck <- threadCheck (valueCenter - powerForce)

    if not endCheck then do
        putMVar ropeCenter (valueCenter - powerForce)
        when withDelay $ threadDelay 1000000 
        threadteamA force ropeCenter endMVar
    else do
        putStrLn "TIME A VENCEU!"
        putMVar endMVar True

threadteamB :: Double -> MVar Double -> MVar Bool ->  IO()
threadteamB force ropeCenter endMVar = do

    valueCenter <- takeMVar ropeCenter

    power <- randomRIO (0 :: Double, 1)
    let powerForce = power * force
    --print "Team B:"
    --print (powerForce - 10.0)

    let printStr = printRope 0 $ floor (valueCenter + powerForce)
    putStrLn printStr
    
    endCheck <- threadCheck (valueCenter + powerForce)

    if not endCheck then do
        putMVar ropeCenter (valueCenter + powerForce)
        when withDelay $ threadDelay 1000000 
        threadteamB force ropeCenter endMVar
    else do
        putStrLn "TIME B VENCEU!"
        putMVar endMVar True


threadCheck :: Double -> IO Bool
threadCheck valueCenter = do
    if valueCenter > 20.0 then do
        return True
    else if valueCenter < 0.0 then do
        return True
    else do
        return False

printRope :: Int -> Int -> [Char]
printRope 0 0 = "A X|" ++ printRope 1 0
printRope 0 center = "A X-" ++ printRope 1 center
printRope n center | n == center = "|" ++ printRope (n+1) center
                   | n <= 20 = "-" ++ printRope (n+1) center
                   | otherwise = "X B"   

createThreadsA :: [Char] -> Int -> (Double -> MVar Double -> MVar Bool -> IO()) -> MVar Double -> MVar Bool -> IO()
createThreadsA team n th mvar endMvar = do

    --A forca da thread varia entre 1 e 4
    mbDouble <- randomRIO (1 :: Double, 5)
    putStrLn (team ++ "Thread " ++ show n ++ " Força: " ++ show (floor mbDouble))

    forkIO $ th ((fromIntegral (floor mbDouble)) :: Double) mvar endMvar
    
    if n > 1 then
        createThreadsA team (n-1) th mvar endMvar
    else
        threadDelay 1

createThreadsB :: [Char] -> Int -> Double -> (Double -> MVar Double -> MVar Bool -> IO()) -> MVar Double -> MVar Bool -> IO()
createThreadsB team n nivel th mvar endMvar = do

    --A forca da thread varia entre 1 e 4
    mbDouble <- randomRIO (1 :: Double, 5)
    let forca = nivel + mbDouble
    putStrLn (team ++ "Thread " ++ show n ++ " Força: " ++ show (floor forca))

    forkIO $ th ((fromIntegral (floor mbDouble)) :: Double) mvar endMvar
    
    if n > 1 then
        createThreadsB team (n-1) nivel th mvar endMvar
    else
        threadDelay 1

mainMVar2 :: IO()
mainMVar2 = do

    ropeCenter <- newEmptyMVar
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
        mainMVar2

    putStrLn "APERTE START!"
    inp <- getLine

    createThreadsA "Time A " 3 threadteamA ropeCenter endMVar
    createThreadsB "Time B " 3 level threadteamB ropeCenter endMVar

    putMVar ropeCenter 10.0

    end <- takeMVar endMVar
    return ()
    



    

    
