import Control.Concurrent


threadAdv :: MVar Int -> IO()
threadAdv ropeCenter = do
    valueCenter <- takeMVar ropeCenter
    print valueCenter
    putMVar ropeCenter (valueCenter - 1)
    threadDelay 1000000
    
    threadAdv ropeCenter

threadPlayer :: MVar Int -> IO()
threadPlayer ropeCenter = do
    --pega ação do jogador
    level <- getLine
    valueCenter <- takeMVar ropeCenter
    print valueCenter 
    putMVar ropeCenter (valueCenter + 1)

    threadPlayer ropeCenter

--verifica se a posição da corda chegou no limite

threadCheck :: MVar Int -> IO()
threadCheck ropeCenter = do
    valueCenter <- takeMVar ropeCenter

    if valueCenter >= 10 then do
        putStrLn "JOGADOR VENCEU!"
    else if valueCenter <= -10 then do
        putStrLn "JOGADOR PERDEU"
    else do
        putMVar ropeCenter (valueCenter)
        threadCheck ropeCenter
        threadDelay 1000

createThreads :: Int -> (MVar Int -> IO()) -> MVar Int -> IO()
createThreads n th mvar = do
    forkIO(th mvar)

    if n > 1 then
        createThreads (n-1) th mvar
    else
        threadDelay 1
    
mainMVar1 :: IO()
mainMVar1 = do
--cria corda no 'centro'
    ropeCenter <- newMVar 0

    putStrLn "Escolha um nível - (1) (2) (3)"
    
    level <- getLine
    
    if level == "1" then do
        putStrLn "Nível Um Escolhido"

        createThreads 1 threadAdv ropeCenter
    
    else if level == "2" then do
        putStrLn "Nível Dois Escolhido"
        createThreads 2 threadAdv ropeCenter
    
    else if level == "3" then do
        putStrLn "Nível Tres Escolhido"
        createThreads 3 threadAdv ropeCenter
    
    else do
        putStrLn "Escolha entre (1) (2) (3)"
        mainMVar1

    forkIO(threadPlayer ropeCenter)
    forkIO(threadCheck ropeCenter)

    threadDelay 1000



    

    
