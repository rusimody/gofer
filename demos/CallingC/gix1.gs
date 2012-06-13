
primitive howdy "sayHello" :: Int -> IO ()

main = howdy (length (filter even [1..5]))

