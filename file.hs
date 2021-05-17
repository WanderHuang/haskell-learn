import System.IO

main = do
  -- 一个句柄操作实例
  inh <- openFile "maze.txt" ReadMode
  inStr <- hGetContents inh
  putStrLn inStr
  hClose inh
  -- h开头的函数带有句柄，操作后需要关闭
  putStrLn inStr
