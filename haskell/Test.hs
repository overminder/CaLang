import Driver

main = do
  src <- getContents
  let result = runAllPasses src
  outputPass result [OutputDot]

