import Text.Parsec
import Text.Parsec.String

parseNumber :: Parser Int
parseNumber = do
  -- the <|> means that try what is there on my left if that fails then try my right
  	--char and all are parsec provided functions
  neg <- (char '-' >> return "-") <|> (return "")
  n' <- many1 $ oneOf "0123456789"
  return (read  (neg ++ n') )

calculation :: Parser Int
calculation = do
  n1 <- parseNumber
  char '+'
  n2 <- parseNumber
  return (n1+n2)

calculate :: String -> String
calculate s = 
  case ret of
    Left e -> "error: " ++ (show e)
    Right n -> "answer: " ++ (show n)
  where
    ret = parse calculation "" s

main :: IO ()
main = interact (unlines . (map calculate) . lines)
