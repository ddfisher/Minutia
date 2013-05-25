import Network.HTTP
import Text.HTML.TagSoup
import Control.Concurrent.ParallelIO
import Control.Applicative


getSite :: String -> IO String
getSite url = simpleHTTP (getRequest $ "http://" ++ url) >>= getResponseBody

countLinks :: String -> Int
countLinks body = length [t | t@(TagOpen "a" _) <- parseTags body]

readSites :: IO [String]
readSites = lines <$> readFile "top-100.txt"

main = do
  sites <- readSites
  -- links <- parallel $ map (fmap countLinks . getSite) sites
  parallel_ $ map ((>>= print) . fmap countLinks . getSite) sites
  -- print links
