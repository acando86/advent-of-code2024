module Stats where

import Args (StatsOutput)
import Control.Monad ((<=<))
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Void (Void)
import qualified Env
import Network.HTTP.Req
import Text.Megaparsec (MonadParsec (try), Parsec, anySingle, optional, runParser, skipManyTill, takeWhileP)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Trans (lift)

newtype StarsCount = StarsCount T.Text deriving (Eq, Show)
newtype Stats = Stats {starsCount :: StarsCount} deriving (Eq, Show)

newtype StatsConfig = StatsConfig
  { session :: String
  }
  deriving (Eq, Show)

data StatsError = Not200 Int | HtmlError ParsingError deriving (Eq,Show)

statsConfigParser :: Env.Parser Env.Error StatsConfig
statsConfigParser = StatsConfig <$> Env.var (Env.str <=< Env.nonempty) "AOC_SESSION" (Env.help "Value of the session cookie, used for login")

program :: Int -> StatsOutput -> IO ()
program year output = do
  config <- Env.parse (Env.header "AOC stats requires env variables") statsConfigParser
  program' year output config

program' :: Int -> StatsOutput -> StatsConfig -> ExceptT StatsError IO Stats
program' year output config = do
   (status, body) <- lift $ fetchPage year (session config)
   if (status /= 200) then throwError (Not200 status) else ExceptT (parseStarsCount body)


generateUrl :: Int -> T.Text
generateUrl year = T.pack ("https://adventofcode.com/" ++ show year)

fetchPage :: Int -> String -> IO (Int, T.Text)
fetchPage year s = runReq defaultHttpConfig $ do
  let url = https (generateUrl year)
      cookieHeader = B8.pack ("session=" ++ s)
  response <- req GET url NoReqBody bsResponse (header "Cookie" cookieHeader)
  let body = TE.decodeUtf8 $ responseBody response
      status = responseStatusCode response
  pure (status, body)

-- parsing
type Parser = Parsec Void T.Text
type ParsingError = ParseErrorBundle T.Text Void

starsCountParser :: Parser (Maybe StarsCount)
starsCountParser =
  optional $
    StarsCount
      <$> ( string "<span class=\"star-count\">"
              *> takeWhileP Nothing (/= '<')
              <* string "</span>"
          )

parseStarsCount :: L8.ByteString -> Either ParsingError (Maybe StarsCount)
parseStarsCount _ = Right (Just (StatsCount "10"))
