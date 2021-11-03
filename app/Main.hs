import qualified Telegram
import qualified VK
import System.Environment (getArgs)

chooseBot :: String -> IO ()
chooseBot "vk" = VK.runVKBot Nothing
chooseBot "telegram" = Telegram.runTelegramBot
chooseBot _ = Telegram.runTelegramBot

main :: IO ()
main = do
  args <- getArgs
  if null args then chooseBot "other" else chooseBot $ head args
