import qualified Telegram
import qualified VK
import System.Environment (getArgs)

chooseBot :: String -> IO ()
chooseBot "vk" = VK.runVKBot
chooseBot "telegram" = Telegram.runTelegramBot
chooseBot _ = Telegram.runTelegramBot

main :: IO ()
main = do
  args <- getArgs
  chooseBot $ head args
