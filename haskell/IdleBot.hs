
import BotLib
import Types

main :: IO ()
main = runStatelessBot (const (BotMessage [] [])) NoLog