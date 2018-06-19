module Config where



searchHost :: String
searchHost = "111.230.171.235:6000"

dsn :: String
dsn = "sqlite:bot.db"

periodicHost :: {port :: Int, host :: String}
periodicHost = {port: 5000, host: "127.0.0.1"}
