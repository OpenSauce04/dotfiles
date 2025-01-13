local settings = require "settings"

-- Set default search engine to All the Internet
local engines = settings.window.search_engines
engines.alltheinternet = "https://www.alltheinternet.com/?q=%s"
engines.default = engines.alltheinternet

-- Disable broken hardware acceleration
settings.webview.hardware_acceleration_policy = 'never'
