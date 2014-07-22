require 'json'
require 'fileutils'

fromPath = File.expand_path("../../../../apps/tapestry/priv/www/") + "/"
toPath = "Tapestry/www/src/"
FileUtils.mkdir_p("Tapestry/www/src")

scripts =  JSON.parse( IO.read(fromPath + "/app_scripts")) +
JSON.parse( IO.read(fromPath + "/app_styles")) +
["nci.html", "app_scripts", "app_styles", "static/server_emulator.js", "output.json"]

scripts.each do |script|
  FileUtils.mkdir_p(File.dirname(toPath + script))
  FileUtils.cp(fromPath + script, toPath + script)
end
