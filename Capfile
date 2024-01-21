require 'capistrano/setup'
require 'capistrano/deploy'

require 'capistrano/tarball_scm'
install_plugin Capistrano::TarballScm::Plugin

require 'dotenv'
Dotenv.load('.env') if File.exist?('.env')
