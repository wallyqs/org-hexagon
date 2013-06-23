# org-hexagon/config.rb

require 'sinatra'
require 'mongoid'

module OrgHexagon
  Mongoid.configure do |config|
    config.master = Mongo::Connection.new.db('org_hexagon')
  end
end
