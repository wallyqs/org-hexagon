require 'cfruntime'
require 'sinatra'
require 'mongoid'

module OrgHexagon
  Mongoid.configure do |config|
    if ::CFRuntime::CloudApp.running_in_cloud?
      config.master = ::CFRuntime::MongoClient.create.db
    else

      if settings.environment == :test
        config.master = Mongo::Connection.new.db('org_hexagon_test')
      else
        config.master = Mongo::Connection.new.db('org_hexagon')
      end
    end
  end
end
