require 'cfruntime'
require 'mongoid'

module OrgHexagon
  Mongoid.configure do |config|
    if ::CFRuntime::CloudApp.running_in_cloud?
      config.master = ::CFRuntime::MongoClient.create.db
    else
      config.master = Mongo::Connection.new.db('org_hexagon')
    end  
  end
end
