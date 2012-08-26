require 'sinatra'
require 'rspec'

set :environment, :test
set :run, false
set :raise_errors, true
set :logging, false

require 'org-hexagon'

def app
  OrgHexagon::Server
end

SAMPLE_ORG_CONTENT = <<ORG
* Hello World

Some text
ORG
