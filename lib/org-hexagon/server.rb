require 'sinatra/base'
require 'org-hexagon/text'
require 'org-ruby'
require 'yajl'

module OrgHexagon
  class Server < Sinatra::Base

    set :root, File.expand_path(File.join(File.dirname(__FILE__), '..', '..'))

    def initialize
      super
      @json_parser = Yajl::Parser.new
    end

    get '/' do

      @texts = Text.all

      erb :main
    end

    post '/api/text.:format' do
      json = request.body.read

      begin
        text = @json_parser.parse(json)
      rescue => e
        return {:status => 500, :message => "Error when parsing the request" }.to_json.to_s
      end

      if text['content']
        t = Text.create(:content => text['content']) 
      else
        return { :status => 500, :message => "Org text content was empty" }.to_json.to_s
      end
      
      { :status => 200, :message => "OK", :id => t.id }.to_json.to_s
    end
  end
end
