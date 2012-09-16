require 'sinatra/base'
require 'org-hexagon/text'
require 'org-ruby'

module OrgHexagon
  class Server < Sinatra::Base

    set :root, File.expand_path(File.join(File.dirname(__FILE__), '..', '..'))

    get '/' do
      @texts = Text.all
      erb :main
    end

    not_found do
      erb :"404"
    end

    get '/texts' do
      @texts = Text.all
      erb :main
    end

    get '/texts/:id' do
      @text = Text.where(:_id => params[:id]).first
      halt 404 unless @text

      erb :text
    end

    # API
    get '/api/texts.:format' do
      content_type 'application/json'
      @texts = Text.all

      @texts.to_json
    end

    get '/api/texts/:id.:format' do
      content_type 'application/json'

      text = Text.where(:_id => params[:id]).first
      halt 404 unless text

      return text.to_json
    end

    post '/api/texts.:format' do
      content_type 'application/json'

      json = request.body.read

      begin
        text = JSON.parse(json)
      rescue => e
        return {:status => 500, :message => "Error when parsing the request" }.to_json.to_s
      end

      if text['content']
        t = Text.create(text)
      else
        return { :status => 500, :message => "Org text content was empty" }.to_json.to_s
      end

      { :status => 200, :message => "OK", :_id => t.id }.to_json.to_s
    end

    get '/api/shelves/:shelf.:format' do
      content_type 'application/json'

      shelf = params[:shelf]
      texts = Text.where('properties.shelf' => shelf)

      texts.to_json
    end
  end
end
