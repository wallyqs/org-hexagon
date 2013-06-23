
require 'sinatra/base'
require 'org-hexagon/text'
require 'org-ruby'

module OrgHexagon
  class Server < Sinatra::Base

    set :root, File.expand_path(File.join(File.dirname(__FILE__), '..', '..'))

    get '/' do
      @texts = Text.all
      erb :texts
    end

    not_found do
      erb :"404"
    end

    get '/texts' do
      @texts = Text.all
      erb :texts
    end

    get '/texts/:id' do
      text_id, format = params[:id].split('.')
      @text = Text.where(:_id => text_id).first
      halt 404 unless @text

      if format == 'org'
        content_type 'text/plain'
        @text.content
      else
        erb :text
      end
    end

    get '/shelves/:shelf' do
      shelf = params[:shelf]
      @texts = Text.where('properties.shelf' => shelf)

      erb :texts
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

    put '/api/texts.:format' do
      content_type 'application/json'

      json = request.body.read

      begin
        text = JSON.parse(json)
      rescue => e
        return { :status => 500,
          :message => "Error when parsing the request" }.to_json.to_s
      end

      begin
        if text['properties'] and text['properties']['id']
          t = Text.find(text['properties']['id'])
          unless t.update_attributes(text)
            return { :status => 500, :message => "Could not save the text" }.to_json.to_s
          end
        elsif text['content']
          t = Text.create(text)
        else
          return { :status => 500, :message => "Org text content was empty" }.to_json.to_s
        end
      rescue => e
        puts e
        return { :status => 500, :message => "Could not save the text" }.to_json.to_s
      end

      { :status => 200, :message => "OK", :id => t.id }.to_json.to_s
    end

    get '/api/shelves/:shelf.:format' do
      content_type 'application/json'

      shelf = params[:shelf]
      texts = Text.where('properties.shelf' => shelf)

      texts.to_json
    end
  end # class
end # module
