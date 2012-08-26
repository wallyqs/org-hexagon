require 'spec_helper'
require 'rack/test'
require 'json'

module OrgHexagon
  describe Server do
    include Rack::Test::Methods

    before :all do
      Text.delete_all
    end

    it 'should respond to /' do
      get '/texts'
      last_response.should be_ok
    end

    context 'using the API' do
      it 'should be possible to create a text and fetch it by _id' do
        post '/api/texts.json', { :content => SAMPLE_ORG_CONTENT }.to_json
        last_response.should be_ok

        resp = JSON.parse(last_response.body)
        resp['status'].should == 200

        id = resp['_id']
        get "/api/texts/#{id}.json"
        last_response.should be_ok
        saved_text = JSON.parse(last_response.body)
        saved_text['_id'].should == resp['_id']

        # HTML pages
        get "/texts/#{id}"
        last_response.should be_ok
      end

      it 'should be possible to get all texts' do
        get '/api/texts.json'
        texts = JSON.parse(last_response.body)

        texts.count.should > 0
      end
    end
  end
end
