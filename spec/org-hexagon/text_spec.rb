require 'spec_helper'

module OrgHexagon
  describe Text do
    it "should get a title from the first headline" do
      org_text = Text.new(:content => SAMPLE_ORG_CONTENT)
      org_text.title.should == 'Hello World'
    end
  end
end
