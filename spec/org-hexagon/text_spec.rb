require 'spec_helper'

module OrgHexagon
  describe Text do
    it "should get a title from the first headline" do
      org_text = Text.new(:content => SAMPLE_ORG_CONTENT)
      org_text.title.should == 'Hello World'
    end

    it "should get the name of the shelf in case it exists" do
      org_text = Text.new(:content => SAMPLE_ORG_CONTENT, 
                          :properties => { 'shelf' => 'tests' })
      org_text.shelf.should == 'tests'

      org_text = Text.new(:content => SAMPLE_ORG_CONTENT)
      org_text.shelf.should == nil
    end
  end
end
