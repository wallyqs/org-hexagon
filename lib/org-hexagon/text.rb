require 'mongoid'
require 'org-ruby'

module OrgHexagon
  class Text
    include Mongoid::Document
    include Mongoid::Timestamps
    include Mongoid::Versioning
    max_versions 10

    def title
      parsed_content = Orgmode::Parser.new(self.content)

      # First try to pick the title from the org-mode settings
      if not parsed_content.in_buffer_settings['TITLE'].nil?
        title = parsed_content.in_buffer_settings['TITLE']
      else
        # If we could not get title from org-mode,
        # we extract it from the first headline in textile
        textile = parsed_content.to_textile
        title = textile.lines.first.gsub(/^\w+\.\s?/, '').chomp
      end

      title
    rescue => e
      puts e
      'untitled'
    end

    def parsed_org_text
      Orgmode::Parser.new(self.content)
    end

    def to_html
      parsed_org_text.to_html
    end

    def shelf
      if self.attributes.include?('properties') && self.properties && self.properties['shelf']
        self.properties['shelf']
      else
        nil
      end
    end
  end
end
