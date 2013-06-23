
require 'mongoid'
require 'org-ruby'

module OrgHexagon
  class Text
    include Mongoid::Document
    include Mongoid::Timestamps
    include Mongoid::Versioning
    max_versions 10

    def title
      org = Orgmode::Parser.new(self.content)

      # First try to pick the title from the org-mode settings
      if not org.in_buffer_settings['TITLE'].nil?
        title = org.in_buffer_settings['TITLE']
      else
        # If we could not get title from org-mode,
        # we extract it from the first headline in textile
        textile = org.to_textile
        title = textile.lines.first.gsub(/^\w+\.\s?/, '').chomp
      end

      title
    rescue => e
      puts e
      'untitled'
    end

    def to_html
      Orgmode::Parser.new(self.content).to_html
    rescue
      'Could not parse text!'
    end

    def shelf
      if self.attributes.include?('properties') && self.properties && self.properties['shelf']
        self.properties['shelf']
      else
        nil
      end
    end

  end # class
end # module
