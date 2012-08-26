require 'mongoid'
require 'org-ruby'

module OrgHexagon
  class Text
    include Mongoid::Document

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
  end
end
