require 'rspec/core'
require 'rspec/core/rake_task'

RSpec::Core::RakeTask.new(:spec) do |spec|
  spec.pattern = FileList['spec/**/*_spec.rb']
  spec.rspec_opts = ["--format", "documentation", "--colour"]
end

namespace 'emacs' do
  desc 'Indent the exports from Org babel'
  task 'indent' do
    files = Dir["lib/**/**/**.rb"]

    files.each do |file|
      command = <<COMMAND
/Applications/Emacs.app/Contents/MacOS/Emacs -Q --batch --eval \
"(progn (find-file \\"#{file}\\") (message (concat \\"Indenting file: \\" \\"#{file}\\")) \
(delete-trailing-whitespace) \
(indent-region (point-min) (point-max) nil) \
(untabify (point-min) (point-max)) \
(save-buffer))"
COMMAND
      output = `#{command}`
      if $? != 0
        puts "Error when indenting file `#{file}' : #{output}"
      end
    end
  end
end
