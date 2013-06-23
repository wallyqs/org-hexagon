require 'rspec/core'
require 'rspec/core/rake_task'

# FIXME: Set this according to the platform
$emacs_runtime = "/Applications/Emacs.app/Contents/MacOS/Emacs -Q --batch --eval"

RSpec::Core::RakeTask.new(:spec) do |spec|
  spec.pattern = FileList['spec/**/*_spec.rb']
  spec.rspec_opts = ["--format", "documentation", "--colour"]
end

task :default => ['emacs:tangle', 'emacs:indent']

namespace 'emacs' do
  desc 'Tangle the project file'
  task 'tangle' do
    file = File.join('lib', 'org-hexagon.org')
    command = <<COMMAND
#{$emacs_runtime} \
"(progn (find-file \\"#{file}\\") (message (concat \\"Org babel on this file: \\" \\"#{file}\\")) (org-babel-tangle))"
COMMAND
    output = `#{command}`
    if $? != 0
      puts "Error when indenting file `#{file}' : #{output}"
    end
  end

  desc 'Indent the exports from Org babel'
  task 'indent' do
    files = Dir["lib/**/**/**.rb"]

    files.each do |file|
      command = <<COMMAND
#{$emacs_runtime} \
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
