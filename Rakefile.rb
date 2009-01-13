require 'rake'
require 'rake/testtask'
require 'rake/rdoctask'

# === Project map
# * skirmish
#  * client
#   * src
#    * *.rb
#  * server
#   * src
#    * *.erl
#  * tests (system-level acceptance tests)
#   * automation.rb
#   * n.storyname.rb
#  * doc
#   * src
#    * design
#     * *.txt

desc "Compile and run tests"
task :default => [:build, :test]

desc "Compile, run tests and build documentation"
task :all => [:build, :test, :doc]

desc "Compile everything"
task :build

tests = FileList['test/test_*.rb']
desc "Run tests"
Rake::TestTask.new :test do |t|
  t.libs << 'test'
  t.test_files = tests
  t.warning = true
end

desc "Build documentation"
task :doc => [:design_docs, :automation_docs, :test_docs]

desc "Build design documents"
task :design_docs => ['design', 'protocol'].map {|n| "doc/design/#{n}.html" }

design_doc_src = proc do |html_path|
  m = html_path.match(/^doc\/(.*)\.html$/)
  return '' unless m
  return "doc/src/#{m[1]}.txt"
end
rule '.html' => [design_doc_src] do |t|
  sh "asciidoc -o #{t.name} #{t.source}"
end

Rake::RDocTask.new :automation_docs do |rd|
  rd.rdoc_dir = 'doc/automation'
  rd.main = 'Skirmish::Automation'
  rd.rdoc_files.include('test/automation.rb')
end

Rake::RDocTask.new :test_docs do |rd|
  rd.rdoc_dir = 'doc/test'
  rd.rdoc_files.include(tests)
end
