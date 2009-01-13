require 'rake'
require 'rake/clean'
require 'rake/testtask'
require 'rake/rdoctask'

# === Project map
# * skirmish
#  * client
#   * bin
#    * skirmish-client
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

skirmish_client = 'client/bin/skirmish-client'
client_tests = FileList['client/test/test_*.rb']
tests = FileList['test/test_*.rb']

desc "Compile and run tests"
task :default => [:build, :test]

desc "Compile, run tests and build documentation"
task :all => [:build, :test, :doc]

desc "Compile everything"
task :build => [:build_client]

desc "Build client"
task :build_client => [skirmish_client]

file skirmish_client => ['client/src/skirmish-client.rb'] do |t|
  cp t.prerequisites[0], t.name
  chmod 0755, t.name
end
CLOBBER << skirmish_client

desc "Run client tests"
Rake::TestTask.new :test_client => [:build_client] do |t|
  t.libs << 'client/test'
  t.test_files = client_tests
  t.warning = true
end

desc "Build server"
task :build_server

desc "Run server tests"
task :test_server => [:build_server]

desc "Run tests"
Rake::TestTask.new :test => [:test_client, :test_server] do |t|
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
CLOBBER << Dir['doc/design/*.html']

Rake::RDocTask.new :automation_docs do |rd|
  rd.rdoc_dir = 'doc/automation'
  rd.main = 'Skirmish::Automation'
  rd.rdoc_files.include('test/automation.rb')
end

Rake::RDocTask.new :test_docs do |rd|
  rd.rdoc_dir = 'doc/test'
  rd.rdoc_files.include(tests)
end
