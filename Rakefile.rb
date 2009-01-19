require 'rake'
require 'rake/clean'
require 'rake/testtask'
require 'rake/rdoctask'

# erlang common_test run_test script
run_test = *Dir['/usr/local/lib/erlang/lib/common_test-*/priv/bin/run_test']

# === Project map
# * skirmish
#  * skirmish-client
#   * bin
#    * 'skirmish-client'
#   * src
#    * '*.rb'
#   * test
#    * 'fake-*.rb'
#    * 'test_*.rb'
#  * skirmish_server (structured to erlang conventions - erlang tools assume it)
#   * 'Emakefile'
#   * src
#    * '*.erl'
#    * '*.hrl'
#   * include
#    * '*.hrl'
#   * test
#    * '*_SUITE.erl'
#   * priv
#   * ebin
#  * tests (system-level acceptance tests)
#   * automation
#    * '*.rb*
#   * 'test_<storyname>.rb'
#  * doc
#   * src
#    * design
#     * '*.txt'

CLOBBER.clear_exclude
CLOBBER << Dir["**/.DS_Store"]
CLOBBER << Dir["**/erl_crash.dump"]

skirmish_client = 'skirmish-client/bin/skirmish-client'
client_tests = FileList['skirmish-client/test/test_*.rb'].
  exclude("skirmish-client/test/test_*_*.rb")
tests = FileList['test/test_*.rb']

desc "Compile and run tests"
task :default => [:build, :test_all]

desc "Compile, run tests and build documentation"
task :all => [:build, :test_all, :doc]

desc "Compile everything"
task :build => [:build_server, :build_client]

desc "Build client"
task :build_client => [skirmish_client]

file skirmish_client => ['skirmish-client/src/skirmish-client.rb'] do |t|
  cp t.prerequisites[0], t.name
  chmod 0755, t.name
end
CLOBBER << skirmish_client

desc "Run client tests"
Rake::TestTask.new :test_client => [:build_client] do |t|
  t.libs = ['skirmish-client/test', 'skirmish-client/src']
  t.test_files = client_tests
  t.warning = true
end

# Run specific tests or test files
#
# rake test_client:connect_validation
# => Runs the full TestConnectValidation unit test suite
#
# rake test:connect:secret
# => Runs the tests matching /secret/ in the TestConnect unit test suite
rule /^test_client:/ do |t|
  # test:file:method
  if /^test_client:(.*)(:([^.]+))?$/.match(t.name)
    arguments = t.name.split(":")[1..-1]
    file_name = arguments.first
    test_name = arguments[1..-1]

    test = 'skirmish-client/test'
    if File.exist?("#{test}/test_#{file_name}.rb")
      run_file_name = "test_#{file_name}.rb"
    else
      raise ScriptError, "No suite named #{file_name}"
    end

    src = 'skirmish-client/src'
    sh "ruby -I#{src}:#{test} #{test}/#{run_file_name} -n /#{test_name}/"
  end
end

Rake::RDocTask.new :client_devel_docs do |rd|
  rd.rdoc_dir = 'doc/client-devel'
  rd.main = 'Skirmish::Client'
  rd.rdoc_files.include('skirmish-client/src/**.rb')
  rd.options << '--inline-source' << '--all'
  rd.template = "extras/flipbook.rb"
end

desc "Build server"
task :build_server => "skirmish_server/ebin/skirmish_server.app" do
  cd 'skirmish_server'
  sh "erl -make"
  cd '..'
end
CLOBBER << Dir['**/*.beam']

file "skirmish_server/ebin/skirmish_server.app" do |t|
  cp t.name.sub("ebin", "src"), t.name
end
CLOBBER << "skirmish_server/ebin/skirmish_server.app"

server_test_log_dir = 'skirmish_server/test-log/'
directory server_test_log_dir
desc "Run server tests"
task :test_server => [:build_server, server_test_log_dir] do
  sh "#{run_test} -pa skirmish_server/ebin -dir skirmish_server/test -logdir skirmish_server/test-log"
end
CLOBBER << server_test_log_dir

desc "Run all tests"
task :test_all => [:test_client, :test_server, :test]

desc "Run tests"
Rake::TestTask.new :test do |t|
  t.libs << 'test'
  t.test_files = tests
  t.warning = true
end

desc "Build documentation"
task :doc => [:design_docs, :client_devel_docs, :automation_docs, :test_docs]

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
  rd.rdoc_files.include('test/automation/*.rb')
  rd.options << '--inline-source'
  rd.template = "extras/flipbook.rb"
end

Rake::RDocTask.new :test_docs do |rd|
  rd.rdoc_dir = 'doc/test'
  rd.rdoc_files.include(tests)
  rd.options << '--inline-source'
  rd.template = "extras/flipbook.rb"
end
