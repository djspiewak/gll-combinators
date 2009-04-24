require 'buildr/scala'

repositories.remote << 'http://repo1.maven.org/maven2'

define 'gll-combinators' do
  project.group = 'edu.uwm.cs'
  project.version = '0.1.0'

  package
end
