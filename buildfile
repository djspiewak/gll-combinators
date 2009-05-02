require 'buildr/scala'

repositories.remote << 'http://repo1.maven.org/maven2'

define 'gll-combinators' do
  project.group = 'edu.uwm.cs'
  project.version = '0.2.0'
  
  test.using :specs
  test.exclude 'AllSpecs'
  
  if defined? cobertura
    cobertura.include /edu.uwm.cs.gll..+/
    
    cobertura.exclude /edu.uwm.cs.gll.Global/
    cobertura.exclude /^[^\.]+$/
    cobertura.exclude /edu.uwm.cs.util..+/
  end

  package
end
