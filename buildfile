require 'buildr/scala'

repositories.remote << 'http://repo1.maven.org/maven2'

Buildr.settings.build['scala.check'] = '1.6'

Project.local_task :pdf
Project.local_task :html

define 'gll-combinators' do
  project.group = 'edu.uwm.cs'
  project.version = '0.5.0-SNAPSHOT'
  
  test.using :specs
  test.exclude 'AllSpecs'
  
  package

  file 'target/doc/readme.tex' => [file('README.rst')] do |f|
    info 'Generating readme.tex'
    
    mkdir_p _(:target, :doc) unless File.exists? _(:target, :doc)

    latex = `rst2latex.py --use-verbatim-when-possible --use-latex-footnotes --use-latex-docinfo README.rst`

    File.open(f.to_s, 'w') do |file|
      file.puts latex
    end
  end
  
  pdf = file 'target/readme.pdf' => [file('target/doc/readme.tex')] do |f|
    info 'Compiling readme.tex into PDF'
    Dir.chdir _(:target, :doc) do
      `latex readme`
      `pdflatex readme`

      mv 'readme.pdf', _(:target) + '/readme.pdf'
    end
  end

  html = file 'target/readme.html' => ['README.rst'] do |f|
    info 'Generating readme.html'
    mkdir File.dirname(f.to_s) unless File.exists? File.dirname(f.to_s)

    html = `rst2html.py #{_('README.rst')}`
    File.open(f.to_s, 'w') do |file|
      file.puts html
    end
  end
  
  task :pdf => pdf
  task :html => html
end

