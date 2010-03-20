ENV['SCALA_HOME'] = ENV['SCALA28_HOME']

require 'buildr/scala'

repositories.remote << 'http://repo1.maven.org/maven2'

Project.local_task :pdf
Project.local_task :html

define 'gll-combinators' do
  project.group = 'edu.uwm.cs'
  project.version = '0.4.0-SNAPSHOT'
  
  test.using :specs
  test.exclude 'AllSpecs'
  
  package

  file 'target/doc/readme.tex' => [file('README.rst')] do |f|
    Dir.mkdir 'target/doc' unless File.exists? 'target/doc'

    latex = `rst2latex.py --use-verbatim-when-possible --use-latex-footnotes --use-latex-docinfo README.rst`

    File.open(f.to_s, 'w') do |file|
      file.puts latex
    end
  end
  
  pdf_task = file 'target/readme.pdf' => [file('target/doc/readme.tex')] do |f|
    Dir.chdir _(:target, :doc) do
      `latex readme`
      `pdflatex readme`

      mv 'readme.pdf', _(:target) + '/readme.pdf'
    end
  end

  html_task = file 'target/readme.html' => ['README.rst'] do |f|
    Dir.mkdir File.dirname(f.to_s) unless File.exists? File.dirname(f.to_s)

    html = `rst2html.py #{_('README.rst')}`
    File.open(f.to_s, 'w') do |file|
      file.puts html
    end
  end
  
  task :pdf => pdf_task
  task :html => html_task
end

