#!/usr/bin/ruby
# -*- ruby -*-

require 'irb/completion'
require 'irb/ext/save-history'

IRB.conf[:SAVE_HISTORY] = 1000
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb_history"

IRB.conf[:PROMPT_MODE] = :SIMPLE

%w[rubygems looksee/shortcuts].each do |gem|
  begin
    require gem
  rescue LoadError
  end
end

begin
  require 'wirble'
  Wirble.init
  Wirble.colorize
  require 'hirb' # sudo gem install cldwalker-hirb --source http://gems.github.com
  Hirb.enable
rescue LoadError
end

class Object
  # list methods which aren't in superclass
  def local_methods(obj = self)
    (obj.methods - obj.class.superclass.instance_methods).sort
  end

  # print documentation
  #
  #   ri 'Array#pop'
  #   Array.ri
  #   Array.ri :pop
  #   arr.ri :pop
  def ri(method = nil)
    unless method && method =~ /^[A-Z]/ # if class isn't specified
      klass = self.kind_of?(Class) ? name : self.class.name
      method = [klass, method].compact.join('#')
    end
    system 'ri', method.to_s
  end

  def sublime(method_name)
    file, line = method(method_name).source_location
    `sublime '#{file}:#{line}'`
  end

  def emacs(method_name)
    file, line = method(method_name).source_location
    `emacs '#{file}'`
  end

end

def copy(str)
  IO.popen('pbcopy', 'w') { |f| f << str.to_s }
end

def copy_history
  history = Readline::HISTORY.entries
  index = history.rindex("exit") || -1
  content = history[(index+1)..-2].join("\n")
  puts content
  copy content
end

def paste
  `pbpaste`
end

load File.dirname(__FILE__) + '/.railsrc' if ($0 == 'irb' && ENV['RAILS_ENV']) || ($0 == 'script/rails' && Rails.env)
