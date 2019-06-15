require 'irb/completion'
require 'irb/ext/save-history'

IRB.conf.tap do |c|
    c[:SAVE_HISTORY]  = 1000
      c[:AUTO_INDENT]   = true
        c[:PROMPT_MODE]   = :SIMPLE
end
