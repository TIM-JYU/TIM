# Start from Ubuntu image
from haskell:7.8
maintainer Ville Tirronen "ville.tirronen@jyu.fi"
  
run apt-get update
run apt-get install -y graphviz
env LANG C.UTF-8
env LC_ALL C.UTF-8

run cabal update

# Add all Haskell files first
#add AnnotatedCode/Annotate.hs /AnnotatedCode/
#add AnnotatedCode/LICENSE /AnnotatedCode/
#
#add Choices/Choice.cabal /Choices/
#add Choices/Choices.hs /Choices/
#add Choices/ChoicesPlugin.hs /Choices/
#add Choices/GraphVizPlugin.hs /Choices/
#add Choices/MultipleChoicesPlugin.hs /Choices/
#add Choices/ShortNotePlugin.hs /Choices/
#add Choices/LICENSE /Choices/
#
#add Interpreter/Interpreter.cabal /Interpreter/
#add Interpreter/Interpreter.hs /Interpreter/
#add Interpreter/InterpreterPlugin.hs /Interpreter/
#add Interpreter/LICENSE /Interpreter/
#
#add PluginConstructionKit/PluginConstructionKit.cabal /PluginConstructionKit/
#add PluginConstructionKit/AnswerRequest.hs /PluginConstructionKit/
#add PluginConstructionKit/HTMLRequest.hs /PluginConstructionKit/
#add PluginConstructionKit/PluginType.hs /PluginConstructionKit/
#add PluginConstructionKit/UtilityPrelude.hs /PluginConstructionKit/
#add PluginConstructionKit/LICENSE /PluginConstructionKit/
#
#add AnnotatedCode/Annotate.hs /AnnotatedCode/
#add AnnotatedCode/Annotate.hs /AnnotatedCode/
#add AnnotatedCode/Annotate.hs /AnnotatedCode/
#
#run cabal install PluginConstructionKit/ Choices/
#
## Add HTML templates and script files
#add AnnotatedCode/style.css /AnnotatedCode/
#
#add Choices/MCQTemplate.html /Choices/
#add Choices/MMCQTemplate.html /Choices/
#add Choices/ShortNoteTemplate.html /Choices/
#add Choices/script.js /Choices/
#add Choices/script2.js /Choices/
#add Choices/shortNote.js /Choices/
#add Choices/SimpleDirective.js /Choices/
#
#add Interpreter/NewConsole/style.css /Interpreter/NewConsole/
#add Interpreter/NewConsole/Console.template.html /Interpreter/NewConsole/
#add Interpreter/NewConsole/Console.js /Interpreter/NewConsole/

expose 5000

