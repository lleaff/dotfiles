# Your init script
#
# Atom will evaluate this file each time a new window is opened. It is run
# after packages are loaded/activated and after the previous editor state
# has been restored.
#
# An example hack to log to the console when each text editor is saved.
#
# atom.workspace.observeTextEditors (editor) ->
#   editor.onDidSave ->
#     console.log "Saved! #{editor.getPath()}"

Prefixes = require './packages/vim-mode/lib/prefixes'
Motions = require './packages/vim-mode/lib/motions/index'
VimMode = require './packages/vim-mode'

MoveByLines = (num) ->
  editor = atom.workspace.getActiveTextEditor()
  vimState = VimMode.getEditorState(editor)
  repeat = new Prefixes.Repeat(Math.abs(num))
  if num > 0
    move = new Motions.MoveDown(editor, vimState)
  else
    move = new Motions.MoveUp(editor, vimState)
  vimState.pushOperations([repeat, move])

atom.commands.add 'atom-text-editor', 'custom:move-five-up',   -> MoveByLines(-5)
atom.commands.add 'atom-text-editor', 'custom:move-five-down', -> MoveByLines(5)
