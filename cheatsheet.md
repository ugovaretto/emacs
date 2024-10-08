# Configuration

Use `~/.config/emacs` as root directory on Mac an Linux.
Use `use-package` to install and configure packages.
Put `.dir-locals.el` in any directory to create per-folder/project configuarations.
`early-init.el` is run before `init.el` if present.
(electric-pair-mode): automatically add matching parentheses.

### Best themes: 

* zenburn (low contrast)
* doom-zenburn (low contras, higher contrast than zenburn)
* doom-rouge (high contrast)
* misterioso (mid contrast)
* modus-vivendi (high contrast)

# Key bindings

C-g: reset status
C-/: undo
C-S-/: redo
M-y: browse kill ring
C-S: select
C-x S: select columns
C-w: kill
M-w: copy
M-f: move one word forward
M-b: move one word backward
C-v: scroll down
M-v: scroll up
M-<: move to beginning of buffer
M->: move to end of buffer
C-s: search
M-s w: fuzzy find word, typing C-s will repeat the search
M-g g: go to line
C-x-k: kill buffer
C-x 0: kill current window
C-x 1: make current window fullscreen
C-x 2: split horizontally
C-x 3: split vertically
S-<arrow>: move to other window
C-x o: cycle through windows
C-x-<left|right>: move to other buffer
C-x b: select buffer
M-x untabify: replace tabs with spaces
C-u <number>: repeat following command <number> times

M-g M-n: next compile error
M-g M-p: prev compile error

*Mark*
C-S-<left|right>: mark from current position to beginning of work at left or right
C-S-<up>: mark from current position to beginning of line above or beginning
of current line if no line above exists
C-S-<down>: mark from current position to end of line below or end of current line
if no line below exists

*Tabs*
C-x t 2: add tab
C-x t 0: close current tab
C-x t 1: keep only current tab
C-x t o: next tab


Elisp: C-j at end of elisp expression in scratch buffer


## Dired
C-x d
C-x d *.cpp only list file ending with ".cpp"
d: flag for deletion
x: delete flagged files
c: compress
C: copy
C-x-q: toggle buffer writable (to e.g. rename multiple files)

# From external packages

### xref
1. Install Universal CTags
2. Run `etags -e -R .` to in roor directory to generate `TAGS` file.

M-.: go to reference or file under cursor. Works with LSP as well without explicit TAG generation.

### Project
Add `.project.el` or `.projectile` file in root directory to turn
directory into a project folder.
C-x p f: fuzzy find file
C-x p p: list projects

### Mark Multiple
C-c q: mark-next-like-this

### Expand Region
C-q: expand-region

### Minions
C-.: show all minor modes in menu

### Embark
C-.: on target will show all the relevant actions
that can be performed on that target e.g. on  C++
class variables it shows the keys to use to "go to definition" or "show references" etc.

### Which key
No keybindings just press on any key combination and wait for a second to
display the list of possible keybindings e.g. 'C-x-' will show 'p'
for project/projectile, 'f' for search file etc.

### Consult
consult-imenu: show list of symbols e.g. in c++ all functions or classes.
consult-flymake: go to errors/warnings.
consult-theme: change theme.

### Yasnippet
yas/new-snippet: new snippet
yas/visit-snippet-file: edit snippet file.
C-e: expand snippet.
