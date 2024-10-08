# Key bindings

C-g: reset status
C--: undo
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
C-x 0: kill window/frame
C-x 2: split horizontally
C-x 3: split vertically
S-<arrow>: move to other window
C-x-<left|right>: move to other buffer
C-x b: select buffer
M-x untabify: replace tabs with spaces

M-g M-n: next compile error
M-g M-p: prev compile error

Elisp: C-j at end of elisp expression in scratch buffer

# From packages

### xref
1. Install Universal CTags
2. Run `etags -e -R .` to in roor directory to generate `TAGS` file.

M-.: go to reference or file under cursor.

### Project
C-x p f: fuzzy find file

### Mark Multiple
C-c q: mark-next-like-this

### Expand Region
C-q: expand-region

### Minions
C-.: show all minor modes in menu
