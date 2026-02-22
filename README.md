# Emacs Configuration

Personal Emacs config built around `straight.el`, `use-package`, and modular `lisp/` features.

## Structure

- `init.el`: bootstrap + module loader.
- `lisp/`: feature modules (`xq-*.el`).
- `AGENTS.md`: repository conventions for future changes.

## Modules

### `xq-ui` (`lisp/xq-ui.el`)

UI and startup defaults:
- disable menu/tool/scroll bars
- enable line and column indicators
- enable global line numbers
- start with maximized frame

### `xq-which-key` (`lisp/xq-which-key.el`)

Enables `which-key` to show available keybindings after prefix keys.

### `xq-evil` (`lisp/xq-evil.el`)

Vim emulation and Evil integrations:
- `evil` for modal editing
- `evil-collection` for Evil keybindings across built-in modes
- split defaults tuned for Vim workflow (`vsplit` right, `split` below)

### `xq-completion` (`lisp/xq-completion.el`)

Completion/search stack and installed plugins:
- `savehist`: persists minibuffer history between sessions.
- `vertico`: vertical minibuffer completion UI.
- `orderless`: flexible fuzzy-like filtering by space-separated patterns.
- `marginalia`: adds rich annotations to minibuffer candidates.
- `consult`: command/search/navigation interface used by leader bindings.

### `xq-org` (`lisp/xq-org.el`)

Daily-note setup with built-in `org`, `org-modern`, and `org-appear`:
- stores Org files in `~/notes`
- creates daily notes in `~/notes/daily/YYYY-MM-DD.org`
- provides `xq/org-open-today-note` to open today's note
- sets basic capture templates for todos and quick notes
- enables `org-modern` for cleaner Org visuals
- enables `org-appear` in manual mode, integrated with Evil insert state

### `xq-keybinds` (`lisp/xq-keybinds.el`)

Special module for leader key definitions.
- `general` defines Evil-style leader keys with `SPC`.
- All plugin and workflow keybindings should be centralized here.

## Keybinds

Current leader key mappings:
- `SPC w` group: window commands
- `SPC w |`: `split-window-right`
- `SPC w -`: `split-window-below`
- `SPC w h`: `windmove-left`
- `SPC w j`: `windmove-down`
- `SPC w k`: `windmove-up`
- `SPC w l`: `windmove-right`
- `SPC s` group: search commands
- `SPC s s`: `consult-line`
- `SPC s g`: `consult-ripgrep`
- `SPC s b`: `consult-buffer`
- `SPC s f`: `consult-find`
- `SPC s i`: `consult-imenu`
- `SPC p` group: project commands
- `SPC p p`: `project-switch-project`
- `SPC p f`: `project-find-file`
- `SPC p d`: `project-find-dir`
- `SPC n` group: notes commands
- `SPC n n`: `xq/org-open-today-note`
- `SPC n i`: `xq/org-open-inbox`
- `SPC n a`: `org-agenda`
- `SPC n c`: `org-capture`

## Startup Flow

`init.el` loads modules in this order:
1. `xq-ui`
2. `xq-which-key`
3. `xq-evil`
4. `xq-completion`
5. `xq-org`
6. `xq-keybinds`
