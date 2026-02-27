# Emacs Configuration

Personal Emacs config built around `straight.el`, `use-package`, and modular `lisp/` features.

## Structure

- `init.el`: bootstrap + module loader.
- `lisp/`: feature modules (`xq-*.el`).
- `AGENTS.md`: repository conventions for future changes.

## System Dependencies

Some Emacs packages in this config require external tools installed on your system.

- `lisp/xq-terminal.el` (`vterm`):
  - `cmake`
  - `libtool` (provides `glibtool` on macOS)
- `lisp/xq-lang.el` (`eglot` Python server):
  - `pyright-langserver` (usually via `npm install -g pyright`)
- `lisp/xq-lang.el` (`eglot` Rust server):
  - `rust-analyzer`
- `lisp/xq-keybinds.el` search command (`consult-ripgrep` bound to `SPC s g`):
  - `ripgrep` (`rg`)
- `lisp/xq-treesit.el` (`treesit-install-language-grammar`):
  - compiler toolchain for grammar builds (`cc`/Xcode Command Line Tools on macOS)

## Modules

### `xq-defaults` (`lisp/xq-defaults.el`)

General defaults for most users:
- on macOS GUI Emacs, initialize `exec-path-from-shell` when available
- stores backup files (`filename~`) under `~/.config/emacs/backups/`
- stores auto-save files under `~/.config/emacs/auto-saves/`
- keeps project working trees clean of editor-generated files

### `xq-ui` (`lisp/xq-ui.el`)

UI configuration:
- startup/UI defaults: disable menu/tool/scroll bars, enable line/column indicators, enable global line numbers, start maximized
- theme via `doom-themes` (`doom-one`)
- mode line via `doom-modeline` with compact defaults

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

### `xq-treesit` (`lisp/xq-treesit.el`)

Tree-sitter integration:
- sets common grammar sources for `M-x treesit-install-language-grammar`
- prefers `*-ts-mode` for supported languages when tree-sitter is available
- falls back to classic modes automatically when tree-sitter is unavailable

Tree-sitter setup on a new machine (after cloning this repo):
1. Run `M-x treesit-install-language-grammar`.
2. Enter grammar name (repeat for each language you want): `python`, `bash`, `json`, `yaml`, `toml`, `css`, `javascript`, `typescript`, `tsx`, `go`, `rust`.
3. Restart Emacs after installation.

### `xq-lang` (`lisp/xq-lang.el`)

Language/LSP entrypoint:
- configures `eglot` defaults
- adds `pyproject.toml` as a `project.el` VC project root marker
- adds `xq/eglot-server-memory-mb` to tune memory for Node-based language servers
- configures Pyright via `eglot-workspace-configuration` to:
  - use `openFilesOnly` diagnostics
  - exclude common heavy directories (`.venv`, `node_modules`, `dist`, `build`, etc.)

Example tuning in your config:
```elisp
;; Increase Node.js LSP heap (e.g., Pyright) to 4 GB.
(setq xq/eglot-server-memory-mb 4096)

;; Or disable explicit memory tuning and use server defaults.
;; (setq xq/eglot-server-memory-mb nil)
```
- sets language server mappings for Python and Rust
- lazy-loads language-specific modules only when matching major modes start

Language-specific modules:
- `lisp/xq-lang-python.el`: Python defaults + `eglot-ensure`
- `lisp/xq-lang-rust.el`: Rust defaults + `eglot-ensure`

### `xq-git` (`lisp/xq-git.el`)

Git workflow setup:
- enables `magit` commands
- opens status in a practical single-window layout
- sets commit summary max length to 72 characters

### `xq-terminal` (`lisp/xq-terminal.el`)

Terminal workflow improvements:
- project-root `vterm` command
- `vterm` as default terminal workflow
- increased terminal scrollback for longer sessions

### `xq-org` (`lisp/xq-org.el`)

Daily-note setup with built-in `org`, `org-modern`, `org-appear`, and `org-roam`:
- stores Org files in `~/notes`
- stores roam notes in `~/notes/roam`
- uses `org-roam-dailies` (`daily/`) for date-based daily notes
- enables `org-modern` for cleaner Org visuals
- enables `org-appear` in manual mode, integrated with Evil insert state
- enables `org-roam-db-autosync-mode`
- enables completion integration via `org-roam-completion-everywhere`

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
- `SPC g` group: git commands
- `SPC g g`: `magit-status`
- `SPC g l`: `magit-log-current`
- `SPC g b`: `magit-blame-addition`
- `SPC t` group: terminal commands
- `SPC t t`: `xq/terminal-vterm-here`
- `SPC n` group: notes commands
- `SPC n n`: `org-roam-dailies-goto-today`
- `SPC n a`: `org-agenda`
- `SPC n r` group: org-roam commands
- `SPC n r f`: `org-roam-node-find`
- `SPC n r i`: `org-roam-node-insert`
- `SPC n r c`: `org-roam-capture`
- `SPC n r t`: `org-roam-dailies-capture-today` (daily TODO)
- `SPC n r b`: `org-roam-buffer-toggle`

## Org Workflow

Notes folder layout:

```text
~/notes/
├── daily/
│   └── YYYY-MM-DD.org
└── roam/
    └── *.org
```

- `~/notes/`: base Org directory (`org-directory`).
- `~/notes/roam/`: org-roam knowledge notes (`org-roam-directory`).
- `~/notes/daily/`: org-roam daily files (`org-roam-dailies-directory`).
- `~/notes/daily/YYYY-MM-DD.org`: one daily note per date.

Daily notes (`org-roam-dailies`):
- Open today: `SPC n n` (`org-roam-dailies-goto-today`)
- Capture TODO to today: `SPC n r t` (`org-roam-dailies-capture-today`)
- Open a past/future date: `M-x org-roam-dailies-goto-date`
- Quick jumps:
  - `M-x org-roam-dailies-goto-yesterday`
  - `M-x org-roam-dailies-goto-tomorrow`

Roam knowledge notes:
- Find/create node: `SPC n r f`
- Insert node link: `SPC n r i`
- Capture node: `SPC n r c`
- Toggle backlinks buffer: `SPC n r b`

Org editing tips:
- Insert source block quickly: type `<s` then press `TAB` (enabled by `org-tempo`).
- Alternative: `C-c C-, s`.

## Future Ideas

- Add `avy` for fast jump-to-char/word/line motions (for example, leader map under `SPC j`).
- Add `evil-snipe` for enhanced Vim-style quick character motions.
- Add machine-local override loading (`local.el`) for per-laptop custom settings.

## Magit Workflow

Basic day-to-day flow:
1. Open status: `SPC g g`.
2. Review changes in status buffer sections (`unstaged`, `staged`, `untracked`).
3. Stage files/hunks with `s` (unstage with `u`).
4. Open diff at point with `TAB` or `RET`.
5. Commit with `c c`, write message, then `C-c C-c`.
6. Push with `P p`.
7. Pull/rebase updates with `F u`.

Useful extras:
- View current-file history: `SPC g l`.
- Blame current file/line context: `SPC g b` (toggle off with `q` in blame view).

## Startup Flow

`init.el` loads modules in this order:
1. `xq-defaults`
2. `xq-ui`
3. `xq-which-key`
4. `xq-evil`
5. `xq-completion`
6. `xq-treesit`
7. `xq-lang`
8. `xq-git`
9. `xq-terminal`
10. `xq-org`
11. `xq-keybinds`
