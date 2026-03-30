# Emacs Configuration

Personal Emacs config built around `straight.el`, `use-package`, and modular `lisp/` features.

## Structure

- `init.el`: bootstrap + module loader.
- `Makefile`: thin wrappers for common batch checks and cleanup.
- `lisp/`: feature modules (`xq-*.el`).
- `lisp/xq-overrides.el`: shared helper for machine-local overrides and module gating.
- `lisp/xq-local-example.el`: example snippets for local override files.
- `tests/`: `ert` coverage for local helper behavior.
- `AGENTS.md`: repository conventions for future changes.

## System Dependencies

Some Emacs packages in this config require external tools installed on your system.

- `lisp/xq-terminal.el` (`vterm`):
  - `cmake`
  - `libtool` (provides `glibtool` on macOS)
- `lisp/xq-lang.el` (`lsp-mode` Python server):
  - `pyright` (usually via `npm install -g pyright`)
- `lisp/xq-format.el` (Python auto-format on save via `apheleia`):
  - `ruff` (usually via `pipx install ruff` or `pip install ruff`)
- `lisp/xq-lang.el` (`lsp-mode` TypeScript/TSX server):
  - `typescript-language-server`
  - `typescript` (usually via `npm install -g typescript typescript-language-server`)
- `lisp/xq-lang.el` (`lsp-mode` Go server):
  - `gopls` (usually via `go install golang.org/x/tools/gopls@latest`)
- `lisp/xq-lang.el` (`lsp-mode` C++ server):
  - `clangd` (usually via your package manager, for example Homebrew `llvm` on macOS)
- `lisp/xq-keybinds.el` search command (`consult-ripgrep` bound to `SPC s g`):
  - `ripgrep` (`rg`)
- `lisp/xq-treesit.el` (`treesit-install-language-grammar`):
  - compiler toolchain for grammar builds (`cc`/Xcode Command Line Tools on macOS)

## Common Commands

- `make load`: verify `init.el` loads in batch mode.
- `make test`: run `ert` tests.
- `make byte-compile`: byte-compile local modules.
- `make freeze`: write the current `straight.el` lockfile.
- `make thaw`: restore packages from the lockfile.
- `make clean-elc`: delete local `.elc` files outside `straight/`.

## Modules

### `xq-defaults` (`lisp/xq-defaults.el`)

General defaults for most users:
- on macOS GUI Emacs, initialize `exec-path-from-shell` when available
- stores backup files (`filename~`) under `~/.config/emacs/backups/`
- stores auto-save files under `~/.config/emacs/auto-saves/`
- uses short confirmation prompts (`y`/`n` instead of `yes`/`no`)
- keeps project working trees clean of editor-generated files

### `xq-overrides` (`lisp/xq-overrides.el`)

Machine-local override support:
- defines the ordered `xq/modules` load list used by `init.el`
- loads optional `lisp/xq-local-pre.el` before shared modules
- loads optional `lisp/xq-local-post.el` after shared modules
- provides helpers to enable/disable modules per machine

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
- `C-u` scrolls up in Evil normal/motion states
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
2. Enter grammar name (repeat for each language you want): `python`, `c`, `cpp`, `bash`, `json`, `yaml`, `toml`, `css`, `javascript`, `typescript`, `tsx`, `go`, `rust`.
3. Restart Emacs after installation.

### `xq-lang` (`lisp/xq-lang.el`)

Language/LSP entrypoint:
- configures `lsp-mode` defaults
- configures `lsp-pyright` integration
- sets `lsp-file-watch-threshold` to `10000`
- associates `.ts` and `.tsx` files with built-in TypeScript tree-sitter modes
- lazy-loads language-specific modules only when matching major modes start

Language-specific modules:
- `lisp/xq-lang-python.el`: Python defaults + `lsp-deferred`, plus Pyright and Python project-root config
- `lisp/xq-lang-go.el`: Go defaults + `lsp-deferred`, plus Go project-root config
- `lisp/xq-lang-cpp.el`: C++ defaults + `lsp-deferred`, plus CMake/compile commands project-root config
- `lisp/xq-lang-rust.el`: Rust defaults
- `lisp/xq-lang-typescript.el`: TypeScript/TSX defaults + `lsp-deferred`

### `xq-format` (`lisp/xq-format.el`)

Formatting workflow:
- enables `apheleia` format-on-save for Python buffers (`python-mode`, `python-ts-mode`)
- uses `ruff format` as the Python formatter
- logs formatter errors only (`apheleia-log-only-errors`)

### `xq-git` (`lisp/xq-git.el`)

Git workflow setup:
- enables `magit` commands
- opens status in a practical single-window layout
- sets commit summary max length to 72 characters

### `xq-code-anchor` (`lisp/xq-code-anchor.el`)

Code hosting links from the current buffer location:
- installs `code-anchor` from GitHub via `straight.el`
- defaults `code-anchor-open` to GitHub links
- provides commands to open the current line or active region on GitHub or Sourcegraph

### `xq-terminal` (`lisp/xq-terminal.el`)

Terminal workflow improvements:
- project-root `vterm` command
- `vterm` as default terminal workflow
- increased terminal scrollback for longer sessions
- keeps `vterm` on native `vterm` bindings instead of Evil bindings
- disables local Evil state inside `vterm` buffers
- `C-c C-z` toggles between terminal input and Emacs/Evil command mode

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
- `SPC l` group: LSP commands
- `SPC l l`: `lsp`
- `SPC l d`: `xref-find-definitions`
- `SPC l D`: `lsp-find-declaration`
- `SPC l i`: `lsp-find-implementation`
- `SPC l t`: `lsp-find-type-definition`
- `SPC l r`: `xref-find-references`
- `SPC l R`: `lsp-rename`
- `SPC l a`: `lsp-execute-code-action`
- `SPC l f`: `lsp-format-buffer`
- `SPC l o`: `lsp-organize-imports`
- `SPC l e`: `flymake-show-buffer-diagnostics`
- `SPC l s`: `lsp-describe-session`
- `SPC g` group: git commands
- `SPC g g`: `magit-status`
- `SPC g l`: `magit-log-current`
- `SPC g b`: `magit-blame-addition`
- `SPC g o` group: open current location in remote forge
- `SPC g o o`: `code-anchor-open`
- `SPC g o g`: `code-anchor-open-github`
- `SPC g o s`: `code-anchor-open-sourcegraph`
- `SPC t` group: terminal commands
- `SPC t t`: `xq/terminal-vterm-here`
- in `vterm`, most keys go directly to the terminal program
- in `vterm`, Emacs keeps `vterm` control prefixes such as `C-c`/`C-x` and commands like `C-c C-t`
- in `vterm`, `C-c C-z` toggles between terminal mode and Emacs/Evil command mode
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

## Machine-Local Overrides

This config now supports two untracked machine-local files:

- `lisp/xq-local-pre.el`: loaded before shared modules. Use this to disable entire modules or set variables that package `:init` blocks should read.
- `lisp/xq-local-post.el`: loaded after shared modules. Use this for last-write-wins overrides after packages have already been configured.

Both files are ignored by Git. Shared config stays in tracked `lisp/xq-*.el` modules; laptop-specific behavior stays local.

Typical setup:

```elisp
;; lisp/xq-local-pre.el
(setq xq/machine
      (pcase (system-name)
        ("work-mbp" 'work)
        ("home-mbp" 'home)
        (_ 'default)))

(when (eq xq/machine 'work)
  (xq/disable-module 'xq-org)
  (setq xq/org-directory (expand-file-name "work-notes" (getenv "HOME"))))
```

```elisp
;; lisp/xq-local-post.el
(when (eq xq/machine 'home)
  (setq doom-modeline-minor-modes t)
  (load-theme 'doom-nord t))
```

For more complicated cases, split plugin defaults so they are easy to override:

- Keep each plugin or concern in its own `xq-*.el` module.
- Put machine-agnostic defaults in the shared module.
- Read user-tunable variables in `:init`, then set those variables from `xq-local-pre.el`.
- Use `xq/disable-module` when a machine should not load a plugin at all.
- Use `xq-local-post.el` when a package must be tweaked after it finishes loading.

Example plugin-specific pattern:

```elisp
;; shared module: lisp/xq-org.el
(defvar xq/org-enabled t
  "Whether Org workflow features should be enabled on this machine.")

(when xq/org-enabled
  (use-package org
    :straight (:type built-in)
    :init
    (setq org-directory xq/org-directory)))
```

```elisp
;; machine-local pre-init file
(setq xq/org-enabled (not (eq xq/machine 'work)))
```

That gives you two levels of control:

- module-level: load or skip a whole feature module
- plugin-level: keep the module but vary package settings or package activation by machine

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

## Vterm Workflow

`vterm` does not use `evil-collection-vterm` in this config.

Key routing in `vterm`:
- sent to the terminal program: normal typing, `ESC`, `RET`, `TAB`, arrows, and most `C-...` / `M-...` keys
- handled by Emacs/`vterm`: reserved prefixes and `vterm` commands such as `C-c`, `C-x`, `C-u`, `C-g`, `C-h`, `C-l`, `M-x`, `M-o`, `C-y`, `M-y`

Useful `vterm` commands that stay in Emacs:
- `C-c C-z`: toggle between terminal mode and Emacs/Evil command mode
- `C-c C-t`: toggle `vterm-copy-mode`
- `C-c C-r`: reset cursor point
- `C-c C-n` / `C-c C-p`: next/previous prompt
- `C-c C-l`: clear scrollback

When `C-c C-z` switches to command mode, local Evil is enabled in the `vterm`
buffer and leader mappings such as `SPC w |` work again. Press `C-c C-z` a
second time to return to normal terminal input.

## Startup Flow

`init.el` loads modules in this order:
1. `xq-defaults`
2. `xq-ui`
3. `xq-which-key`
4. `xq-evil`
5. `xq-completion`
6. `xq-treesit`
7. `xq-lang`
8. `xq-format`
9. `xq-git`
10. `xq-code-anchor`
11. `xq-terminal`
12. `xq-org`
13. `xq-keybinds`
