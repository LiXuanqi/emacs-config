# Repository Guidelines

## Project Structure & Module Organization
This repo is an Emacs config using `straight.el` for package management.
- `init.el`: bootstrap `straight.el`, load core settings, and declare packages.
- `early-init.el` (optional): startup/UI defaults that must run before package init.
- `lisp/`: local modules grouped by concern (for example, `lisp/ui.el`, `lisp/lang-python.el`).
- `tests/`: `ert` tests (`*-test.el` naming).
- `straight/`: package checkouts and build metadata (generated).

Keep generated caches and machine-local artifacts out of version control.

## Build, Test, and Development Commands
Use batch Emacs commands for reproducible checks:
- `emacs --batch -Q -l init.el`: verify config loads without interactive startup state.
- `emacs --batch -Q -L . -L lisp -l tests/run-tests.el -f ert-run-tests-batch-and-exit`: run automated tests.
- `emacs --batch -Q -f batch-byte-compile lisp/*.el`: byte-compile local modules.
- `emacs --batch -Q -l init.el --eval "(straight-freeze-versions)"`: pin package commits.
- `emacs --batch -Q -l init.el --eval "(straight-thaw-versions)"`: restore pinned package commits.

If you add a `Makefile`, keep targets thin wrappers around these commands.

## Coding Style & Naming Conventions
Follow standard Emacs Lisp conventions:
- Indentation: 2 spaces, no tabs.
- File names: lower-case with dashes (for example, `editing-tools.el`).
- Symbols: prefix project-specific functions/variables (for example, `lq/enable-ui`).
- Module design: one responsibility per file; expose a clear `provide` feature.

Run `checkdoc` and byte-compilation warnings before opening a PR.

## Testing Guidelines
Use `ert` for behavior tests on custom Lisp modules.
- Place tests under `tests/` and name files `*-test.el`.
- Name tests with scenario-focused names (for example, `lq/ui-mode-line-hides-in-terminal`).
- Cover startup-critical paths: package bootstrap, keybindings, and module loading.

## Commit & Pull Request Guidelines
Repository history is new; start with consistent conventions now.
- Commit format: `area: change` (for example, `ui: simplify mode-line`).
- One logical change per commit; include migration notes when behavior changes.
- PRs should include: summary, test evidence (commands run), and screenshots/GIFs for visible UI changes.

## Security & Configuration Tips
Do not commit secrets, machine-local absolute paths, or auth tokens.
- Load private values from environment variables or separate local files excluded via `.gitignore`.
- Keep `custom-file` separate from `init.el` to reduce noisy diffs.
- Review `straight/versions/default.el` changes carefully before merging dependency updates.
