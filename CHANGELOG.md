# Changelog

## master (unreleased)

## 0.7.0 (2023-03-09)

### New features

- Added `imenu` support.
- Associate with `.adoc` and `.asciidoc` files automatically.

### Changes

- Require Emacs 26.
- Respect `mode-require-final-newline`.
- [https://github.com/bbatsov/adoc-mode/issues/25]: Remove `markup-faces` dependency.

### Bugs fixed

- Handle `unichars.el` properly.
- Add missing quote before `adoc-reserved` in `adoc-kw-verbatim-paragraph-sequence`.
- [https://github.com/bbatsov/adoc-mode/issues/17]: Show only titles in `imenu`.
