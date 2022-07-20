# Changelog

## master (unreleased)

### New features

- Added `imenu` support.
- Associate with `.adoc` and `.asciidoc` files automatically.

### Changes

- Require Emacs 26.
- Respect `mode-require-final-newline`.

### Bugs fixed

- Handle `unichars.el` properly.
- Add missing quote before `adoc-reserved` in `adoc-kw-verbatim-paragraph-sequence`.
