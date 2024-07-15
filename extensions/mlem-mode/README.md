# `mlem-mode`

A hungry deletion minor mode for the Lem editor.

![mlem](./mlem.gif)

## Description

`mlem-mode` is a minor mode that implements "hungry" deletion. When enabled, deleting whitespace will delete all consecutive whitespace until the next non-whitespace character is reached. 

## Usage

Simply enable `mlem-mode` with `<M-x> mlem-mode`. The default keybinds for deletion, `<Backspace>` and `<Delete>`, will now be hungry.

The default, non-hungry (fed) binds are available with `<M-Backspace>` and `<M-Delete>`.