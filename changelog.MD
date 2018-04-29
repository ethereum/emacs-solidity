# Emacs Solidity Mode Changelog

The changelog starts from version 0.1.4 as too much was added in each version before that.

## Nightly

- Added the interface keyword (https://github.com/ethereum/emacs-solidity/pull/27).

- Fixed flycheck integration doesn't search for solc and solium in PATH.

- Added missing keywords:

  - constructor

  - payable

## Version 0.1.9

- Integrated [company-solidity](https://github.com/ssmolkin1/company-solidity) into solidity-mode, providing autocompletion out of the box if the user has [company-mode](http://company-mode.github.io) installed.

## Version 0.1.8

- Bugfix for [issue 8](https://github.com/ethereum/emacs-solidity/issues/8). Now if flycheck is not installed
on the user's system there should be no errors or warnings.

## Version 0.1.7

- Allow for customization of the way that emacs inserts comments into the source file via

```emacs-lisp
(setq solidity-comment-style 'slash)
;; or
(setq solidity-comment-style 'star) ;; this is the default
```

## Version 0.1.6

- Add gas estimation code. User facing function is `solidity-estimate-gas-at-point`.

## Version 0.1.5

- Add ability to chain flycheck checkers for solidity.
- Add `solidity-flycheck-chaining-error-level` so that user can customize
  the level at which chaining will happen.

## Version 0.1.4

- Integrate with the solium linter
- Allow specification of solium config file via `flycheck-solidity-solium-soliumrcfile`

