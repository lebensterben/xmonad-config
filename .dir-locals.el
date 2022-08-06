;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((format-all-formatters . (("Haskell" brittany)
                                   ("Cabal Config" cabal-fmt)
                                   ("YAML" prettier)
                                   ("Emacs Lisp" emacs-lisp)))
         (eval . (when (fboundp 'xah-syntax-color-hex) (eval '(xah-syntax-color-hex))))))
 (haskell-cable-mode . ((format-all-mode . t)))
 (haskell-mode . ((fill-column . 100)
                  (format-all-mode . t))))
