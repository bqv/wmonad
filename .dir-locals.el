;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . (
        ;(eval . (set (make-local-variable 'default-directory)
        ;             (locate-dominating-file buffer-file-name ".dir-locals.el")))
         (eval . (setq dante-repl-command-line
                       '("nix" "develop" "--impure" "-c" "cabal" "v1-repl" (or dante-target (dante-package-name) "") "--builddir=dist/dante")))
         (eval . (setq projectile-project-compilation-cmd
                       "nix develop -c cabal v1-build"))
         (eval . (setq projectile-project-test-cmd
                       "nix develop -c /run/wrappers/bin/swc-launch -t /dev/tty10 -- cabal v1-run"))
         )))
