(nil
 '(when-let* ((path (and (string-match (rx (or "/lib/" "/test/")
                                           (group (+? nonl))
                                           (or ".ex" ".exs"))
                                       (buffer-file-name))
                         (match-string 1 (buffer-file-name)))))
    (setq module-name
          (string-join (seq-map #'string-inflection-pascal-case-function
                                (split-string path "/"))
                       ".")))

 "defmodule " module-name " do
  use ExUnit.Case

  " _ "
end"

 '(progn
    (makunbound 'module-name)))
