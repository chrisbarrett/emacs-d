(
 ";" ;; First element is ignored if it is a comment for some reason.
 ";;; " (file-name-nondirectory (buffer-file-name)) " --- DESC -*- lexical-binding: t; -*-" \n
 \n
 ";;; Commentary:" \n
 \n
 ";;; Code:" \n
 \n
 _ \n
 \n
 "(provide '" (file-name-sans-extension (file-name-nondirectory (buffer-file-name))) ")" \n
 \n 
 ";;; " (file-name-nondirectory (buffer-file-name)) " ends here"
 )
