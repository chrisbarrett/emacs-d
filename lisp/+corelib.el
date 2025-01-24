;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun +visible-buffers (&optional buffer-list all-frames)
  "Return a list of visible buffers (i.e. not buried)."
  (let ((buffers
	 (delete-dups
	  (cl-loop for frame in (if all-frames (visible-frame-list) (list (selected-frame)))
		   if (window-list frame)
		   nconc (mapcar #'window-buffer it)))))
    (if buffer-list
	(cl-loop for buf in buffers
		 unless (memq buf buffer-list)
		 collect buffers)
      buffers)))

(defmacro pushnew! (var &rest elements)
  "Add missing ELEMENTS to VAR in-place."
  (let ((gvar (gensym)))
    `(let ((,gvar ',var))
       (set ,gvar
            (seq-union (eval ,gvar) ',elements)))))

(provide '+corelib)
