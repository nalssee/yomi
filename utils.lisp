(in-package :ynb)

(defun change-directory (dir)
  (let ((dir (fad:pathname-as-directory dir)))
    (if (fad:directory-exists-p dir)
	(setf yomi:*working-directory* dir)
	(error "~A doesn't exist" dir))))


