(defun c++-ide-cmake-check (make-command test-dir)
  "Run the test suite in test-dir"
  (let ((default-directory test-dir)
        (compile-command (concat make-command " check -C " test-dir))) 
    (call-interactively 'compile)))

