;;; efrit-tool-rails.el --- Rails development tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge
;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, rails
;; Version: 0.4.1

(require 'efrit-tool-utils)
(require 'cl-lib)

(defun efrit-tool-rails--is-rails-p (&optional root)
  (let ((root (or root (efrit-tool--get-project-root))))
    (or (file-exists-p (expand-file-name "config/application.rb" root))
        (file-exists-p (expand-file-name "config/environment.rb" root)))))

(defun efrit-tool-rails--is-ruby-p (&optional root)
  (let ((root (or root (efrit-tool--get-project-root))))
    (or (file-exists-p (expand-file-name "Gemfile" root))
        (file-exists-p (expand-file-name ".ruby-version" root)))))

(defun efrit-tool-rails--get-ruby-version (&optional root)
  (let ((root (or root (efrit-tool--get-project-root))))
    (cond
     ((file-exists-p (expand-file-name ".ruby-version" root))
      (with-temp-buffer
        (insert-file-contents (expand-file-name ".ruby-version" root))
        (string-trim (buffer-string))))
     ((executable-find "ruby")
      (with-temp-buffer
        (call-process "ruby" nil t nil "-v")
        (and (re-search-forward "ruby \\([0-9.]+\\)" nil t)
             (match-string 1 (buffer-string))))))))

(defun efrit-tool-rails--get-rails-version (&optional root)
  (let ((root (or root (efrit-tool--get-project-root))))
    (when (efrit-tool-rails--is-rails-p root)
      (when (file-exists-p (expand-file-name "Gemfile" root))
        (with-temp-buffer
          (insert-file-contents (expand-file-name "Gemfile" root))
          (and (re-search-forward "gem [\'\"]rails[\'\"], [\'\"]\\([~>0-9.]+\\)[\'\"]" nil t)
               (match-string 1 (buffer-string))))))))

(defun efrit-tool-rails--list-routes (&optional root)
  (let ((root (or root (efrit-tool--get-project-root))))
    (when (and (efrit-tool-rails--is-rails-p root)
               (executable-find "rails"))
      (let ((default-directory root))
        (with-temp-buffer
          (call-process "rails" nil t nil "routes")
          (goto-char (point-min))
          (let (routes)
            (while (not (eobp))
              (let ((line (string-trim (buffer-substring (point) (line-end-position)))))
                (when (and (not (string-prefix-p "Prefix" line))
                           (not (string-prefix-p "---" line))
                           (not (string-empty-p line)))
                  (when (string-match "^\\([^ ]+\\) *\\([^ ]+\\) *\\(.+\\)$" line)
                    (push (list (cons "verb" (match-string 1 line))
                                (cons "path" (match-string 2 line))
                                (cons "controller_action" (match-string 3 line)))
                          routes))))
              (forward-line 1))
            (reverse routes)))))))

(defun efrit-tool-rails--list-migrations (&optional root)
  (let ((root (or root (efrit-tool--get-project-root))))
    (when (efrit-tool-rails--is-rails-p root)
      (let ((default-directory root)
            (applied nil))
        (let ((migrate-dir (expand-file-name "db/migrate" root)))
          (when (file-directory-p migrate-dir)
            (dolist (f (directory-files migrate-dir t "^[0-9]+_.*\\.rb$"))
              (push (cons "version" (file-name-nondirectory f)) applied))))
        (list (cons "applied" (reverse applied))
              (cons "pending" nil))))))

(defun efrit-tool-rails--discover-tests (&optional root)
  (let ((root (or root (efrit-tool--get-project-root))))
    (when (or (efrit-tool-rails--is-rails-p root)
              (efrit-tool-rails--is-ruby-p root))
      (let (tests)
        (dolist (dir (list (expand-file-name "spec" root)
                           (expand-file-name "test" root)))
          (when (file-directory-p dir)
            (dolist (f (directory-files-recursively dir "\\.rb$"))
              (push (cons "path" (file-relative-name f root)) tests))))
        (reverse tests)))))

(defun efrit-tool-rails--run-rubocop (&optional path root)
  (let ((root (or root (efrit-tool--get-project-root))))
    (when (executable-find "rubocop")
      (let ((default-directory root)
            (target (if (stringp path) path ".")))
        (with-temp-buffer
          (call-process "rubocop" nil t nil target "--format" "json")
          (buffer-string))))))

(defun efrit-tool-rails (args)
  "Execute Rails/Ruby development commands.
ARGS: action (required) - routes, migrations, tests, version, rubocop, bundle
      path (optional) - path for rubocop"
  (efrit-tool-execute rails args
    (let* ((action (or (alist-get 'action args) ""))
           (path-input (alist-get 'path args))
           (path-info (efrit-resolve-path path-input))
           (path (plist-get path-info :path))
           (project-root (plist-get path-info :project-root)))
      (cond
       ((string= action "routes")
        (unless (efrit-tool-rails--is-rails-p project-root)
          (signal 'user-error (list "Not a Rails project")))
        (let ((routes (efrit-tool-rails--list-routes project-root)))
          (efrit-tool-success
           `((action . "routes")
             (routes . ,(vconcat routes))
             (count . ,(length routes))))))
       ((string= action "migrations")
        (unless (efrit-tool-rails--is-rails-p project-root)
          (signal 'user-error (list "Not a Rails project")))
        (let ((migrations (efrit-tool-rails--list-migrations project-root)))
          (efrit-tool-success
           `((action . "migrations")
             (applied . ,(vconcat (or (cdr (assoc "applied" migrations)) [])))
             (pending . ,(vconcat (or (cdr (assoc "pending" migrations)) [])))
             (applied_count . ,(length (cdr (assoc "applied" migrations))))
             (pending_count . ,(length (cdr (assoc "pending" migrations))))))))
       ((string= action "tests")
        (unless (or (efrit-tool-rails--is-rails-p project-root)
                    (efrit-tool-rails--is-ruby-p project-root))
          (signal 'user-error (list "Not a Ruby or Rails project")))
        (let ((tests (efrit-tool-rails--discover-tests project-root)))
          (efrit-tool-success
           `((action . "tests")
             (tests . ,(vconcat tests))
             (count . ,(length tests))))))
       ((string= action "version")
        (let ((ruby-ver (efrit-tool-rails--get-ruby-version project-root))
              (rails-ver (efrit-tool-rails--get-rails-version project-root))
              (is-rails (efrit-tool-rails--is-rails-p project-root))
              (is-ruby (efrit-tool-rails--is-ruby-p project-root)))
          (efrit-tool-success
           `((action . "version")
             (ruby_version . ,(or ruby-ver ""))
             (rails_version . ,(or rails-ver ""))
             (is_rails . ,(if is-rails t :json-false))
             (is_ruby . ,(if is-ruby t :json-false))))))
       ((string= action "rubocop")
        (unless (efrit-tool-rails--is-ruby-p project-root)
          (signal 'user-error (list "Not a Ruby project")))
        (let ((results (efrit-tool-rails--run-rubocop path project-root)))
          (efrit-tool-success
           `((action . "rubocop")
             (results . ,results)))))
       ((string= action "bundle")
        (unless (efrit-tool-rails--is-ruby-p project-root)
          (signal 'user-error (list "Not a Ruby project")))
        (efrit-tool-success
         `((action . "bundle")
           (message . "Use shell_exec to run bundle commands"))))
       (t (signal 'user-error (list (format "Unknown rails action: %s" action))))))))

(provide 'efrit-tool-rails)
