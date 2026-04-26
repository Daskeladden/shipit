;;; shipit-comments.el --- Comment utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2025 shipit contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; This file is part of shipit — code review integration for Magit.

;;; Commentary:
;; Comment utilities for shipit including:
;; - Thread resolution tracking
;; - Comment thread navigation
;; - Comment count utilities

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'shipit-lib)
(require 'shipit-comment-backends)

;; Forward declarations
(declare-function shipit--debug-log "shipit-debug")

;; External variables from shipit-core
(defvar shipit--reaction-cache)

;; External variables from shipit-http
(declare-function shipit--reaction-cache-key "shipit-http")

;; External variables from shipit-cache
(defvar shipit--cached-resolved-threads)

;;; Resolved Comments Hash Table
;; Provides O(1) lookup for whether a comment is in a resolved thread.

(defvar shipit--resolved-comments-hash nil
  "Hash table mapping comment IDs to resolved status for fast lookup.")

(defun shipit--build-resolved-comments-hash ()
  "Build hash table of resolved comment IDs for O(1) lookup."
  (when (and (boundp 'shipit--cached-resolved-threads) shipit--cached-resolved-threads)
    (let ((hash-table (make-hash-table :test 'equal))
          (total-resolved 0)
          (resolved-ids '()))
      (dolist (thread shipit--cached-resolved-threads)
        (let ((is-resolved (cdr (assq 'isResolved thread)))
              (comments-obj (cdr (assq 'comments thread))))
          (when (and (eq is-resolved t) comments-obj)
            (let ((comment-nodes (cdr (assq 'nodes comments-obj))))
              (when comment-nodes
                (dolist (comment-node comment-nodes)
                  (let ((database-id (cdr (assq 'databaseId comment-node))))
                    (when database-id
                      (puthash database-id t hash-table)
                      (push database-id resolved-ids)
                      (setq total-resolved (1+ total-resolved))))))))))
      (setq shipit--resolved-comments-hash hash-table))))

(defun shipit--is-comment-in-resolved-thread (comment-id)
  "Check if COMMENT-ID belongs to a resolved thread using optimized hash lookup."
  (unless shipit--resolved-comments-hash
    (shipit--build-resolved-comments-hash))
  (and shipit--resolved-comments-hash comment-id
       (gethash comment-id shipit--resolved-comments-hash)))

(defun shipit--clear-resolved-comments-hash ()
  "Clear the resolved comments hash table.
Call this when resolved threads cache is invalidated."
  (setq shipit--resolved-comments-hash nil))

;;; Thread Root Finding
;; Functions for walking up the in_reply_to_id chain to find thread roots.

(defun shipit--get-thread-root-id (comment comments)
  "Get the root comment ID for COMMENT by walking up the in_reply_to_id chain.
Returns the ID of the root comment (the one with no in_reply_to_id).
COMMENTS is used to look up parent comments."
  (unless comment
    (error "shipit--get-thread-root-id: comment is nil"))
  (unless comments
    (error "shipit--get-thread-root-id: comments list is nil"))

  (let ((current comment)
        (visited (make-hash-table :test 'equal))
        (max-depth 100)) ; Prevent infinite loops
    (catch 'root-found
      (dotimes (_ max-depth)
        (let ((comment-id (cdr (assq 'id current)))
              (in-reply-to (cdr (assq 'in_reply_to_id current))))
          ;; Detect cycles
          (when (gethash comment-id visited)
            (throw 'root-found comment-id))
          (puthash comment-id t visited)

          ;; If no parent, this is the root
          (unless in-reply-to
            (throw 'root-found comment-id))

          ;; Find parent comment
          (let ((parent (seq-find (lambda (c)
                                    (equal (cdr (assq 'id c)) in-reply-to))
                                  comments)))
            (if parent
                (setq current parent)
              ;; Parent not found, current must be root
              (throw 'root-found comment-id)))))
      ;; Fallback if max depth reached
      (cdr (assq 'id current)))))

(defun shipit--find-thread-root-id (comment-id in-reply-to comments)
  "Find the root comment ID by walking up in_reply_to chain.
Starts from COMMENT-ID and follows IN-REPLY-TO chain through COMMENTS list.
Returns the root comment ID (comment with no in_reply_to_id)."
  (if in-reply-to
      (catch 'root-found
        (let ((current in-reply-to)
              (depth 0))
          (while (and (< depth 100) current)
            (let ((parent-comment (seq-find (lambda (c)
                                              (equal (cdr (assq 'id c)) current))
                                            comments)))
              (if parent-comment
                  (let ((parent-reply-to (cdr (assq 'in_reply_to_id parent-comment))))
                    (if parent-reply-to
                        (setq current parent-reply-to
                              depth (1+ depth))
                      ;; Parent has no reply-to, so it's the root
                      (throw 'root-found (cdr (assq 'id parent-comment)))))
                ;; Parent not found, current is root
                (throw 'root-found current))))
          ;; Max depth reached, current is root
          current))
    comment-id))

(defun shipit--comment-or-parent-resolved-p (comment comments)
  "Check if COMMENT or any of its parents (by in_reply_to_id chain) are resolved.
COMMENTS is a list of all comment objects to search through for parent comments."
  (condition-case err
      (let* ((root-id (shipit--get-thread-root-id comment comments)))
        (shipit--is-comment-in-resolved-thread root-id))
    (error
     (when (fboundp 'shipit--debug-log)
       (shipit--debug-log "ERROR in shipit--comment-or-parent-resolved-p: %s" err))
     nil)))

;;; Comment Count Utilities

(defun shipit--count-file-comments (comments filename)
  "Count comments in COMMENTS list that belong to FILENAME."
  (let ((count 0))
    (dolist (comment comments)
      (when (equal (cdr (assq 'path comment)) filename)
        (setq count (1+ count))))
    count))

(defun shipit--count-outdated-comments (comments)
  "Count outdated comments in COMMENTS list."
  (let ((count 0))
    (dolist (comment comments)
      (when (or (cdr (assq 'outdated comment))
                (cdr (assq 'is_outdated comment)))
        (setq count (1+ count))))
    count))

(defun shipit--count-resolved-comments (comments)
  "Count resolved comments in COMMENTS list."
  (let ((count 0))
    (dolist (comment comments)
      (let ((comment-id (cdr (assq 'id comment))))
        (when (shipit--is-comment-in-resolved-thread comment-id)
          (setq count (1+ count)))))
    count))

;;; Reaction Fetching via Backend Dispatch

(defun shipit-comment--fetch-reactions-batch (comments repo is-inline &optional done-callback)
  "Fetch reactions for COMMENTS via backend dispatch and cache them.
REPO is the repository.  IS-INLINE selects the endpoint type.
Uses :fetch-reactions-batch if available, otherwise loops :fetch-reactions.
Skips review comments (they use GraphQL).

When DONE-CALLBACK is non-nil, the backend's batch implementation may
return immediately and invoke DONE-CALLBACK after all responses arrive
(non-blocking).  Backends that don't support async fall back to sync
and DONE-CALLBACK is invoked synchronously after the sync fetch."
  (when comments
    (let* ((resolved (shipit-comment--resolve-for-repo repo))
           (backend (car resolved))
           (config (cdr resolved))
           (batch-fn (plist-get backend :fetch-reactions-batch)))
      (if batch-fn
          (condition-case _err
              (funcall batch-fn config comments is-inline done-callback)
            (wrong-number-of-arguments
             ;; Backend doesn't accept the optional 4th arg yet.
             (funcall batch-fn config comments is-inline)
             (when done-callback (funcall done-callback))))
        ;; Fallback: loop :fetch-reactions for each comment
        (let ((fetch-fn (plist-get backend :fetch-reactions)))
          (dolist (comment comments)
            (let* ((comment-id (cdr (assq 'id comment)))
                   (is-review (equal (cdr (assq 'shipit-comment-type comment)) "review"))
                   (cache-key (shipit--reaction-cache-key repo comment-id is-inline)))
              (when (and comment-id (not is-review))
                (condition-case err
                    (let ((reactions (funcall fetch-fn config comment-id is-inline)))
                      (puthash cache-key (or reactions '()) shipit--reaction-cache))
                  (error
                   (shipit--debug-log "fetch-reactions fallback failed for %s: %s"
                                      comment-id (error-message-string err))
                   (puthash cache-key '() shipit--reaction-cache))))))
          (when done-callback (funcall done-callback)))))))

(defun shipit-comment--fetch-reactions (repo comment-id is-inline)
  "Fetch reactions for single COMMENT-ID via backend dispatch and cache.
REPO is the repository.  IS-INLINE selects the endpoint type."
  (let* ((resolved (shipit-comment--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (reactions (funcall (plist-get backend :fetch-reactions)
                             config comment-id is-inline))
         (cache-key (shipit--reaction-cache-key repo comment-id is-inline)))
    (puthash cache-key (or reactions '()) shipit--reaction-cache)
    reactions))

(provide 'shipit-comments)
;;; shipit-comments.el ends here
