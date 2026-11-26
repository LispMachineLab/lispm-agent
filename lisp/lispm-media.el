;;; lispm-media.el --- Image and video media blocks and frame caching -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Image encoding, ffmpeg/ffprobe video frame extraction and caching, and
;; lazy media placeholder expansion for session transcripts.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-exec)
(require 'lispm-core)

(defcustom lispm-context-images 5
  "How many of a session's latest images and videos requests carry.
Older ones are dropped in batches of this many, so that the request
prefix, and with it the model's cache, changes only once a batch."
  :type 'integer
  :group 'lispm)

(defcustom lispm-video-frames 8
  "How many frames of a video the model sees.
They are taken evenly across its length, each at most
`lispm-video-frame-width' pixels wide, by ffmpeg (see `lispm-ffmpeg')."
  :type 'integer
  :group 'lispm)

(defcustom lispm-video-frame-width 768
  "How wide, in pixels at most, the frames of a video the model sees are."
  :type 'integer
  :group 'lispm)

(defcustom lispm-ffmpeg "ffmpeg"
  "The ffmpeg program, which takes the frames of videos.
Nil, or one not installed, and videos are named to the model instead."
  :type '(choice (string :tag "Program") (const :tag "None" nil))
  :group 'lispm)

(defcustom lispm-ffprobe "ffprobe"
  "The ffprobe program, which tells how long and how large a video is."
  :type '(choice (string :tag "Program") (const :tag "None" nil))
  :group 'lispm)

(defconst lispm-transcript-image-types
  '(("png" . "image/png") ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg")
    ("gif" . "image/gif") ("webp" . "image/webp"))
  "Image file extensions and their media types: those models take.")

(defconst lispm-media--video-types
  '(("mp4" . "video/mp4") ("m4v" . "video/mp4") ("mov" . "video/quicktime")
    ("webm" . "video/webm") ("mkv" . "video/x-matroska") ("avi" . "video/x-msvideo"))
  "Video file extensions and their media types.")

(defun lispm-media-kind (file)
  "What FILE is by its extension: `image', `video' or nil."
  (let ((extension (downcase (or (file-name-extension file) ""))))
    (cond ((assoc extension lispm-transcript-image-types) 'image)
          ((assoc extension lispm-media--video-types) 'video))))

(defconst lispm-media-link-re
  "^\\(?:\\[\\[file:\\([^]\n]+\\)\\]\\]\\|!\\[[^]\n]*\\](\\([^)\n]+\\))\\)[ \t]*$"
  "A line that is one link to a file, org's or Markdown's image.")

(defun lispm-transcript-media-link (line)
  "The image or video file LINE links, alone, or nil.
That is an org link, [[file:F]], or a Markdown image, ![alt](F), and
nothing else on the line; F is returned as written."
  (when (and (string-match lispm-media-link-re line)
             (lispm-media-kind (or (match-string 1 line) (match-string 2 line))))
    (or (match-string 1 line) (match-string 2 line))))

(defun lispm-media-image-block (file)
  "A placeholder block for the image or video FILE.
`lispm-media-load-images' reads it when the request is made."
  `((type . ,(if (eq (lispm-media-kind file) 'video) "video" "image")) (lispm-file . ,file)))

(defun lispm-media--text-block (text)
  "A model content block holding TEXT."
  `((type . "text") (text . ,text)))

(defun lispm-media--map-images (function messages)
  "MESSAGES with each image or video placeholder replaced by the blocks
FUNCTION returns for it, a list.  Placeholders are content blocks or
blocks inside tool results."
  (let ((map (lambda (blocks)
               (vconcat (mapcan (lambda (block)
                                  (cond ((alist-get 'lispm-file block) (funcall function block))
                                        ((and (equal (alist-get 'type block) "tool_result")
                                              (vectorp (alist-get 'content block)))
                                         (let ((copy (copy-alist block)))
                                           (setf (alist-get 'content copy)
                                                 (vconcat (mapcan (lambda (b)
                                                                    (if (alist-get 'lispm-file b)
                                                                        (funcall function b)
                                                                      (list b)))
                                                                  (append (alist-get 'content block) nil))))
                                           (list copy)))
                                        (t (list block))))
                                (append blocks nil))))))
    (mapcar (lambda (message)
              (let ((content (alist-get 'content message)))
                (if (not (vectorp content))
                    message
                  (let ((copy (copy-alist message)))
                    (setf (alist-get 'content copy) (funcall map content))
                    copy))))
            messages)))

(defun lispm-media--image-data (file)
  "An image block holding the image FILE, read and encoded."
  `((type . "image")
    (source (type . "base64")
            (media_type . ,(cdr (assoc (downcase (file-name-extension file))
                                       lispm-transcript-image-types)))
            (data . ,(with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally file)
                       (base64-encode-region (point-min) (point-max) t)
                       (buffer-string))))))

(defun lispm-media-load-images (messages)
  "MESSAGES with their latest image and video placeholders read in: an
image as an image block, a video as its frames (see `lispm-video-frames').
The others become a line of text; see `lispm-context-images'."
  (let ((total 0) (seen 0))
    (lispm-media--map-images (lambda (block) (cl-incf total) (list block)) messages)
    (if (zerop total)
        messages
      (let* ((keep (max 0 lispm-context-images))
             (drop (if (zerop keep)
                       total
                     (let ((d (- total keep)))
                       (if (> d 0) (- d (% d keep)) 0)))))
        (lispm-media--map-images
         (lambda (block)
           (let* ((file (alist-get 'lispm-file block))
                  (video (equal (alist-get 'type block) "video"))
                  (kind (if video "video" "image")))
             (cl-incf seen)
             (cond ((<= seen drop)
                    (list (lispm-media--text-block
                           (format "[%s %s, no longer shown]" kind (file-name-nondirectory file)))))
                   ((not (file-readable-p file))
                    (list (lispm-media--text-block (format "[%s %s is missing]" kind file))))
                   (video (lispm-media-video-blocks file))
                   (t (list (lispm-media--image-data file))))))
         messages)))))

;;;; Videos
;;
;; Models take images, not videos, so a video reaches one as frames
;; taken evenly across it.  ffmpeg takes them, once per video, into a
;; cache named after the file, its size and its time, so that a video
;; changed gets new frames and one unchanged keeps them.  The chat shows
;; a video by one such frame, its poster.

(defvar lispm-video--info-cache (make-hash-table :test #'equal)
  "Cached ffprobe metadata plists keyed by `(TRUENAME SIZE MTIME)'.")

(defvar lispm-video--pending (make-hash-table :test #'equal)
  "In-flight asynchronous video poster requests keyed by `(poster FILE WIDTH)'.")

(defun lispm-video--info-key (file)
  "Return cache key for FILE based on truename, size and modification time."
  (when-let* ((attrs (file-attributes file)))
    (list (file-truename file)
          (file-attribute-size attrs)
          (float-time (file-attribute-modification-time attrs)))))

(defun lispm-video--parse-info (output)
  "Parse ffprobe key-value OUTPUT into a metadata plist."
  (when (and (stringp output) (not (string-empty-p output)))
    (let (info)
      (with-temp-buffer
        (insert output)
        (goto-char (point-min))
        (while (re-search-forward "^\\(width\\|height\\|duration\\)=\\([0-9.]+\\)" nil t)
          (setq info (plist-put info (intern (concat ":" (match-string 1)))
                                (string-to-number (match-string 2))))))
      info)))

(defun lispm-video-info (file)
  "What ffprobe says of the video FILE: a plist of its `:duration' in
seconds, `:width' and `:height', those it tells; nil without ffprobe."
  (let ((key (lispm-video--info-key file)))
    (or (and key (gethash key lispm-video--info-cache))
        (when-let* ((ffprobe (and lispm-ffprobe (executable-find lispm-ffprobe))))
          (with-temp-buffer
            (when (zerop (call-process ffprobe nil t nil "-v" "error" "-select_streams" "v:0"
                                       "-show_entries" "stream=width,height:format=duration"
                                       "-of" "default=noprint_wrappers=1" (expand-file-name file)))
              (when-let* ((info (lispm-video--parse-info (buffer-string))))
                (when key (puthash key info lispm-video--info-cache))
                info)))))))

(defun lispm-video-info-async (file callback)
  "Probe video FILE metadata asynchronously and call CALLBACK with the plist."
  (let ((key (lispm-video--info-key file)))
    (if-let* ((cached (and key (gethash key lispm-video--info-cache))))
        (funcall callback cached)
      (if-let* ((ffprobe (and lispm-ffprobe (executable-find lispm-ffprobe))))
          (lispm-exec
           ffprobe
           (list "-v" "error" "-select_streams" "v:0"
                 "-show_entries" "stream=width,height:format=duration"
                 "-of" "default=noprint_wrappers=1" (expand-file-name file))
           (lambda (exit output _errors)
             (let ((info (and (eql exit 0) (lispm-video--parse-info output))))
               (when (and info key)
                 (puthash key info lispm-video--info-cache))
               (funcall callback info))))
        (funcall callback nil)))))

(defun lispm-video--length (seconds)
  "SECONDS as a length: 12.3 s, or 1:05 past a minute."
  (if (< seconds 60)
      (format "%.1f s" seconds)
    (format "%d:%02d" (/ (round seconds) 60) (% (round seconds) 60))))

(defun lispm-video-description (file)
  "The video FILE named, with its length and size when ffprobe tells them."
  (let ((info (lispm-video-info file)))
    (concat (file-name-nondirectory file)
            (if-let* ((duration (plist-get info :duration)))
                (format ", %s" (lispm-video--length duration))
              "")
            (if (and (plist-get info :width) (plist-get info :height))
                (format ", %d×%d" (plist-get info :width) (plist-get info :height))
              ""))))

(defun lispm-video--cache-dir (file)
  "Return the cache directory path for video FILE without creating it."
  (let ((attributes (file-attributes file)))
    (expand-file-name
     (format "cache/video/%s/"
             (substring (secure-hash 'sha1 (format "%s %s %s" (file-truename file)
                                                   (file-attribute-size attributes)
                                                   (float-time (file-attribute-modification-time
                                                                attributes))))
                        0 16))
     lispm-directory)))

(defun lispm-video--cache (file)
  "Directory of what is kept of the video FILE: its frames.
Named after the file, its size and its time, so that a video changed
gets a new one."
  (let ((dir (lispm-video--cache-dir file)))
    (make-directory dir t)
    dir))

(defun lispm-video-frames (file &optional n width)
  "N frames of the video FILE, taken evenly across it, as (SECONDS . JPEG).
N is `lispm-video-frames' by default; each frame is at most WIDTH pixels
wide, `lispm-video-frame-width' by default.  Nil without ffmpeg, or where
ffprobe cannot tell how long FILE is.  Frames once taken are kept (see
`lispm-video--cache'), so a video costs ffmpeg's time once."
  (when-let* ((ffmpeg (and lispm-ffmpeg (executable-find lispm-ffmpeg)))
              (duration (plist-get (lispm-video-info file) :duration))
              ((> duration 0)))
    (let* ((n (max 1 (or n lispm-video-frames)))
           (width (or width lispm-video-frame-width))
           (dir (lispm-video--cache file))
           (frames (cl-loop for i from 0 below n
                            collect (cons (/ (* (+ i 0.5) duration) n)
                                          (expand-file-name (format "%d-of-%d-%dw.jpg" i n width) dir)))))
      (dolist (frame frames)
        (unless (file-exists-p (cdr frame))
          (call-process ffmpeg nil nil nil "-v" "error" "-y"
                        "-ss" (format "%.3f" (car frame)) "-i" (expand-file-name file)
                        "-frames:v" "1" "-vf" (format "scale='min(%d,iw)':-2" width)
                        "-q:v" "4" (cdr frame))))
      (seq-filter (lambda (frame) (file-exists-p (cdr frame))) frames))))

(defun lispm-video--extract-frames-async (ffmpeg file width frames pending callback)
  "Extract PENDING frame pairs from video FILE using FFMPEG asynchronously.
FRAMES holds all target `(SECONDS . JPEG)' pairs; CALLBACK receives
those present when finished."
  (while (and pending (file-exists-p (cdar pending)))
    (setq pending (cdr pending)))
  (if (null pending)
      (funcall callback (seq-filter (lambda (frame) (file-exists-p (cdr frame))) frames))
    (let ((frame (car pending))
          (rest (cdr pending)))
      (lispm-exec
       ffmpeg
       (list "-v" "error" "-y"
             "-ss" (format "%.3f" (car frame)) "-i" (expand-file-name file)
             "-frames:v" "1" "-vf" (format "scale='min(%d,iw)':-2" width)
             "-q:v" "4" (cdr frame))
       (lambda (_exit _out _err)
         (lispm-video--extract-frames-async ffmpeg file width frames rest callback))))))

(defun lispm-video-frames-async (file callback &optional n width)
  "Extract N evenly spaced frames of video FILE at WIDTH without blocking.
Calls CALLBACK with `(SECONDS . JPEG)' pairs, or nil on failure."
  (let ((ffmpeg (and lispm-ffmpeg (executable-find lispm-ffmpeg))))
    (if (not ffmpeg)
        (funcall callback nil)
      (lispm-video-info-async
       file
       (lambda (info)
         (let ((duration (plist-get info :duration)))
           (if (not (and duration (> duration 0)))
               (funcall callback nil)
             (let* ((count (max 1 (or n lispm-video-frames)))
                    (w (or width lispm-video-frame-width))
                    (dir (lispm-video--cache file))
                    (frames (cl-loop for i from 0 below count
                                     collect (cons (/ (* (+ i 0.5) duration) count)
                                                   (expand-file-name
                                                    (format "%d-of-%d-%dw.jpg" i count w) dir)))))
               (lispm-video--extract-frames-async ffmpeg file w frames frames callback)))))))))

(defun lispm-video-poster (file &optional width)
  "A frame from the middle of the video FILE, a JPEG at most WIDTH pixels
wide (`lispm-video-frame-width' by default); nil without ffmpeg."
  (cdar (lispm-video-frames file 1 width)))

(defun lispm-video-poster-cached (file &optional width)
  "Return already cached poster JPEG for video FILE at WIDTH, or nil.
Performs only a filesystem check and never spawns subprocesses."
  (let* ((w (or width lispm-video-frame-width))
         (poster (expand-file-name (format "0-of-1-%dw.jpg" w)
                                   (lispm-video--cache-dir file))))
    (and (file-exists-p poster) poster)))

(defun lispm-video-poster-async (file callback &optional width)
  "Find or generate a poster frame for video FILE at WIDTH without blocking.
Calls CALLBACK with the JPEG file path or nil."
  (if-let* ((cached (lispm-video-poster-cached file width)))
      (funcall callback cached)
    (let* ((w (or width lispm-video-frame-width))
           (key (list 'poster (expand-file-name file) w)))
      (if-let* ((existing (gethash key lispm-video--pending)))
          (puthash key (cons callback existing) lispm-video--pending)
        (puthash key (list callback) lispm-video--pending)
        (lispm-video-frames-async
         file
         (lambda (frames)
           (let ((callbacks (nreverse (gethash key lispm-video--pending)))
                 (poster (cdar frames)))
             (remhash key lispm-video--pending)
             (dolist (cb callbacks)
               (funcall cb poster))))
         1 w)))))

(defun lispm-image-as-png (file)
  "FILE, an image of a kind models do not take, as a PNG: the file made,
next to it, or nil where nothing here converts images.  macOS's sips
does, else ffmpeg."
  (let ((png (concat (file-name-sans-extension (expand-file-name file)) ".png")))
    (and (cond ((and (eq system-type 'darwin) (executable-find "sips"))
                (zerop (call-process "sips" nil nil nil "-s" "format" "png"
                                     (expand-file-name file) "--out" png)))
               ((and lispm-ffmpeg (executable-find lispm-ffmpeg))
                (zerop (call-process lispm-ffmpeg nil nil nil "-v" "error" "-y"
                                     "-i" (expand-file-name file) png))))
         (file-exists-p png)
         png)))

(defun lispm-image-as-png-async (file callback)
  "Convert image FILE to PNG asynchronously and invoke CALLBACK with its path."
  (let* ((src (expand-file-name file))
         (png (concat (file-name-sans-extension src) ".png"))
         (cmd (cond ((and (eq system-type 'darwin) (executable-find "sips"))
                     (list "sips" "-s" "format" "png" src "--out" png))
                    ((and lispm-ffmpeg (executable-find lispm-ffmpeg))
                     (list lispm-ffmpeg "-v" "error" "-y" "-i" src png)))))
    (if (not cmd)
        (funcall callback nil)
      (lispm-exec (car cmd) (cdr cmd)
                  (lambda (exit _out _err)
                    (funcall callback (and (eql exit 0) (file-exists-p png) png)))))))

(defun lispm-media-video-blocks (file)
  "The blocks the model sees for the video FILE: a line about it, then
its frames; the line alone, saying so, where ffmpeg takes none."
  (let ((frames (lispm-video-frames file))
        (description (lispm-video-description file)))
    (cons (lispm-media--text-block
           (if frames
               (format "[video %s; %d frames follow, at %s]" description (length frames)
                       (mapconcat (lambda (frame) (lispm-video--length (car frame))) frames ", "))
             (format "[video %s; ffmpeg is needed to see it]" description)))
          (mapcar (lambda (frame) (lispm-media--image-data (cdr frame))) frames))))

(provide 'lispm-media)
;;; lispm-media.el ends here
