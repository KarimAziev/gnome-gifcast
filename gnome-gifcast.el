;;; gnome-gifcast.el --- GIF recording using gnome-screencast and ffmpeg -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/gnome-gifcast
;; Version: 0.1.0
;; Keywords: tools, multimedia
;; Package-Requires: ((emacs "29.1") (gnome-screencast "1.3"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; GIF recording using gnome-screencast and ffmpeg.

;;; Code:


(require 'xdg)
(require 'gnome-screencast)

(defcustom gnome-gifcast-ffmpeg-args '("-y" "-vf"
                                       "setpts=0.5*PTS,fps=12,split[s0][s1];[s0]palettegen=max_colors=96[p];[s1][p]paletteuse=dither=bayer"
                                       "-loop" "0")
  "List of arguments for ffmpeg command.

Specifies the arguments passed to ffmpeg when converting a screencast to GIF
format. The default value is a list containing \"-vf\" and \"fps=10\", which
sets the frame rate of the output GIF to 10 frames per second.

Each element in the list is a string that represents a single command-line
argument to ffmpeg. These arguments are used to control the conversion process,
such as setting video filters, frame rates, or other encoding options.

To customize the conversion process, modify the list with appropriate ffmpeg
command-line arguments. Ensure that each argument is a separate string in the
list.

For example, to scale the width to 680 pixels, keep the aspect ratio for the
height, and apply the high-quality Lanczos resampling algorithm:

\\='(\"-vf\" \"fps=10,scale=680:-1:flags=lanczos\")."
  :type '(repeat string)
  :group 'gnome-gifcast)

(defcustom gnome-gifcast-gnome-screencast-arguments (list nil 30
                                                          "vp9enc ! mp4mux")
  "Customizable arguments for Gnome Screencast."
  :group 'gnome-gifcast
  :type '(list
          (boolean
           :tag "Draw cursor"
           :doc "Whether the cursor should be included")
          (natnum :tag "Framerate" 30)
          (radio :tag "Pipeline"
           (const
            :tag "vp9enc ! mp4mux"
            :format "%v    %h"
            :doc
            "High efficiency, especially in HD video.
                  Offers high efficiency, especially in HD video.
                  Uses VP9 codec for encoding and MP4 container.
                  Note: VP9 in MP4 may limit compatibility."
            "vp9enc ! mp4mux")
           (const
            :format "%v   %h"
            :tag "x264enc ! mp4mux"
            :doc
            "Balance in quality and efficiency, with broad compatibility.
                  Renowned for balance in quality and efficiency, with broad compatibility.
                  Utilizes H.264/AVC codec with MP4 container."
            "x264enc ! mp4mux")
           (string :tag "other"))))

(defcustom gnome-gifcast-out-filename #'gnome-gifcast--generate-filename-with-counter
  "Function or string defining output filename for GNOME screencasts.

Determines the naming strategy for output video files created by GNOME
Gifcast.

Note it is not the name of the GIF output file, it is the name of video outfile,
that will be created in `gnome-gifcast-screencast-directory'.

The default method generates a filename with an incrementing counter to ensure
uniqueness.

Options include:
- A static filename based on project or bufer,
which will be reused for every recording, potentially overwriting previous ones.
- A filename generated with a counter to avoid overwrites.
- A custom function which will be called without arguments and should return
filename base to put in `gnome-gifcast-screencast-directory'.
- A permanent name specified as a string, which will also risk overwriting files
with the same name.

To use a custom function, it must take no arguments and return a string"
  :group 'gnome-gifcast
  :type '(radio
          (function-item gnome-gifcast--generate-static-filename)
          (function-item gnome-gifcast--generate-filename-with-counter)
          (function :tag "Custom function")
          (string :tag "Permanent name")))

(defcustom gnome-gifcast-countdown-seconds 3
  "The number of seconds that will be counted down before recording."
  :group 'gnome-gifcast
  :type 'natnum)

(defcustom gnome-gifcast-post-process-hook '(gnome-gifcast-browse-file-url)
  "Hook to run after GIF creation from screencast.

A hook that is run after the GIF conversion process is complete.

Hooks are lists of functions to be called at specific times. When a screencast
recording is stopped and the resulting video file is converted to a GIF, the
functions in this hook are called with one argument: the path to the output GIF
file.

To add a function to this hook, use `add-hook'. For example, to add a function
named `my-post-process-function' that takes the output file path as an argument,
use:

\\=(add-hook \\='gnome-gifcast-post-process-hook \\='my-post-process-function)

To remove a function from the hook, use `remove-hook'.

Each function should accept a single string argument, which is the path to the
GIF file. Functions can perform any desired post-processing, such as moving the
file, uploading it, or opening it in an image viewer.

The default value is a list containing `gnome-gifcast-browse-file-url', which
opens the resulting GIF file in the default web browser.

Customize this hook to extend the functionality of the GIF conversion process
according to specific post-processing needs."
  :group 'gnome-gifcast
  :type 'hook)

(defcustom gnome-gifcast-pre-init-hook '()
  "Hook run before GNOME GIF screencast starts.

Hook to run before GNOME GIF screencast recording starts.

A hook that is run before the screencast recording process is initiated.

Hooks are lists of functions to be called at specific times. When a screencast
recording is about to start, the functions in this hook are called with no
arguments.

To add a function to this hook, use `add-hook'. For example, to add a function
named `my-pre-init-function', use:

\(add-hook \\='gnome-gifcast-pre-init-hook \\='my-pre-init-function)

To remove a function from the hook, use `remove-hook'.

Each function should perform any setup required before the recording starts.
This could include setting up window arrangements, starting applications, or
any other pre-recording tasks.

Customize this hook to prepare the environment for screencast recording
according to specific needs."
  :group 'gnome-gifcast
  :type 'hook)

(defcustom gnome-gifcast-post-record-hook '()
  "Hook run after recording a GIF screencast.

A hook that is run after the recording of a screencast is stopped.

Hooks are lists of functions to be called at specific times. When a screencast
recording is stopped, the functions in this hook are called with no arguments.

To add a function to this hook, use `add-hook'. For example, to add a function
named `my-post-record-function', use:

\(add-hook \\='gnome-gifcast-post-record-hook \\='my-post-record-function)

To remove a function from the hook, use `remove-hook'.

Each function added to this hook should take no arguments and can perform any
desired actions, such as cleaning up temporary files, displaying a notification,
or logging the completion of the recording.

The default value is nil, which means no functions are called after the
recording stops.

Customize this hook to extend the functionality of the screencast recording
process according to specific needs."
  :group 'gnome-gifcast
  :type 'hook)

(defcustom gnome-gifcast-record-current-window-only nil
  "Records only the current window for GIF screencasts.

Determines whether to record only the current window during a screencast.

When non-nil, the screencast will capture only the contents of the currently
focused window. When nil, the entire screen will be recorded.

To change the recording scope to the current window only, set this to t. To
record the entire screen, set this to nil.

This is a boolean option. Toggle or set the value to change the recording
behavior for future screencasts."
  :type 'boolean
  :group 'gnome-gifcast)

(defcustom gnome-gifcast-screencast-directory (xdg-user-dir "VIDEOS")
  "Directory for saving screencast videos before conversion.

Directory where GNOME GIF screencast files are saved.

Specifies the directory path where screencast recordings are stored before they
are converted to GIF format. The default directory is determined by the
`xdg-user-dir' command, which typically resolves to the user's \"VIDEOS\"
directory.

To change the directory where screencast files are saved, set this to a string
representing the desired directory path. Ensure that the directory exists and is
writable.

For example, to save screencast files to a directory named \"Screencasts\" in
the user's home directory:

\\=(setq gnome-gifcast-screencast-directory \"~/Screencasts\")

This will direct all new screencast recordings to the specified directory."
  :group 'gif-screencast
  :type 'directory)

(defvar gnome-gifcast--duration-timer nil
  "Timer controlling GIF frame duration.")

(defvar gnome-gifcast--duration-seconds nil
  "Duration in seconds for each frame in a GIF.")

(defun gnome-gifcast--cancel-duration-timer ()
  "Cancel `gnome-gifcast--duration-timer'."
  (when (timerp gnome-gifcast--duration-timer)
    (cancel-timer gnome-gifcast--duration-timer))
  (setq gnome-gifcast--duration-timer nil))

(defconst gnome-gifcast-mode-line-format '(:eval
                                           (gnome-gifcast--mode-line-indicator))
  "Format string or list for mode line indicator when recording GIFs.")

(defun gnome-gifcast--increment-duration ()
  "Increment `gnome-gifcast--duration-seconds' and update modeline."
  (setq gnome-gifcast--duration-seconds (1+ (or gnome-gifcast--duration-seconds 0)))
  (force-mode-line-update)
  (gnome-gifcast--cancel-duration-timer)
  (setq gnome-gifcast--duration-timer
        (run-with-timer 1 nil #'gnome-gifcast--increment-duration)))

(defun gnome-gifcast--mode-line-indicator ()
  "Return a string for the mode line with countdown or duration."
  (pcase gnome-gifcast--duration-seconds
    ('nil)
    ((pred (not (integerp))))
    ((pred (> 0))
     (format "▶ in %s" (- gnome-gifcast--duration-seconds)))
    (_ (format "● %s" gnome-gifcast--duration-seconds))))

(defun gnome-gifcast--mode-line-start ()
  "Add custom modeline format if not already present."
  (when (and (listp mode-line-format)
             (not (member gnome-gifcast-mode-line-format mode-line-format)))
    (setq mode-line-format
          (cons gnome-gifcast-mode-line-format
                mode-line-format))))


(defun gnome-gifcast--mode-line-stop ()
  "Remove gif mode line."
  (dolist (buff (buffer-list))
    (when-let* ((mline (buffer-local-value 'mode-line-format buff)))
      (cond ((and (listp mline)
                  (or
                   (eq gnome-gifcast-mode-line-format
                       (car-safe
                        gnome-gifcast-mode-line-format))
                   (memq gnome-gifcast-mode-line-format mline)))
             (with-current-buffer buff
               (setq mode-line-format
                     (remove gnome-gifcast-mode-line-format
                             mode-line-format)))))
      buff)))


(defvar gnome-gifcast-gnome-screencast-file nil
  "Path to the saved GNOME screencast GIF file.")

(defvar gnome-gifcast-gnome-running nil
  "Flag indicating if the Gnome GIFCast tool is active.")


(defvar browse-url-chrome-program)

(defun gnome-gifcast-browse-file-url (outfile)
  "Open OUTFILE in the default web browser.

Argument OUTFILE is the path to the file that will be opened in the browser."
  (require 'browse-url)
  (cond ((executable-find browse-url-chrome-program)
         (when (fboundp 'browse-url-chrome)
           (browse-url-chrome (concat "file:///"
                                      outfile))))
        (t (browse-url (concat "file:///" outfile)))))

(defun gnome-gifcast-resolve-screencastfile ()
  "Resolve full path for GNOME screencast file."
  (when gnome-gifcast-gnome-screencast-file
    (when-let* ((file (expand-file-name
                      gnome-gifcast-gnome-screencast-file
                      gnome-gifcast-screencast-directory)))
      (when (file-exists-p file)
        file))))

(defun gnome-gifcast--convert-to-gif (file &optional outfile on-success
                                           on-error)
  "Convert a video FILE to GIF format using ffmpeg.

Argument FILE is the path to the input video file.

Optional argument OUTFILE is the path where the output GIF should be saved. If
not provided, it defaults to the same name as FILE but with a `.gif' extension.

Optional argument ON-SUCCESS is a function to call upon successful conversion.

Optional argument ON-ERROR is a function to call if the conversion fails."
  (let* ((command "ffmpeg")
         (outfile (or outfile
                      (concat
                       (file-name-sans-extension
                        file)
                       ".gif")))
         (args (delq nil
                     (append (list "-i" file)
                             gnome-gifcast-ffmpeg-args
                             (list "-c:v" "gif"
                                   "-f" "gif"
                                   outfile))))
         (buffer (generate-new-buffer (format "*%s*" command)))
         (proc))
    (progn (with-current-buffer buffer
             (setq proc (apply #'start-process command buffer
                               command
                               args))
             (require 'shell)
             (when (fboundp 'shell-mode)
               (shell-mode))
             (view-mode +1))
           (pop-to-buffer buffer)
           (set-process-sentinel
            proc
            (lambda (process _state)
              (let ((buff (process-buffer process)))
                (if (not (zerop (process-exit-status process)))
                    (and on-error
                         (if buff
                             (with-current-buffer buff
                               (funcall on-error))
                           (funcall on-error)))
                  (when on-success
                    (funcall on-success outfile))
                  (when-let* ((buff (process-buffer process)))
                    (kill-buffer buff))))))
           (require 'comint)
           (when (fboundp 'comint-output-filter)
             (set-process-filter proc #'comint-output-filter))
           proc)))

;;;###autoload
(defun gnome-gifcast-convert-to-gif (file &optional outfile)
  "Convert a video FILE to GIF format and notify on completion.

Argument FILE is the path to the video file to be converted.

Optional argument OUTFILE is the path where the converted GIF should be saved."
  (interactive
   (let* ((input-file
           (read-file-name "Video to convert: "
                           nil
                           nil
                           t
                           (and (derived-mode-p 'dired-mode)
                                (car
                                 (when (fboundp 'dired-get-marked-files)
                                   (dired-get-marked-files))))))
          (out-file (read-file-name "Outfile: " (concat
                                                 (file-name-sans-extension
                                                  input-file)
                                                 ".gif"))))
     (list input-file
           out-file)))
  (gnome-gifcast--convert-to-gif file
                                 outfile
                                 (lambda (outfile)
                                   (gnome-gifcast-notify outfile))))


(defun gnome-gifcast-screencast-file-to-gif ()
  "Convert a GNOME screencast file to GIF format."
  (when-let* ((file (gnome-gifcast-resolve-screencastfile)))
    (gnome-gifcast--convert-to-gif file
                                   nil
                                   (lambda (outfile)
                                     (gnome-gifcast-notify outfile)))))


;;;###autoload
(defun gnome-gifcast-stop ()
  "Stop recording and convert to GIF."
  (interactive)
  (let ((running gnome-gifcast-gnome-running))
    (unwind-protect
        (gnome-screencast-stop)
      (setq gnome-gifcast-gnome-running nil)
      (progn
        (when-let* ((file (gnome-gifcast-resolve-screencastfile)))
          (when (and running
                     (not (memq 'gnome-gifcast-screencast-file-to-gif
                                gnome-gifcast-post-record-hook)))
            (gnome-gifcast-notify file)
            (run-hook-with-args
             'gnome-gifcast-post-process-hook file)))
        (run-hooks 'gnome-gifcast-post-record-hook)))))

(defvar gnome-gifcast-coords '()
  "Coordinates for GIF capture area in Gnome Gifcast.")


(defun gnome-gifcast--generate-static-filename ()
  "Generate a static filename based on project root, or buffer name.
Note, potentially it can overwrite previous ones."
  (file-name-base
   (directory-file-name
    (or
     (when-let* ((project (ignore-errors
                           (project-current))))
       (if (fboundp 'project-root)
           (project-root project)
         (with-no-warnings
           (car (project-roots project)))))
     buffer-file-name
     (let ((buff-name (buffer-name)))
       (if (string-match-p "[a-z0-9]" buff-name)
           (replace-regexp-in-string "[-][-]+" "-"
                                     (replace-regexp-in-string
                                      "[^a-z0-9_]" "-"
                                      buff-name))
         (replace-regexp-in-string "[/]" "" buff-name)))))))

(defun gnome-gifcast--generate-filename-with-counter ()
  "Generate a unique name by appending a counter from project root or buffer name."
  (gnome-gifcast--uniqify-filename-with-counter
   (gnome-gifcast--generate-static-filename)))

(defun gnome-gifcast--uniqify-filename-with-counter (file)
  "Generate a unique filename for FILE by appending a counter to avoid duplicates.

Optional argument FILE is the base name for the output file, possibly with
extension."
  (let* ((ext (file-name-extension file))
         (basename (file-name-base file))
         (file-regex (concat "\\`"
                             (regexp-quote basename)
                             (if ext
                                 (concat "\\(-[0-9]+\\)" "\\." ext "\\'")
                               "\\(-[0-9]+\\)\\'")))
         (max-count 0)
         (new-name))
    (dolist (filename (directory-files gnome-gifcast-screencast-directory
                                       nil
                                       file-regex))
      (let
          ((count (string-to-number (car (last (split-string filename "-" t))))))
        (when (> count max-count)
          (setq max-count count))))
    (setq new-name (string-join
                    (delq nil (list (format "%s-%d" basename max-count)
                                    (and ext (concat "." ext))))
                    ""))
    (while (file-exists-p (expand-file-name new-name
                                            gnome-gifcast-screencast-directory))
      (setq max-count (1+ max-count))
      (setq new-name (string-join
                      (delq nil (list (format "%s-%d" basename max-count)
                                      (and ext (concat "." ext))))
                      "")))
    new-name))

;;;###autoload
(defun gnome-gifcast-start (outfile-name-base)
  "Start recording a screencast with optional area coordinates.

Argument OUTFILE-NAME-BASE is the file name base of the output file."
  (interactive (list (read-string "Outfile name base")))
  (setq gnome-gifcast-gnome-screencast-file outfile-name-base)
  (setq gnome-gifcast-gnome-running t)
  (if gnome-gifcast-coords
      (apply #'gnome-screencast-area gnome-gifcast-gnome-screencast-file
             (append gnome-gifcast-coords
                     gnome-gifcast-gnome-screencast-arguments))
    (gnome-screencast gnome-gifcast-gnome-screencast-file
                      gnome-gifcast-gnome-screencast-arguments)))

(defun gnome-gifcast-get-window-coords (wind)
  "Retrieve pixel coordinates of a given window.

Argument WIND is the window for which to get the coordinates."
  (and wind
       (with-selected-window wind
         (let* ((top (window-pixel-top))
                (left (window-pixel-left))
                (width (window-pixel-width))
                (height (- (frame-pixel-height) top)))
           (list left top width height)))))


(defun gnome-gifcast--worker ()
  "Run GNOME screencast recording and conversion.

Optional argument ARG is an integer that modifies the behavior of the function
when provided."
  (gnome-gifcast--cancel-duration-timer)
  (pcase gnome-gifcast--duration-seconds
    ((pred (> 0))
     (setq gnome-gifcast--duration-seconds (1+ gnome-gifcast--duration-seconds))
     (force-mode-line-update)
     (setq gnome-gifcast--duration-timer
           (run-with-timer 1 nil #'gnome-gifcast--worker)))
    ((pred (zerop))
     (force-mode-line-update)
     (gnome-gifcast-start (if (functionp gnome-gifcast-out-filename)
                              (funcall gnome-gifcast-out-filename)
                            gnome-gifcast-out-filename))
     (setq gnome-gifcast--duration-timer
           (run-with-timer 1 nil #'gnome-gifcast--increment-duration)))))

;;;###autoload
(defun gnome-gifcast-toggle (&optional arg)
  "Toggle GNOME screencast recording.

Optional argument ARG is an integer that modifies the behavior of the function
when provided."
  (interactive "P")
  (gnome-gifcast--cancel-duration-timer)
  (pcase gnome-gifcast--duration-seconds
    ((pred (not (numberp)))
     (setq gnome-gifcast--duration-seconds (- gnome-gifcast-countdown-seconds))
     (setq gnome-gifcast-coords
           (when (if arg
                     (not gnome-gifcast-record-current-window-only)
                   gnome-gifcast-record-current-window-only)
             (if-let* ((wnd (minibuffer-selected-window)))
                 (gnome-gifcast-get-window-coords wnd)
               (gnome-gifcast-get-window-coords (selected-window)))))
     (run-hooks 'gnome-gifcast-pre-init-hook)
     (gnome-gifcast--mode-line-start)
     (setq gnome-gifcast--duration-timer
           (run-with-timer 1 nil #'gnome-gifcast--worker)))
    ((pred (< 0))
     (setq gnome-gifcast--duration-seconds nil)
     (gnome-gifcast-stop)
     (gnome-gifcast--mode-line-stop))
    (_
     (setq gnome-gifcast--duration-seconds nil)
     (gnome-gifcast--mode-line-stop))))

;;;###autoload
(defun gnome-gifcast (&optional arg)
  "Toggle GNOME screencast recording and conversion to gif.

Optional argument ARG is an integer that modifies the behavior of the function
when provided."
  (interactive "P")
  (let ((gnome-gifcast-post-record-hook
         (if (memq 'gnome-gifcast-screencast-file-to-gif
                   gnome-gifcast-post-record-hook)
             gnome-gifcast-post-record-hook
           (append
            gnome-gifcast-post-record-hook
            '(gnome-gifcast-screencast-file-to-gif)))))
    (gnome-gifcast-toggle arg)))


(defun gnome-gifcast--open-file-extern (file)
  "Open FILE with the default external application based on the system type.

Argument FILE is the path to the file to be opened."
  (if (and (eq system-type 'windows-nt)
           (fboundp 'w32-shell-execute))
      (w32-shell-execute "open" file)
    (call-process-shell-command (format "%s %s"
                                        (pcase system-type
                                          ('darwin "open")
                                          ('cygwin "cygstart")
                                          (_ "xdg-open"))
                                        (shell-quote-argument (expand-file-name
                                                               file)))
                                nil 0)))

(defun gnome-gifcast-notify (file &rest params)
  "Send a notification about a recorded screencast with an option to open it.

Argument FILE is the path to the screencast file.

Remaining arguments PARAMS are additional parameters passed to
`notifications-notify'."
  (require 'notifications)
  (gnome-gifcast-browse-file-url file)
  (when (fboundp 'notifications-notify)
    (apply #'notifications-notify
           :title "Screencast recordered"
           :body (format "Click here to open %s" file)
           :actions `("default" ,(format "Click here to open %s" file))
           :urgency 'critical
           :on-action (lambda (_id key)
                        (pcase key
                          ("default"
                           (message "openning")
                           (gnome-gifcast--open-file-extern file))))
           params)))

(provide 'gnome-gifcast)
;;; gnome-gifcast.el ends here