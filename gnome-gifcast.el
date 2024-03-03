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

(defcustom gnome-gifcast-ffmpeg-args '("-vf"
                                       "split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse"
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
  (when-let ((label
              (pcase gnome-gifcast--duration-seconds
                ('nil)
                ((pred (not (integerp))))
                ;; ((pred (<= 0))
                ;;  (propertize " ● " 'face 'font-lock-builtin-face))
                ((pred (> 0))
                 (format "▶ in %s" (- gnome-gifcast--duration-seconds)))
                (_ (format "● %s" gnome-gifcast--duration-seconds)))))
    label))

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
    (when-let ((mline (buffer-local-value 'mode-line-format buff)))
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
    (when-let ((file (expand-file-name
                      gnome-gifcast-gnome-screencast-file
                      gnome-gifcast-screencast-directory)))
      (when (file-exists-p file)
        file))))

(defun gnome-gifcast-convert-to-gif (file)
  "Convert a video FILE to GIF using ffmpeg with custom arguments.

Argument FILE is the path to the video file to be converted to GIF."
  (interactive (list (read-file-name "Video to convert: ")))
  (let* ((outfile (concat
                   (file-name-sans-extension
                    file)
                   ".gif"))
         (command "ffmpeg")
         (args (delq nil
                     (append (list "-i" file)
                             gnome-gifcast-ffmpeg-args
                             (list "-c:v" "gif"
                                   "-f" "gif"
                                   (concat
                                    (file-name-sans-extension
                                     file)
                                    ".gif")))))
         (buffer (generate-new-buffer (format "*%s*" command)))
         (proc))
    (when (file-exists-p outfile)
      (delete-file outfile t))
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
              (unwind-protect
                  (let* ((buff (process-buffer process))
                         (output (if (process-buffer process)
                                     (with-current-buffer
                                         (process-buffer process)
                                       (buffer-string)))))
                    (if (zerop (process-exit-status process))
                        (progn
                          (run-hook-with-args
                           'gnome-gifcast-post-process-hook outfile)
                          (when outfile
                            (run-hook-with-args
                             'gnome-gifcast-browse-file-url
                             outfile))
                          (when (and buff (buffer-live-p buff))
                            (kill-buffer buff)))
                      (user-error (format "%s\n%s" command output))))
                (setq gnome-gifcast-gnome-running nil))))
           (require 'comint)
           (when (fboundp 'comint-output-filter)
             (set-process-filter proc #'comint-output-filter)))))

(defun gnome-gifcast-screencast-file-to-gif ()
  "Convert a GNOME screencast file to GIF format."
  (when-let ((file (gnome-gifcast-resolve-screencastfile)))
    (gnome-gifcast-convert-to-gif file)))


;;;###autoload
(defun gnome-gifcast-stop ()
  "Stop recording and convert to GIF."
  (interactive)
  (gnome-screencast-stop)
  (setq gnome-gifcast-gnome-running nil)
  (when-let ((file (gnome-gifcast-resolve-screencastfile)))
    (message "Recorded %s" (abbreviate-file-name file)))
  (run-hooks 'gnome-gifcast-post-record-hook))

(defvar gnome-gifcast-coords '()
  "Coordinates for GIF capture area in Gnome Gifcast.")

;;;###autoload
(defun gnome-gifcast-start ()
  "Start recording a screencast with GNOME."
  (interactive)
  (let ((project-name (file-name-base
                       (directory-file-name
                        (or
                         (when-let ((project (ignore-errors
                                               (project-current))))
                           (if (fboundp 'project-root)
                               (project-root project)
                             (with-no-warnings
                               (car (project-roots project)))))
                         buffer-file-name
                         (replace-regexp-in-string "\\*" "" (buffer-name)))))))
    (setq gnome-gifcast-gnome-screencast-file (concat project-name ".webm"))
    (setq gnome-gifcast-gnome-running t)
    (if gnome-gifcast-coords
        (apply #'gnome-screencast-area gnome-gifcast-gnome-screencast-file
               gnome-gifcast-coords)
      (gnome-screencast gnome-gifcast-gnome-screencast-file))))

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
     (gnome-gifcast-start)
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
    ((pred (not (integerp)))
     (setq gnome-gifcast--duration-seconds -3)
     (setq gnome-gifcast-coords
           (when (if arg
                     (not gnome-gifcast-record-current-window-only)
                   gnome-gifcast-record-current-window-only)
             (if-let ((wnd (minibuffer-selected-window)))
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

(provide 'gnome-gifcast)
;;; gnome-gifcast.el ends here