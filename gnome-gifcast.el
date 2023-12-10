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

(defcustom gnome-gifcast-ffmpeg-args '("-vf" "fps=10")
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


(defcustom gnome-gifcast-minimal-autoopen-duration 3
  "Minimal duration in seconds for auto opening recorded gifs."
  :group 'gnome-gifcast
  :type 'integer)

(defcustom gnome-gifcast-allow-mode-line-indicator t
  "Whether to show countdown and duration in mode-line."
  :group 'gnome-gifcast
  :type 'boolean)


(defvar gnome-gifcast--duration-timer nil)
(defvar gnome-gifcast--duration-seconds -3)

(defun gnome-gifcast--cancel-duration-timer ()
  "Cancel `gnome-gifcast--duration-timer'."
  (when (timerp gnome-gifcast--duration-timer)
    (cancel-timer gnome-gifcast--duration-timer))
  (setq gnome-gifcast--duration-timer nil))

(defconst gnome-gifcast-mode-line-format '(:eval
                                           (gnome-gifcast--mode-line-indicator)))

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
                ((pred (<= 0))
                 (propertize " ● " 'face 'font-lock-builtin-face))
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


(defvar gnome-gifcast-gnome-screencast-file nil)
(defvar gnome-gifcast-gnome-running nil)


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


;;;###autoload
(defun gnome-gifcast-stop ()
  "Stop screencast recording and convert to GIF."
  (interactive)
  (gnome-screencast-stop)
  (when-let* ((dir (xdg-user-dir "VIDEOS"))
              (file (and gnome-gifcast-gnome-screencast-file
                         (expand-file-name
                          gnome-gifcast-gnome-screencast-file
                          dir))))
    (when (file-exists-p file)
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
                  (let* ((buff (process-buffer process))
                         (output (if (process-buffer process)
                                     (with-current-buffer
                                         (process-buffer process)
                                       (buffer-string)))))
                    (if (zerop (process-exit-status process))
                        (progn (setq gnome-gifcast-gnome-running nil)
                               (run-hook-with-args
                                'gnome-gifcast-post-process-hook outfile)
                               (when outfile
                                 (run-hook-with-args
                                  'gnome-gifcast-browse-file-url
                                  outfile))
                               (when (and buff (buffer-live-p buff))
                                 (kill-buffer buff)))
                      (setq gnome-gifcast-gnome-running nil)
                      (user-error (format "%s\n%s" command output))))))
               (require 'comint)
               (when (fboundp 'comint-output-filter)
                 (set-process-filter proc #'comint-output-filter)))))))

;;;###autoload
(defun gnome-gifcast-start ()
  "Start GNOME screencast with project-based filename."
  (interactive)
  (when-let (project-name
             (file-name-base
              (directory-file-name
               (or
                (when-let ((project (ignore-errors
                                      (project-current))))
                  (if (fboundp 'project-root)
                      (project-root project)
                    (with-no-warnings
                      (car (project-roots project)))))
                buffer-file-name
                default-directory))))
    (setq gnome-gifcast-gnome-running t)
    (setq gnome-gifcast-gnome-screencast-file (concat project-name ".webm"))
    (gnome-screencast gnome-gifcast-gnome-screencast-file)))

;;;###autoload
(defun gnome-gifcast ()
  "Toggle GNOME screencast recording with project name."
  (interactive)
  (gnome-gifcast--cancel-duration-timer)
  (pcase gnome-gifcast--duration-seconds
    ((pred (not (integerp)))
     (setq gnome-gifcast--duration-seconds -3)
     (gnome-gifcast--mode-line-start)
     (setq gnome-gifcast--duration-timer
           (run-with-timer 1 nil #'gnome-gifcast)))
    ((pred (> 0))
     (setq gnome-gifcast--duration-seconds (1+ gnome-gifcast--duration-seconds))
     (force-mode-line-update)
     (setq gnome-gifcast--duration-timer
           (run-with-timer 1 nil #'gnome-gifcast)))
    ((pred (zerop))
     (setq gnome-gifcast--duration-seconds (1+ gnome-gifcast--duration-seconds))
     (force-mode-line-update)
     (gnome-gifcast-start))
    (_
     (gnome-gifcast--cancel-duration-timer)
     (gnome-gifcast--mode-line-stop)
     (setq gnome-gifcast--duration-seconds nil)
     (gnome-gifcast-stop))))


(provide 'gnome-gifcast)
;;; gnome-gifcast.el ends here