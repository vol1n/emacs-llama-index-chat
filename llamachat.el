;; chatgpt.el --- Use ChatGPT inside Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-openai/chatgpt
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (openai "0.1.0") (lv "0.0") (ht "2.0") (markdown-mode "2.1") (spinner "1.7.4"))
;; Keywords: comm openai

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Use ChatGPT inside Emacs
;;

;;; Code:

(setq debug-on-error t)

(require 'cl-lib)
(require 'let-alist)
(require 'subr-x)

(require 'lv)
(require 'ht)
(require 'markdown-mode)
(require 'spinner)

(load-file "~/Projects/emacs-llama-index-chat/llama-api-interface.el")
(require 'llama-api-interface)

(defgroup llamachat nil
  "Use Llamachat inside Emacs."
  :prefix "llamachat-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/vol1n/emacs-llama-index-chat"))

(defcustom llamachat-model "gpt-3.5-turbo"
  "Model to use for chat."
  :type 'string
  :group 'llamachat)

(defcustom llamachat-max-tokens 2000
  "The maximum number of tokens to generate in the completion."
  :type 'integer
  :group 'llamachat)

;; (defcustom llamachat-temperature 1.0
;;   "What sampling temperature to use."
;;   :type 'number
;;   :group 'llamachat)

;; (defcustom llamachat-top-p 1.0
;;   "Nucleus sampling parameter."
;;   :type 'number
;;   :group 'llamachat)

(defcustom llamachat-input-method 'window
  "The method receive input."
  :type '(choice (const :tag "Read from minibuffer" minibuffer)
                 (const :tag "Read inside new window" window))
  :group 'llamachat)

(defcustom llamachat-display-method nil
  "The method to display buffer."
  :type '(choice (const :tag "Target display function." function)
                 (const :tag "Use default function." nil))
  :group 'llamachat)

(defcustom llamachat-window-prompt "Type response here..."
  "Text inserted when window is created."
  :type 'string
  :group 'llamachat)

(defcustom llamachat-inhibit-input-afterward t
  "Stop input after sending one."
  :type 'boolean
  :group 'llamachat)

(defcustom llamachat-spinner-type 'moon
  "The type of the spinner."
  :type '(choice (const :tag "Key to variable `spinner-types'" symbol)
                 (const :tag "Vector of characters" vector))
  :group 'llamachat)

(defcustom llamachat-display-tokens-info t
  "Non-nil we display tokens information for each request."
  :type 'boolean
  :group 'llamachat)

(defcustom llamachat-priority 100
  "Overlays' priority."
  :type 'integer
  :group 'llamachat)

(defcustom llamachat-animate-text t
  "Display text gradually instead of output it all at once."
  :type 'boolean
  :group 'llamachat)

(defcustom llamachat-animate-fps 5
  "Frame per seconds to display text animation."
  :type 'integer
  :group 'llamachat)

(defconst llamachat-buffer-name-format "*Llamachat: <%s>*"
  "Name of the buffer to use for the `llamachat' instance.")

(defvar llamachat-instances (ht-create)
  "List of instances, each pair is consist of (index . buffer).")

(defvar-local llamachat-thread-id nil
  "Thread ID for each buffer")

(defvar-local llamachat-instance nil
  "Instance data for each buffer.")

(defvar-local llamachat-chat-history nil
  "The chat history use to send request.")

(defvar-local llamachat-chat-points nil
  "Buffer points every time we add a new message.")

(defvar-local llamachat-requesting-p nil
  "Non-nil when requesting; waiting for the response.")

(defvar-local llamachat-spinner nil
  "Spinner.")

(defvar-local llamachat-data (ht-create)
  "Store other information other than messages.")

(defvar-local llamachat--display-pointer 0
  "Display pointer for each message to display.")

(defvar-local llamachat--text-pointer 1
  "Text pointer for text animation.")

(defvar-local llamachat-text-timer nil
  "Text timer for text animation.")

(defvar-local llamachat-animating-p nil
  "Non-nil when animating text scroll.")

(defvar-local llamachat-user "user"
  "User name")

(defface llamachat-user
  '((t :inherit font-lock-builtin-face))
  "Face used for user."
  :group 'llamachat)

(defface llamachat-tip
  '((t :foreground "#848484"))
  "Face used for tip."
  :group 'llamachat)

(defface llamachat-info
  '((t :height 0.8 :foreground "#999999"))
  "Face added to codemetrics display."
  :group 'llamachat)

;;
;;; Externals

(declare-function string-pixel-width "ext:subr-x.el")
(declare-function shr-string-pixel-width "ext:shr.el")

;;
;;; Util

(defmacro llamachat--with-no-redisplay (&rest body)
  "Execute BODY without any redisplay execution."
  (declare (indent 0) (debug t))
  `(let ((inhibit-redisplay t)
         (inhibit-modification-hooks t)
         after-focus-change-function
         buffer-list-update-hook
         display-buffer-alist
         window-configuration-change-hook
         window-scroll-functions
         window-size-change-functions
         window-state-change-hook)
     ,@body))

(defmacro llamachat--with-buffer (buffer &rest body)
  "Execute BODY with in BUFFER."
  (declare (indent 1) (debug t))
  `(when (get-buffer ,buffer)
     (with-current-buffer ,buffer ,@body)))

;; TODO: Use function `string-pixel-width' after 29.1
(defun llamachat--string-pixel-width (str)
  "Return the width of STR in pixels."
  (if (fboundp #'string-pixel-width)
      (string-pixel-width str)
    (require 'shr)
    (shr-string-pixel-width str)))

(defun llamachat--str-len (str)
  "Calculate STR in pixel width."
  (let ((width (frame-char-width))
        (len (llamachat--string-pixel-width str)))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))  ; add one if exceeed

(defun llamachat--align (&rest lengths)
  "Align sideline string by LENGTHS from the right of the window."
  (list (* (window-font-width)
           (+ (apply #'+ lengths) (if (display-graphic-p) 1 2)))))

(defun llamachat--kill-buffer (buffer-or-name)
  "Like function `kill-buffer' (BUFFER-OR-NAME) but in the safe way."
  (when-let ((buffer (get-buffer buffer-or-name)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun llamachat--cancel-timer (timer)
  "Cancel TIMER."
  (when (timerp timer)
    (cancel-timer timer)))

(defun llamachat--display-buffer (buffer-or-name)
  "Wrapper to function `pop-to-buffer'.

Display buffer from BUFFER-OR-NAME."
  (if (functionp llamachat-display-method)
      (funcall llamachat-display-method buffer-or-name)
    (pop-to-buffer buffer-or-name `((display-buffer-in-direction)
                                    (dedicated . t)))))

(defun llamachat-busy-p ()
  "Return non-nil if session is busy."
  (or llamachat-requesting-p llamachat-animating-p))

(defun llamachat-user ()
  "Return the current user."
  (if (string-empty-p llamachat-user)
      "user"  ; this is free?
    llamachat-user))

;;
;;; Chat Point

(defun llamachat-chat-points ()
  "Return chat points in correct order."
  (reverse llamachat-chat-points))

(defun llamachat-current-chat-point ()
  "Return current chat point."
  (let ((chat-point))
    (cl-some (lambda (pt)
               (when (<= pt (point))
                 (setq chat-point pt)))
             llamachat-chat-points)
    chat-point))

(defun llamachat-chat-point-index (chat-point)
  "Return CHAT-POINT's index."
  (cl-position chat-point (llamachat-chat-points)))

(defun llamachat-current-chat-point-index ()
  "Return current chat point index."
  (llamachat-chat-point-index (llamachat-current-chat-point)))

(defun llamachat-current-message ()
  "Return current message."
  (elt llamachat-chat-history (llamachat-current-chat-point-index)))

(defun llamachat-current-content ()
  "Return current content."
  (alist-get 'content (llamachat-current-message)))

(defun llamachat-current-role ()
  "Return current role."
  (alist-get 'role (llamachat-current-message)))

(defun llamachat-shift-chat-points (start delta)
  "Shift all chat points after START by DELTA."
  (let ((index 0) (pt))
    (while (< index (length llamachat-chat-points))
      (setq pt (nth index llamachat-chat-points))
      (when (< start pt)
        (setf (nth index llamachat-chat-points) (+ pt delta)))
      (cl-incf index))))

;;
;;; Instances

(defmacro llamachat-with-instance (instance &rest body)
  "Execute BODY within INSTANCE."
  (declare (indent 1))
  `(when-let* ((buffer (and ,instance
                            (get-buffer (cdr ,instance))))
               ((buffer-live-p buffer)))
     (with-current-buffer buffer
       (let ((inhibit-read-only t))
         ,@body))))

(defun llamachat--live-instances ()
  "Return a list of live instances."
  (let ((live-instances))
    (ht-map (lambda (_ buffer)
              (when (and (get-buffer buffer)
                         (buffer-live-p buffer))
                (push buffer live-instances)))
            llamachat-instances)
    (reverse live-instances)))

(defun llamachat--shown-instances ()
  "Return a list of live instances that are displayed on the screen."
  (let ((live-instances (llamachat--live-instances))
        (shown-instances))
    (dolist (instance live-instances)
      (when (get-buffer-window instance)
        (push instance shown-instances)))
    (reverse shown-instances)))

(defun llamachat--new-index ()
  "Find killed instance before giving new index."
  (let ((target))
    (cl-some (lambda (index)
               (let ((buffer (ht-get llamachat-instances index)))
                 (when (or (not (get-buffer buffer))
                           (not (buffer-live-p buffer)))  ; if buffer is killed
                   (setq target index)
                   t)))
             (ht-keys llamachat-instances))
    (unless target                                ; No killed instance?
      (setq target (ht-size llamachat-instances)))  ; Create a new one!
    target))

(defun llamachat-restart-session ()
  "Restart session."
  (when (eq major-mode #'llamachat-mode)
    (let* ((instance llamachat-instance)
           (index    (car instance))
           (old-name))
      ;; If buffer is alive, kill it!
      (llamachat-with-instance instance
        (setq old-name (buffer-name))
        (kill-this-buffer))
      ;; `old-name' will remain `nil' if buffer is not killed or invalid!
      (when old-name
        (llamachat-register-instance index old-name)
        (switch-to-buffer old-name)))))

;;
;;; Core

(defun llamachat--get-face-height ()
  "Make sure we get the face height."
  (let ((height (face-attribute 'llamachat-info :height)))
    (if (numberp height) height
      1)))

(defun llamachat--create-tokens-overlay (prompt-tokens completion-tokens total-tokens)
  "Display tokens information.

Arguments PROMPT-TOKENS, COMPLETION-TOKENS, and TOTAL-TOKENS are the tokens
information we want to display."
  (when llamachat-display-tokens-info
    (let* ((ov (make-overlay (1- (point)) (1- (point)) nil t t))
           (content (format "prompt %s, completion %s, total: %s"
                            prompt-tokens completion-tokens total-tokens))
           (content-len (* (llamachat--str-len content)
                           (llamachat--get-face-height)))
           (str (concat
                 (propertize " " 'display
                             `((space :align-to (- right ,(llamachat--align (1- content-len))))
                               (space :width 0))
                             `cursor t)
                 (propertize content 'face 'llamachat-info))))
      (overlay-put ov 'llamachat t)
      (overlay-put ov 'priority llamachat-priority)
      (overlay-put ov 'after-string str))))

(defun llamachat--add-tokens (data)
  "Record all tokens information from DATA."
  (let-alist data
    (let-alist .usage
      ;; First we get the current tokens,
      (let ((prompt-tokens     (ht-get llamachat-data 'prompt_tokens 0))
            (completion-tokens (ht-get llamachat-data 'completion_tokens 0))
            (total-tokens      (ht-get llamachat-data 'total_tokens 0))
            (tokens-history    (ht-get llamachat-data 'tokens_history nil)))
        ;; Then we add it up!
        (ht-set llamachat-data 'prompt_tokens     (+ prompt-tokens     .prompt_tokens))
        (ht-set llamachat-data 'completion_tokens (+ completion-tokens .completion_tokens))
        (ht-set llamachat-data 'total_tokens      (+ total-tokens      .total_tokens))
        (ht-set llamachat-data 'tokens_history
                (append tokens-history
                        `((,.prompt_tokens ,.completion_tokens ,.total_tokens))))
        ;; Render it!
        (unless llamachat-animate-text
          (llamachat--create-tokens-overlay .prompt_tokens .completion_tokens .total_tokens))))))

(defun llamachat--add-chat-point ()
  "Add a chat point."
  (if llamachat-chat-points
      (push (point-max) llamachat-chat-points)
    (push (point-min) llamachat-chat-points)))

(defun llamachat--add-message (role content)
  "Add a message to history.

The data is consist of ROLE and CONTENT."
  (message "adding message")
  (message (concat "role: " role))
  (message (concat "content: " content))
  (llamachat--add-chat-point)
  (message "added chat point")
  (setq llamachat-chat-history
        (vconcat (or llamachat-chat-history '[])
                 `[((role    . ,role)
                    (content . ,(string-trim content)))])))

(defun llamachat--add-response-messages (data)
  "Add the message to history from DATA, and return the message itself."
  (let-alist data
    (mapc (lambda (choice)
            (let-alist choice
              (let-alist .message
                (llamachat--add-message .role .content))))
          .choices)))

;;
;;; Display

(defun llamachat--render-markdown (content)
  "Render CONTENT in markdown."
  (if (featurep 'markdown-mode)
      (with-temp-buffer
        (insert content)
        (delay-mode-hooks (markdown-mode))
        (ignore-errors (font-lock-ensure))
        (buffer-string))
    content))

(defun llamachat--fill-region (start end)
  "Like function `fill-region' (START to END), improve readability."
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (not (eobp))
      (end-of-line)
      (when (< fill-column (current-column))
        (fill-region (line-beginning-position) (line-end-position)))
      (forward-line 1))))

(defun llamachat--cancel-text-timer ()
  "Cancel text timer."
  (llamachat--cancel-timer llamachat-text-timer)
  (setq llamachat-text-timer nil
        llamachat-animating-p nil))

(defun llamachat--start-text-timer ()
  "Start text timer."
  (llamachat--cancel-text-timer)
  (setq llamachat-text-timer (run-with-timer (/ llamachat-animate-fps 60.0)
                                           nil
                                           #'llamachat--do-text-animation
                                           llamachat-instance)
        llamachat-animating-p t))


(defun llamachat--display-messages-at-once ()
  "If variable `llamachat-animate-text' is nil, we display messages all at once."
  (while (< llamachat--display-pointer (length llamachat-chat-history))
    (let ((message (elt llamachat-chat-history llamachat--display-pointer)))
      (let-alist message
        (goto-char (point-max))
        (let* ((start (point))
               (is-user (string= (llamachat-user) .role))
               (role (format "<%s>:" .role))
               ;; XXX: If messages from user, don't try to render to markdown!
               ;; else, messages from OpenAI will most likely going to be
               ;; markdown so we render it!
               (content (if is-user .content
                          (llamachat--render-markdown .content))))
          (add-face-text-property 0 (length role) 'llamachat-user nil role)
          (insert role " " content)
          (insert "\n\n")
          (llamachat--fill-region start (point)))))
    (cl-incf llamachat--display-pointer)))

(defun llamachat--display-messages ()
  "Display all messages to latest response."
  (when (zerop llamachat--display-pointer)  ; clear up the tip message
    (erase-buffer))
  (if llamachat-animate-text
      (unless (timerp llamachat-text-timer)  ; when is already running, don't interfere it
        (spinner-start llamachat-spinner)
        (llamachat--start-text-timer))
    (llamachat--display-messages-at-once)))

(defun llamachat--do-text-animation (instance chunk)
  "The main loop for text animation for the INSTANCE, processing CHUNK."
  (message chunk)
      (llamachat-with-instance instance
			       (let ((message (elt llamachat-chat-history llamachat--display-pointer)))
				 (let-alist message
				   (goto-char (point-max))
				   (let* ((is-user (string= (llamachat-user) .role))
					  (role (format "<%s>:" .role))
					  ;; XXX: If messages from user, don't try to render to markdown!
					  ;; else, messages from OpenAI will most likely going to be
					  ;; markdown so we render it!
					  (content (if is-user .content
						     (llamachat--render-markdown chunk))))
				     (add-face-text-property 0 (length role) 'llamachat-user nil role)
				     (insert role " " content)
				     (insert "

")
				     (llamachat--fill-region (point-max) (point-max))))
				 (setq llamachat--text-pointer (point-max)))))

(defun llamachat-display-streamed-content (instance content)
      "Display streamed CONTENT in the main llamachat INSTANCE buffer."
      (llamachat-with-instance instance
			       (goto-char (point-max))
			       (insert content)
			       ;; Optionally, process markdown or other formatting here
			       (when llamachat-animate-text
				 (llamachat-animate-display))
			       ;; Ensure the buffer scrolls to show the latest content
			       (set-window-point (get-buffer-window (current-buffer) t) (point-max))))

(defun llamachat-handle-plaintext-delta (instance delta)
  "Handle the incoming plaintext DELTA for a given INSTANCE."
  (llamachat-with-instance instance
			   (let ((content (concat (or (gethash 'partial-content llamachat-data) "") delta)))
			     ;; Update the partial content stored in the data hash table
			     (puthash 'partial-content content llamachat-data)
			     ;; Display the incoming chunk immediately
			     (llamachat-display-streamed-content instance delta)
			     ;; Check if the delta contains the end of a message marker or similar condition
			     (when (string-match-p "\n\n$" delta)
			       ;; If the end of a message is detected, process the complete message
			       (llamachat-process-complete-message instance content)
			       ;; Clear the stored partial content after processing
			       (remhash 'partial-content llamachat-data)))))

(defun llamachat-process-complete-message (instance message)
  "Process a complete MESSAGE that has been fully received in the given INSTANCE."
  ;; This function can handle additional parsing or actions based on the complete message
  (message "Complete message processed in instance."))

(defun llamachat-animate-display ()
  "Animate the display of text in the Llamachat buffer, if necessary."
  ;; Implementation of text animation goes here, if applicable
  )

;; Example usage of this setup might include setting up llamachat in the appropriate mode
;; and ensuring the instance is ready to display messages


;; (defun llamachat-handle-plaintext-delta (instance delta)
;;   "Handle the incoming plaintext DELTA for a given INSTANCE."
;;   (message "delta")
;;   (message delta)
;;       (llamachat-with-instance instance
;; 			       (let ((content (concat (or (gethash 'partial-content llamachat-data) "") delta)))
;; 				 ;; Update the partial content stored in the data hash table
;; 				 (puthash 'partial-content content llamachat-data)
;; 				 ;; Check if the delta contains the end of a message marker or similar condition
;; 				 (when (string-match-p "\n\n$" delta)
;; 				   ;; When a full message is detected, process and display it
;; 				   (llamachat-display-incoming-message content)
;; 				   ;; Clear the stored partial content after processing
;; 				   (remhash 'partial-content llamachat-data)))))

(defun llamachat-display-incoming-message (content)
  "Display the incoming message CONTENT in the chat buffer."
  (message "content")
  (message content)
  (with-current-buffer (cdr llamachat-instance)  ; Ensure correct buffer
    (let ((buffer-read-only nil)
	  (scroll-to-bottom (eq (point) (point-max))))  ; Auto-scroll if at bottom
      (goto-char (point-max))
      (insert (format "\n%s" content))  ; Add received content to buffer
      (when scroll-to-bottom
	(goto-char (point-max))))))  ; Maintain scroll position if needed

;; Add this line somewhere in your initialization or appropriate hook to clear partial content on session restart
(puthash 'partial-content "" llamachat-data)



(defun llamachat--handle-streaming-chunk (instance chunk)
  "Handle streaming CHUNK for INSTANCE."
  (llamachat-with-instance instance
			   (llamachat--do-text-animation instance chunk)))

(defun llamachat--start-streaming (instance)
  "Start handling streaming response for INSTANCE."
  
  (setq llamachat-text-timer (run-with-timer (/ llamachat-animate-fps 60.0)
					     nil
					     #'llamachat--do-text-animation
					     instance)
	llamachat-animating-p t))


(defun llamachat-send-response (response &optional callback)
      "Send RESPONSE to Llamachat and optionally call CALLBACK with the response."
      (let ((user (llamachat-user))
	    (instance llamachat-instance))
	(when (string-empty-p response)
	  (user-error "[INFO] Invalid response or instruction: %s" response))
	(llamachat--add-message user response)  ; add user's response
	(llamachat-with-instance instance
				 (let (llamachat-animate-text)
				   (llamachat--display-messages)))  ; display it
	(setq llamachat-requesting-p t)
	(spinner-start llamachat-spinner)
	;; Check if thread ID is available or not
	(if (or (null llamachat-thread-id) (string-empty-p llamachat-thread-id))
	    (progn
	      (llamachat--add-message user response)
	      ;; If no thread ID, create a new thread and use its ID
	      (message "added message, new thread now")
	      (llama-new-thread response
				(lambda (data)
				  (llamachat-with-instance instance
							   (setq llamachat-thread-id (alist-get 'thread_id data))
							   )

				  (message "llamachat thread id")
				  (message llamachat-thread-id)
				  (llamachat-handle-response instance data
							     (lambda (data)
							       (when callback
								 (funcall callback data)))))))

	  ;; If thread ID exists, send message to existing thread
	  (llama-send-message llamachat-thread-id response
			      (lambda (data)
				(llamachat-handle-response instance data callback))))))

(defun llamachat-handle-response (instance data callback)
  "Handle the response after sending a message to the thread."
  (message "llamachat thread id")
  (message llamachat-thread-id)
  (llamachat-with-instance instance
			   (message "llamachat thread id")
			   (message llamachat-thread-id)
			   (setq llamachat-requesting-p nil)
			   (spinner-stop llamachat-spinner)
			   (unless (alist-get 'error data)
			     ;; Start streaming the response after the message has been sent
			     (llamachat-stream-response llamachat-thread-id
							(lambda (chunk)
							  (llamachat-handle-plaintext-delta instance chunk)
							  (when callback
							    (funcall callback chunk)))))
			   (when callback
			           (funcall callback data))))

(defun llamachat-send-message (thread-id content callback)
  "Send a message to the specified THREAD-ID with CONTENT and call CALLBACK with the response."
  (llama-api-stream-request "/thread/message" "POST" `(("thread_id" . ,thread-id) ("content" . ,content))
			    (lambda (chunk)
			      (llamachat--handle-streaming-chunk llamachat-instance chunk)
			      (when callback
				                               (funcall callback chunk)))))
(defun llamachat-type-response ()
  "Type response to OpenAI."
  (interactive)
  (cond
   (llamachat-requesting-p
    (message "[BUSY] Waiting for OpanAI to response..."))
   (llamachat-animating-p
    (message "[BUSY] Waiting for animation to finish..."))
   (t
    (cl-case llamachat-input-method
      (`minibuffer
       (llamachat-send-response (read-string "Type response: ")))
      (`window
       (llamachat--start-input llamachat-instance))
      (t
       (user-error "Invalid input method: %s" llamachat-input-method))))))

;;
;;; Input

(defconst llamachat-input-buffer-name "*Llamachat-Input*"
  "Buffer name to receive input.")

(defvar llamachat-input-instance nil
  "The current instance; there is only one instance at a time.")

(defun llamachat-input-exit ()
  "Exit the input."
  (interactive)
  (llamachat--kill-buffer llamachat-input-buffer-name))

(defun llamachat--start-input (instance)
  "Start input from INSTANCE."
  (llamachat-input-exit)
  (let ((dir (if (window-parameter nil 'window-side)
                 'bottom 'down))
        (buffer (get-buffer-create llamachat-input-buffer-name)))
    ;; XXX: Without this, the highlighting at the end wouldn't work!?
    (llamachat--with-no-redisplay
      (with-current-buffer buffer
        (llamachat-input-mode)
        (setq llamachat-input-instance instance)
        (erase-buffer)
        (insert llamachat-window-prompt)
        (call-interactively #'set-mark-command)
        (goto-char (point-min))))  ; waiting for deletion
    (pop-to-buffer buffer `((display-buffer-in-direction)
                            (direction . ,dir)
                            (dedicated . t)
                            (window-height . fit-window-to-buffer)))))

(defun llamachat-input-send ()
  "Send the input."
  (interactive)
  (cond
   ((not (eq major-mode #'llamachat-input-mode)) )  ; does nothing
   (llamachat-requesting-p
    (message "[BUSY] Waiting for OpanAI to response..."))
   ((region-active-p)
    (delete-region (region-beginning) (region-end)))
   (t
    (let ((response (buffer-string)))
      (llamachat-with-instance llamachat-input-instance
        (llamachat-send-response response))
      (erase-buffer))
    (when llamachat-inhibit-input-afterward
      (llamachat-input-exit)))))

(defun llamachat-input--post-command ()
  "Execution after input."
  (let ((max-lines (line-number-at-pos (point-max))))
    (fit-window-to-buffer)
    (enlarge-window (- max-lines (window-text-height)))))

(defun llamachat-input-header-line ()
  "The display for input header line."
  (format " [Session] %s" (cdr llamachat-input-instance)))

(defvar llamachat-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "S-<return>") #'newline)
    (define-key map (kbd "RET") #'llamachat-input-send)
    map)
  "Keymap for `llamachat-input-mode'.")

(define-derived-mode llamachat-input-mode fundamental-mode "Llamachat Input"
  "Major mode for `llamachat-input-mode'.

\\<llamachat-input-mode-map>"
  (setq-local header-line-format `((:eval (llamachat-input-header-line))))
  (add-hook 'post-command-hook #'llamachat-input--post-command nil t))

;;
;;; Edit

(defconst llamachat-edit-buffer-name "*Llamachat-Edit*"
  "Buffer name to receive edit.")

(defvar llamachat-edit-instance nil
  "The current instance; there is only one instance at a time.")

(defvar llamachat-edit-chat-point-index nil
  "Store the chat point index.")

(defun llamachat-edit-exit ()
  "Exit the edit."
  (interactive)
  (llamachat--kill-buffer llamachat-edit-buffer-name))

(defun llamachat-edit-start (instance)
  "Start edit from INSTANCE."
  (llamachat-edit-exit)
  (let ((dir (if (window-parameter nil 'window-side)
                 'bottom 'down))
        (buffer (get-buffer-create llamachat-edit-buffer-name))
        (content (llamachat-current-content))
        (index (llamachat-current-chat-point-index)))
    ;; XXX: Without this, the highlighting at the end wouldn't work!?
    (llamachat--with-no-redisplay
      (with-current-buffer buffer
        (llamachat-edit-mode)
        (setq llamachat-edit-instance instance
              llamachat-edit-chat-point-index index)
        (erase-buffer)
        (insert content)))
    (pop-to-buffer buffer `((display-buffer-in-direction)
                            (direction . ,dir)
                            (dedicated . t)
                            (window-height . fit-window-to-buffer)))))

(defun llamachat-edit-send ()
  "Send the edit."
  (interactive)
  (cond
   ((not (eq major-mode #'llamachat-edit-mode)) )  ; do nothing
   (llamachat-requesting-p
    (message "[BUSY] Waiting for OpanAI to response..."))
   ((region-active-p)
    (delete-region (region-beginning) (region-end)))
   (t
    (let ((response (buffer-string)))
      (llamachat-with-instance llamachat-edit-instance
        ;; Update display!
        (let* ((start (llamachat-current-chat-point))
               (old-len (length response))
               (new-len (length (llamachat-current-content)))
               (diff (- old-len new-len)))
          (goto-char start)
          (search-forward ">: " (line-end-position) t)  ; XXX: Improve this!
          (delete-region (point) (+ (point) new-len))
          (insert response)
          (llamachat--fill-region start (point))
          (llamachat-shift-chat-points start diff))

        ;; Update history!
        (setf (elt llamachat-chat-history llamachat-edit-chat-point-index)
              `((role    . ,(llamachat-user))
                (content . ,(string-trim response)))))
      (erase-buffer))
    (when llamachat-inhibit-input-afterward
      (llamachat-edit-exit)))))

(defvar llamachat-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "S-<return>") #'newline)
    (define-key map (kbd "RET") #'llamachat-edit-send)
    map)
  "Keymap for `llamachat-edit-mode'.")

(define-derived-mode llamachat-edit-mode fundamental-mode "Llamachat Edit"
  "Major mode for `llamachat-edit-mode'.

\\<llamachat-edit-mode-map>"
  (setq-local header-line-format `((:eval (llamachat-input-header-line))))
  (add-hook 'post-command-hook #'llamachat-input--post-command nil t))

;;
;;; Info

(defun llamachat--pre-command-once (&rest _)
  "One time pre-command after Easky command."
  ;; XXX: We pass on to next post-command!
  (remove-hook 'pre-command-hook #'llamachat--pre-command-once)
  (add-hook 'post-command-hook #'llamachat--post-command-once))

(defun llamachat--post-command-once ()
  "One time post-command after info command."
  ;; XXX: This will allow us to scroll in the lv's window!
  (unless (equal lv-wnd (selected-window))
    ;; Once we select window other than lv's window, then we kill it!
    (remove-hook 'post-command-hook #'llamachat--post-command-once)
    (lv-delete-window)))

(defun llamachat-info ()
  "Show session information."
  (interactive)
  (when (eq major-mode #'llamachat-mode)
    (lv-message
     (concat
      (format "session: %s" (cdr llamachat-instance)) "\n"
      (format "history size: %s" (length llamachat-chat-history))
      "\n\n"
      (format "prompt_tokens: %s | completion_tokens: %s | total_tokens: %s"
              (ht-get llamachat-data 'prompt_tokens 0)
              (ht-get llamachat-data 'completion_tokens 0)
              (ht-get llamachat-data 'total_tokens 0))
      "\n\n"
      (format "model: %s" llamachat-model) "\n"
      (format "max_tokens: %s" llamachat-max-tokens) "\n"
      (format "temperature: %s" llamachat-temperature) "\n"
      (format "top-p: %s" llamachat-top-p) "\n"
      (format "user: %s" (llamachat-user))))
    ;; Register event to cancel lv window!
    (add-hook 'pre-command-hook #'llamachat--pre-command-once)))

;;
;;; Entry

(defun llamachat--clear-side-fields ()
  "Kill side fileds."
  (let ((instances llamachat-instances))
    (llamachat--with-buffer llamachat-input-buffer-name
      (when (equal instances llamachat-instances)
        (kill-this-buffer)))
    (llamachat--with-buffer llamachat-edit-buffer-name
      (when (equal instances llamachat-instances)
        (kill-this-buffer)))))

(defun llamachat-mode--kill-buffer-hook (&rest _)
  "Kill buffer hook."
  (ht-clear llamachat-data)
  (spinner-stop llamachat-spinner)
  (llamachat--cancel-text-timer)
  (llamachat--clear-side-fields))

(defun llamachat-header-line ()
  "The display for header line."
  (format " %s[Session] %s  [History] %s  [User] %s"
          (if-let ((frame (spinner-print llamachat-spinner)))
              (concat frame " ")
            "")
          (cdr llamachat-instance)
          (length llamachat-chat-history)
          (llamachat-user)))

(defvar llamachat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'llamachat-type-response)
    map)
  "Keymap for `llamachat-mode'.")

(defun llamachat-mode-insert-tip ()
  "Insert tip to output buffer."
  (when (string-empty-p (buffer-string))
    (let ((inhibit-read-only t)
          (tip "Press <return> to start asking questions

`M-x llamachat-info` will print out more information about the current session!
"))
      (add-face-text-property 0 (length tip) 'llamachat-tip nil tip)
      (insert tip))))

(define-derived-mode llamachat-mode fundamental-mode "Llamachat"
  "Major mode for `llamachat-mode'.

\\<llamachat-mode-map>"
  (buffer-disable-undo)
  (setq-local buffer-read-only t)
  (font-lock-mode -1)
  (add-hook 'kill-buffer-hook #'llamachat-mode--kill-buffer-hook nil t)
  (setq-local header-line-format `((:eval (llamachat-header-line))))
  (setq llamachat-spinner (spinner-create llamachat-spinner-type t))
  (setq-local llamachat-data (ht-create))
  (llamachat-mode-insert-tip))

(defun llamachat-register-instance (index buffer-or-name)
  "Register BUFFER-OR-NAME with INDEX as an instance.

Caution, this will overwrite the existing instance!"
  (ht-set llamachat-instances index (get-buffer-create buffer-or-name))
  (with-current-buffer buffer-or-name
    (llamachat-mode)
    (setq llamachat-instance (cons index (current-buffer)))))

;;;###autoload
(defun llamachat-new ()
  "Run a new instance of Llamachat."
  (interactive)
  (let* ((new-index       (llamachat--new-index))
         (new-buffer-name (format llamachat-buffer-name-format new-index)))
    (when (get-buffer new-buffer-name)
      (user-error "Internal Error: creating instance that already exists"))
    (llamachat-register-instance new-index new-buffer-name)
    (llamachat--display-buffer new-buffer-name)))

;;;###autoload
(defun llamachat ()
  "Start Llamachat with existing instance, else create a new instance."
  (interactive)
  (let ((live-instances  (llamachat--live-instances))
        (shown-instances (llamachat--shown-instances)))
    (cond (shown-instances
           (llamachat--display-buffer (nth 0 shown-instances)))
          (live-instances
           (llamachat--display-buffer (nth 0 live-instances)))
          (t
           (llamachat-new)))))

;;;###autoload
(defun llamachat-restart ()
  "Restart the current Llamachat instance."
  (interactive)
  (save-window-excursion
    (let ((instance
           (cl-case major-mode
             (`llamachat-mode       (current-buffer))
             (`llamachat-input-mode (with-current-buffer llamachat-input-buffer-name
                                    (cdr llamachat-input-instance)))
             (`llamachat-edit-mode  (with-current-buffer llamachat-edit-buffer-name
                                    (cdr llamachat-edit-instance))))))
      (when instance
        (with-current-buffer instance
          (llamachat--clear-side-fields)
          (llamachat-restart-session))))))

(provide 'llamachat)
;;; llamachat.el ends here
