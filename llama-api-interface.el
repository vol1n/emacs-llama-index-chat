;; -*- lexical-binding: t -*-


(require 'request)

(defun llama-api-request (endpoint method &optional data callback)
  "Make an API request to the Llama API at ENDPOINT using METHOD and optionally send DATA.
CALLBACK is called with the response data."
  (let ((url (concat "http://localhost:8000" endpoint))
        (headers '(("Content-Type" . "application/json"))))
    (request
      url
      :type method
      :data (and data (json-encode data))
      :headers headers
      :parser 'json-read
      :sync nil
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (message "Request succeeded: %S" data)
                  (and callback
                    (funcall callback data))))
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (message "Request failed: %S" error-thrown))))))

(require 'json)

(defun llama-api-stream-request (endpoint method &optional data callback)
  "Make a streaming API request to the Llama API at ENDPOINT using METHOD and send DATA.
Pass each response chunk to CALLBACK."
  (let ((url (concat "http://localhost:8000" endpoint))
	(url-request-method method)
	(url-request-extra-headers '(("Content-Type" . "application/json")))
	(url-request-data (json-encode data)))

    (url-retrieve url 'llama-handle-stream (list
					    (lambda (data)
					      (message "data")
					      (message data)
					      (when callback
						(funcall callback data)))))))

(defun llama-handle-stream (status callback)
  "Handle the streaming response. STATUS is the status info, CALLBACK is the function to process each chunk."
  (when (plist-get status :error)
    (message "Request failed: %s" (plist-get status :error)))
  (goto-char (point-min))
  (re-search-forward "\n\n")
  (let ((response (buffer-substring-no-properties (point) (point-max))))
    (dolist (chunk (split-string response "\n" t))
      (funcall callback chunk)))
  (kill-buffer (current-buffer)))

(defun process-chunk (chunk)
  "Process each CHUNK of data."
    (message "Received chunk: %s" chunk))

(defun llama-send-message (thread-id content callback)
  "Send a message to the specified THREAD-ID with CONTENT and call CALLBACK with the response."
  (llama-api-request "/thread/message" "POST" `(("thread_id" . ,thread-id) ("content" . ,content))
		     (lambda (data)
		       (when callback
			 (funcall callback data)))))

(defun llama-edit-message (thread-id message-id new-content callback)
  "Edit a message in THREAD-ID with MESSAGE-ID to NEW-CONTENT and call CALLBACK with the response."
  (llama-api-request "/thread/message" "PUT" `(("thread_id" . ,thread-id) ("message_id" . ,message-id) ("new_content" . ,new-content))
		     (lambda (data)
		       (when callback
			 (funcall callback data)))))

(defun llama-delete-message (thread-id &optional message-id callback)
  "Delete a message with MESSAGE-ID in THREAD-ID and call CALLBACK with the response. If MESSAGE-ID is nil, delete the thread."
  (if message-id
      (llama-api-request "/thread/message" "DELETE" `(("thread_id" . ,thread-id) ("message_id" . ,message-id)) callback)
    (llama-api-request "/thread/message" "DELETE" `(("thread_id" . ,thread-id))
		       (lambda (data)
		       (when callback
			 (funcall callback data))))))

(defun llama-get-thread (thread-id callback)
  "Get the messages in THREAD-ID and call CALLBACK with the response."
  (llama-api-request (concat "/thread/" thread-id) "GET" nil
		     (lambda (data)
		       (when callback
			 (funcall callback data)))))

(defun llama-new-thread (content callback)
  "Create a new thread and call CALLBACK with the response"
  (llama-api-request "/thread" "POST" `(("content" . ,content))
		     (lambda (data)
		       (when callback
			 (funcall callback data)))))

(defun llama-generate-response (thread-id callback)
  "Generate a response for THREAD-ID and call CALLBACK with the response."
  (llama-api-request (concat "/thread/" thread-id "/generate") "POST" nil
		     (lambda (data)
		       (when callback
			 (funcall callback data)))))

(defun llamachat-stream-response (thread-id callback)
      "Stream the response for THREAD-ID and process each chunk with CHUNK-HANDLER."
      (llama-api-stream-request (concat "/thread/" thread-id "/stream") "POST" nil
				(lambda (data)
				  (when callback
				    (funcall callback data)))))

(provide 'llama-api-interface)
