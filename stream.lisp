(in-package #:org.shirakumo.iclendar)

(defclass icalendar-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((backing-stream :initarg :backing-stream :accessor backing-stream)
   (column :initform 0 :accessor column))
  (:default-initargs
   :backing-stream (error "backing-stream required.")))

(defun make-icalendar-stream (output)
  (make-instance 'icalendar-stream :backing-stream output))

(defmethod trivial-gray-streams:stream-file-position ((stream icalendar-stream))
  (file-position (backing-stream stream)))

(defmethod trivial-gray-streams:stream-finish-output ((stream icalendar-stream))
  (finish-output (backing-stream stream)))

(defmethod trivial-gray-streams:stream-force-output ((stream icalendar-stream))
  (force-output (backing-stream stream)))

(defmethod trivial-gray-streams:stream-clear-output ((stream icalendar-stream))
  (setf (column stream) 0)
  (clear-output (backing-stream stream)))

(defmethod trivial-gray-streams:stream-line-column ((stream icalendar-stream))
  (column stream))

(defmethod trivial-gray-streams:stream-start-line-p ((stream icalendar-stream))
  (= 0 (column stream)))

(defmethod trivial-gray-streams:stream-advance-to-column ((stream icalendar-stream) column)
  (setf (column stream) column))

(defmethod trivial-gray-streams:stream-fresh-line ((stream icalendar-stream))
  (unless (= 0 (column stream))
    (trivial-gray-streams:stream-terpri stream)))

(defmethod trivial-gray-streams:stream-terpri ((stream icalendar-stream))
  (let ((backing (backing-stream stream)))
    (write-char #\Return backing)
    (write-char #\Linefeed backing)
    (setf (column stream) 0)))

(defun %write-char (char stream backing)
  (case char
    (#\Return
     #| Avoid CRLFs being output in content lines. |#)
    (#\Linefeed
     (%write-char #\\ stream backing)
     (%write-char #\n stream backing))
    (T
     (let ((octets (ceiling (log (char-code char) 2) 8)))
       (when (<= 75 (+ (column stream) octets))
         (write-char #\Return backing)
         (write-char #\Linefeed backing)
         (write-char #\Space backing)
         (setf (column stream) 1))
       (incf (column stream) octets)
       (write-char char backing)))))

(defmethod trivial-gray-streams:stream-write-char ((stream icalendar-stream) char)
  (%write-char char stream (backing-stream stream)))

(defmethod trivial-gray-streams:stream-write-sequence ((stream icalendar-stream) sequence start end &key)
  (loop with backing = (backing-stream stream)
        for i from start below end
        do (%write-char (aref sequence i) stream backing)))

(defmethod trivial-gray-streams:stream-write-string ((stream icalendar-stream) string &optional start end)
  (trivial-gray-streams:stream-write-sequence stream string (or start 0) (or end (length string))))
