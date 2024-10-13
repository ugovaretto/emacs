# LISP Cookbook

## Rename macro

```(setf (macro-function 'mvb) (macro-function 'multiple-value-bind))```

## Get current datetime

```lisp
(defun current-date-string ()
  "Returns current date as a string."
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
                       (get-decoded-time)
    (declare (ignore sec min hr dow dst-p tz))
    (format nil "~4,'0d-~2,'0d-~2,'0d" yr mon day) ))
```

## Lists to/from arrays

### Lists to 2D array

```lisp
(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

```

### 2D array to lists

```lisp
(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))

```

## Numbers from strings

```lisp
(defun parse-string-to-num (line)
  (with-input-from-string (s line)
    (loop
      for num := (read s nil nil)
      while num
      collect num)))
```

## Number string to digits

```lisp
(defun chars-to-nums (chars)
  (loop for i across chars for num := (digit-char-p i) collect num))
```

## Read multiple lines from stdin
```lisp
(defvar lines ())
(loop for line = (read-line *terminal-io* nil :eof)
      until (or (eq line :eof) (zerop (length line)))
      do (push line lines))
(format t "~&~S" lines)
```
