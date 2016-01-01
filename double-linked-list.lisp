;;; Advise from shka on lisp (chat.freenode.net):
;;; "Store both ends of the list, for faster reverse and dll-push-end"
;;; Advise from mood:
;;; "More descriptive names, use descriptive functions instead of cddadar"
;;; Advise from blub:
;;; "instead of '(when dll ; dll not empty?' you could just write '(when (not (null dll))', just as descriptive but self documenting"



(defpackage :double-linked-list
  (:use :common-lisp))

(in-package :double-linked-list)

;;; the double linked list:

;; lla:
(l1 l2 l3 l4 l5 l6 . nil)
;; lx:
(pos-in-lla pos-in-lld tuple) 
;; lld:
(l6 l5 l4 l3 l2 l1 . nil)

(defun dll-push (item dll)
  "create a new record and add it to the start of lla, and to the end of lld"
  (let ((r (list nil nil item))) ; positions will be new conses, so nil for now
    (setf (cadr r)  (cons r nil)) ; cons for lld
    (setf (car r) (cons r dll)) ; cons for lla
    (when dll ; dll not empty?
      ;; then add it to the end of lld. This end is pointed to by
      ;; the cadr of the still current first record of lla
      (setf (cdr (cadr (car dll))) 
	    (cadr r))) ; set the end of lld (is nil) to the new cons
    (car r)))



(defun dll-pop (dll)
  "Remove and return the first tuple from dll, returns a list with the tuple and the new dll "
  (let ((item (caddr (car dll)))
	(newdll (cdr dll)))
    (when newdll
      (setf (cdr (cadr (car newdll))) nil))
    (list item newdll)))


;; length is usable for ddls
(defun dll-length (dll)
  (length dll))

(defun dll-elt (dll n)
  (caddr (cl:elt dll n)))

(defun dll-subseq (dll start &optional (end nil))
  "build a new dll, taken from :start to :end from dll. "
  (cond ((not end) (dll-subseq dll start  (dll-length dll)))
	((< 0 start) (dll-subseq (cdr dll) (1- start) (1- end)))
	((> 0 start) (error "start should be > 0"))
	((> 0 end) (error "end should be >= start"))
	((= start end) nil)
	((null dll) nil)
	(t (dll-push (caddar dll) (dll-subseq (cdr dll) start (1- end))))))

(defun dll-nsubseq (dll start &optional (end nil))
  "takes part of a dll, might destroy the old one to save time or conses, or just for fun. "
  ;; This function still takes stupid boring start and end. It should be something more awesome. 
  (let ((p 0)
	(r nil))
    (do ()
	((or (= p start)
	     (null dll)))
      (progn (incf p)
	     (setf dll (cdr dll))))
    (setf r dll)
    (incf p)
    (when dll (progn
		(setf (cdadar dll) nil)
		(do ()
		    ((or (null (cdr dll))
			 (and end (>= p end))))
		  (format t "~a ~&" p)
		  (progn (incf p)
			 (setf dll (cdr dll))))
		(setf (cdr dll) nil))
	  r)))
  
(defun dll-reverse (dll)
  "Builds a new dll, with the records of dll in reverse order"
  (let ((r nil))
    (do ()
	((null dll))
      (let ((p (dll-pop dll)))
	(setf dll (cadr p))
	(setf r (dll-push (car p) r))))
    r))
	   
	

(defun dll-nreverse (dll)
  "Reverses the order of the records in dll"
  (let ((r nil))
    (do ()
	((null dll))
      (progn
	(rotatef (caar dll) (cadar dll))
	(setf r (caar dll))
	(setf dll (cdr dll))))
    f))

(defun dll-copy (dll)
  "Creates a fresh copy of dll, with fresh cons cells."
  (dll-nreverse (dll-reverse dll)))

(defun dll-count (item dll &key (key #'identity) (test #'eql) start end)
  (cond ((or start end) (dll-count item (dll-subseq dll start end)
				   :key key :test test))
	((null dll) 0)
	((funcall test item (funcall key (caddar dll)))
	 (1+ (dll-count item (cdr dll) :key key :test test)))
	(t (dll-count item (cdr dll) :key key :test test))))



(defun dll-find-record (item dll &key (key #'identity) (test #'eql) start end)
  (cond ((or start end) (dll-count item (dll-subseq dll start end)
				   :key key :test test))
	((null dll) nil)
	((funcall test item (funcall key (caddar dll)))
	 (car dll))
	(t (dll-find-record item (cdr dll) :key key :test test))))
  
(defun dll-find (item dll &key (key #'identity) (test #'eql) start end)
  (caddr (dll-find-record item dll :key key :test test :start start :end end)))

(defun dll-position (item dll &key (key #'identity) (test #'eql) start end)

  ;; This function is not tail-recursive!! Maybe some cool helper param?
  (cond ((or start end) (+ (or start 0)
			   (dll-position item (dll-subseq dll start end)
					 :key key :test test)))
	((null dll) nil)
	((funcall test item (funcall key (caddar dll)))
	 0)
	(t (let ((r (dll-position item (cdr dll) :key key :test test)))
	     (when r (1+ r))))))

(defun dll-delete-record (record)
  (setf (cdaar (cdadr record)) (cdar record))
  (setf (cdadr (cadar record)) (cdadr record)))

(defun dll-delete (item dll &key (key #'identity) (test #'eql) (start 0) (end -1) (count -1))
  "Remove all records that contain item as their contents. "
  ;; I could only make the caddaddadr's in the core by drawing out the structure.
  (let ((r dll))
    (do () ((or (zerop start) (null dll)))
      (progn (decf start)
	     (setf dll (cdr dll))
	   (when end (decf end))))
    (do () ((or (zerop end) (null dll) (zerop count)))
      (progn (decf end)
	     (when (funcall test item (funcall key (caddar dll)))
	       (progn (decf count)
		      (setf (cdaar (cdadar dll)) (cdr dll))
		      (setf (cdr (cadadr dll)) (cdr (cadar dll)))
		      ))
	     (setf dll (cdr dll))))
    r))

(defun dll-remove (item dll &key (key #'identity) (test #'eql) (start 0) (end -1) (count -1))
  (dll-delete item (dll-copy dll) :key key :test test :start start :end end :count count))

(defun dll-nsubstitute (nitem item dll &key (key #'identity) (test #'eql) (start 0) (end -1) (count -1))
  ;; This is so much like dll-delete, i should use a macro
  (let ((r dll))
    (do () ((or (zerop start) (null dll)))
      (progn (decf start)
	     (setf dll (cdr dll))
	   (when end (decf end))))
    (do () ((or (zerop end) (null dll) (zerop count)))
      (progn (decf end)
	     (when (funcall test item (funcall key (caddar dll)))
	       (progn (decf count)
		      (setf (caddar dll) nitem)
		      ))
	     (setf dll (cdr dll))))
    r))

(defun dll-substitute  (nitem item dll &key (key #'identity) (test #'eql) (start 0) (end -1) (count -1))
  (dll-nsubstitute nitem item (dll-copy dll) :key key :test test :start start :end end :count count))

  
(defun dll-to-list (dll)
  (if dll
      (cons (caddar dll) (dll-to-list (cdr dll)))
      nil))

(defun dll-push-list (list dll)
  (if list
      (dll-push-list (cdr list) (dll-push (car list) dll))
      dll))

