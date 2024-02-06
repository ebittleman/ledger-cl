;;;; general journal and example transactions

(in-package #:ledger-cl)

(define-condition value-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)(format stream "~a~&" (message condition))))
  )

(defun wrap (val) (if val (list :value val) nil))
(defun unwrap (a) (getf a :value))
(defun bind (a fn) (let ((val (unwrap a))) (if val (wrap (funcall fn val)) nil)))
(defun chain (initial &rest funcs)
  (reduce #'(lambda (a func) (bind a func)) funcs :initial-value (wrap initial)))

(defun safe_divide (a b) (if (eq b 0) nil (/ a b)))
;; (chain 4
;;        #'(lambda (x) (* x 2))
;;        #'(lambda (x) (- 6 x))
;;        #'(lambda (x) (safe_divide 2 x))
;;        #'(lambda (x) (* 2 x))
;;        )

(defun curry (function &rest args)
  (lambda (&rest more-args)
    (apply function (append args more-args))))

(defmacro dcons((var1 var2 pair) &body body)
  (let ((tmp-eval (gensym)))
    `(let* ((,tmp-eval ,pair)
	    (,var1 (car ,tmp-eval))
	    (,var2 (cdr ,tmp-eval)))
       ,@body)))

(defun zip (fn list1 list2)
  (mapcar #'(lambda (pair)
	      (dcons (a b pair) (funcall fn a b)))
	  (pairlis list1 list2)))

(defmacro all ((msg &rest rest) &body body)
  `(if (and ,@rest)
       ,@body
       (error 'value-error :message ,msg)))

(defmacro using (plist using-form)
  (let((tmp-var (gensym))
       (fn (car using-form))
       (args (cdr using-form)))
    `(let ((,tmp-var ,plist))(apply #',fn (append (list ,@args) ,tmp-var)))))

;; example with "using"
;;
;; (using
;;  (mk-receiver-line "PROD-002" 6.0 32.78 1401 2001)
;;  (receive-product *tables*))

(defun ident (x) x)

(defun k (x) (lambda (y) (declare (ignore y)) x))

(defun s (f)
  (lambda (g)
    (lambda (x)
      (funcall (funcall f x) (funcall g x)))))

(defun sk () (s #'k))

(defun pass-along (side-effect) (funcall (sk) side-effect))

(defmacro b (f g) `(lambda (x) (,f (,g x))))

(defun printarg (x) (format t "'~a'~%" x) nil)

(defun print-return (x) (funcall (pass-along #'printarg) x))

(defun print-map (map) (maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) map))

(defun default-value (val) (curry #'(lambda (a i) (or i a)) val))

(defun map-and-apply (mapper fn &rest a) (apply fn (funcall mapper a)))

(defun filter (pred items)
  (loop for x in items
	for result = (funcall pred x)
	when result collect x))

(defmacro applyf (getter fn &rest rest)
  `(setf ,getter (funcall ,fn ,@rest ,getter)))

(defun map-reduce (items mapper reducer &key (initial-value 0.0))
  (reduce
   #'(lambda (val x)
       (let ((mapped (funcall mapper x)))
	 (if mapped
	     (funcall reducer val mapped)
	     val)))
   items
   :initial-value initial-value))

;; (map-reduce (gethash "PROD-002" *batch-history-table*)
;; 	    #'(lambda (x) (if (< 33.0 (getf x :cost)) x nil))
;; 	    #'(lambda (val x) (+ (getf x :qty) val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Common

(defun append-ref (plist &optional (ref nil ref-p))
  (setf (getf plist :ref) (if ref-p ref (uuid::make-v4-uuid)))
  plist)

(defun wrap-ref (&rest rest)
  (loop
    for i in rest collect (cons i nil)))

(defmacro get-items ((items tables) &body body)
  `(let ,(loop for i in items collect
	       `(,(car i) (gethash ,(second i) (getf ,tables ,(third i)))))
     ,@body))

(defun build-table (map-func key-func tbl items)
  (mapcar
   #'(lambda (item)
       (setf (gethash (funcall key-func item) tbl)
	     (apply map-func item)))
   items))

(defmacro getorset (key ctor)
  (let ((var (gensym)))
    `(let ((,var ,key))
       (if ,var ,var
	   (setf ,key ,ctor)))))

(defun select-vector (from where)
  (loop for x across from
	when (funcall where x)
	  collect x))
