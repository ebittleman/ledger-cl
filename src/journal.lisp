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

(defun print-map (map) (maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) map))

(defun default-value (val) (curry #'(lambda (a i) (or i a)) val))

(defun clean-list (val a)
  (let ((dval (default-value val)))
    (mapcar dval a))
  )

(defun cleaner (val) (curry #'clean-list val))

(defun map-and-apply (mapper fn &rest a) (apply fn (funcall mapper a)))

(defun filter (pred items) (loop for x in items
				 for result = (funcall pred x) when result collect x))
(defun sumlist (a) (apply #'+ a))
(defun sumeq (a b) (eql (sumlist a) (sumlist b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Common

(defun append-ref (plist &optional (ref nil ref-p))
  (setf (getf plist :ref) (if ref-p ref (uuid::make-v4-uuid)))
  plist)

(defun wrap-ref (&rest rest)
  (loop
    for i in rest collect (cons i nil)))

;; Ledger and Tx

(defparameter *acct-classifications*
  (list
   :asset 1.0 :liability -1.0
   :equity -1.0
   :revenue -1.0 :expense 1.0))

(defparameter *acct-kinds*
  (list
   :bank :asset
   :ar :asset
   :current-asset :asset
   :inventory :asset
   :fixed-asset :asset
   :ap :liability
   :current-liability :liability
   :unpaid-expense-claim :liability
   :wages-payable :liability
   :sales-tax :sales-tax
   :historical-adjustment :liability
   :rounding :liability
   :tracking :liability
   :non-current-liability :liability
   :equity :equity
   :retained-earnings :equity
   :revenue :revenue
   :direct-costs :expense
   :expense :expense))

(defun mk-acct (number kind &optional (name nil))
  (let ((class (getf *acct-kinds* kind)))
    (if class
	(list :number number
	      :kind kind
	      :class class
	      :normal (getf *acct-classifications* class)
	      :name (if name name (format nil "~a-~a" number kind)))
	(error 'value-error :message (format nil "unknown account kind: ~a" kind)))))

(defun build-table (map-func key-func tbl items)
  (mapcar
   #'(lambda (item) (setf (gethash (funcall key-func item) tbl) (apply map-func item)))
   items))

(defun build-accts-table (tbl accts)
    (build-table #'mk-acct #'car tbl accts))

(defun mk-tx (acct debit-amt credit-amt)
  (list :acct-number (getf acct :number)
	:debit debit-amt
	:credit credit-amt))

;; Product

(defun mk-product (sku name &optional (upc nil))
  (list :sku sku :name name :upc upc))

(defun build-products-table (tbl products)
  (build-table #'mk-product #'car tbl products))

(defun mk-inventory-batch (product qty cost)
  (list :sku (getf product :sku) :qty qty :cost cost :qty-allocated 0))

(defun get-history (tbl product)
  (let ((sku (getf product :sku)))
    (if (and product sku)
	(let ((history (gethash sku tbl)))
	  (if history history
	      (setf (gethash sku tbl) (make-array 1 :fill-pointer 0 :adjustable t))))
	(error 'value-error :message "sku cannot be nil"))))

(defun put-batch (tbl batch)
  (vector-push-extend batch (get-history tbl batch)))

(defun receive-product-tx (product qty cost inventory-acct payable-acct)
  (values (append-ref (mk-inventory-batch product qty cost))
	  (mk-tx inventory-acct (* qty cost) 0.0)
	  (mk-tx payable-acct 0.0 (* qty cost))))

(defmacro get-item ((var-name key table tables) &body body)
  `(let ((,var-name (gethash ,key (getf ,tables ,table))))
     ,@body)
  )

(defmacro get-items ((items tables) &body body)
  `(let ,(loop for i in items collect
	       `(,(car i) (gethash ,(second i) (getf ,tables ,(third i)))))
     ,@body))

(defun receive-product (sku qty cost accts tables)
  (get-items (((product sku :products)
	       (inventory-acct (getf accts :debit) :accounts)
	       (payable-acct   (getf accts :credit) :accounts))
	      tables)
    (all ("Invalid receive-product configuration" product inventory-acct payable-acct)
      (multiple-value-bind (a b c)
	  (receive-product-tx product qty cost inventory-acct payable-acct)
	(list (put-batch (getf tables :inventory-batch) a) b c)))))


(defparameter *account-table* (make-hash-table))
(defparameter *product-table* (make-hash-table :test #'EQUAL))
(defparameter *batch-history-table* (make-hash-table :test #'EQUAL))
(defparameter *transactions* (make-array 1 :fill-pointer 0 :adjustable t))

(defparameter *tables*
  (list
   :accounts *account-table*
   :products *product-table*
   :inventory-batch *batch-history-table*
   :gl-transactions *transactions*))

(defparameter *accounts*
  (list
   '(1001 :bank "Checking Account")
   '(1002 :bank "Savings Account")
   '(1101 :ar "Accounts Receivable")
   '(1401 :inventory "Inventory - Finished Goods")
   '(1402 :inventory "Inventory - Raw Materials")
   '(1403 :inventory "Inventory - Work In Progress")
   '(1404 :inventory "Inventory - Goods In Transit")
   '(2001 :ap "Accounts Payable")
   '(2050 :current-liability "Accruals")
   '(3001 :equity "Owners Contribution")
   '(3101 :equity "Owners Draw")
   '(3201 :retained-earnings "Retained Earnings")
   '(3501 :equity "Common Stock")
   '(4001 :revenue "Sales")
   '(4601 :revenue "Other Revenue")
   '(4701 :revenue "Interest Income")
   '(4801 :revenue "Refunds")
   '(5001 :direct-costs "Cost of Goods Sold")
   '(6001 :expense "General Expenses")
   ))

(defparameter *products*
  (list
   '("PROD-001" "Example Product")
   '("PROD-002" "Another Product")
   '("PROD-003" "Last Thing We Work With" 123456)))
