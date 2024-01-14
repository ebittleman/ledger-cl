;;;; general journal and example transactions

(in-package #:ledger-cl)

(defvar *general-journal* nil
  "general transactions")

(defun mk-transaction (
		       &key
		       account-code account trial-balance-debit trial-balance-credit
		       adjusting-entries-debit adjusting-entries-credit adjusted-trial-balance-debit
		       adjusted-trial-balance-credt income-statement-debit income-statement-credit
		       balance-sheet-debit balance-sheet-credit)

  (list :account-code account-code
	:account account
	:trial-balance-debit trial-balance-debit
	:trial-balance-credit trial-balance-credit
	:adjusting-entries-debit adjusting-entries-debit
	:adjusting-entries-credit adjusting-entries-credit
	:adjusted-trial-balance-debit adjusted-trial-balance-debit
	:adjusted-trial-balance-credt adjusted-trial-balance-credt
	:income-statement-debit income-statement-debit
	:income-statement-credit income-statement-credit
	:balance-sheet-debit balance-sheet-debit
	:balance-sheet-credit balance-sheet-credit)
  )

(defun add-transaction (record) (push record *general-journal*))

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
  `(let ((,var1 (car ,pair))
	 (,var2 (cdr ,pair)))
     ,@body))

(defun zip (fn list1 list2)
  (mapcar #'(lambda (pair)
	      (dcons (a b pair) (funcall fn a b)))
	  (pairlis list1 list2)))

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

(defun mk-partial-transaction (acct amount) (list :acct acct :amount (or amount 0)))

(defun get-acct (a) (getf a :acct))
(defun get-amount (a) (getf a :amount 0.0))


(defun inventory-purchase (&key
			     (raw-materials-inventory nil)
			     (merchandise-inventory nil)
			     (purchasing-account nil))
  (let (
	(debits (list (get-amount raw-materials-inventory) (get-amount merchandise-inventory)))
	(credits (list (get-amount purchasing-account)))
	(raw-acct (get-acct raw-materials-inventory))
	(merch-acct (get-acct merchandise-inventory))
	)
    (if (sumeq debits credits)
	(list
	 (if (> (first debits) 0) (mk-transaction :account-code raw-acct :balance-sheet-debit (first debits)))
	 (if (> (second debits) 0) (mk-transaction :account-code merch-acct :balance-sheet-debit (second debits))))
        (format t "mismatch ~a != ~a" debits credits)
	)
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *acct-types*
  (list
   :asset 1.0 :liability -1.0
   :equity -1.0
   :revenue -1.0 :expense 1.0))

(defun mk-acct (number kind &optional (name nil))
  (list :number number
	:kind kind
	:normal (getf *acct-types* kind)
	:name (if name name (format nil "~a-~a" number kind))))

(defun put-acct (tbl acct)
  (setf (gethash (car acct) tbl) (apply #'mk-acct acct)))

(defun build-accts-table (tbl accts)
    (mapcar
     #'(lambda (acct)(put-acct tbl acct))
     accts))

(defun mk-tx (acct debit-amt credit-amt)
  (list :acct-number (getf acct :number)
	:debit debit-amt
	:credit credit-amt))

(defparameter *accounts* (make-hash-table))

(defparameter *acct-list*
  (list
   '(1001 :asset "Checking Account")
   '(1002 :asset "Savings Account")
   '(1101 :asset "Accounts Receivable")
   '(1401 :asset "Inventory - Finished Goods")
   '(1402 :asset "Inventory - Raw Materials")
   '(1403 :asset "Inventory - Work In Progress")
   ))


