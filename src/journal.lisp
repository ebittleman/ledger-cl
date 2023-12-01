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
(export #:wrap)
(defun unwrap (a) (getf a :value))
(defun bind (a fn) (let ((val (unwrap a))) (if val (wrap (funcall fn val)) nil)))
(defun chain (initial &rest funcs)
  (reduce #'(lambda (a func) (bind a func)) funcs :initial-value (wrap initial)))

(defun curry (function &rest args)
  (lambda (&rest more-args)
    (apply function (append args more-args))))

(defun default-value (val) (curry #'(lambda (a i) (or i a)) val))

(defun clean-list (val a)
  (let ((dval (default-value val)))
    (mapcar dval a))
  )

(defun cleaner (val) (curry #'clean-list val))

(defun map-and-apply (mapper fn &rest a) (apply fn (funcall mapper a)))

(defun filter (pred items) (loop for x in items
				 for result = (funcall pred x) when result collect x))

;; (defun useless (fn &rest a) (eval `(funcall ,fn ,@a)))

(defun mk-partial-transaction (acct amount) (list :acct acct :amount amount))

(defun get-acct (a) (getf a :acct))
(defun get-amount (a) (getf a :amount 0))

(defun inventory-purchase (&key
			     (raw-materials-inventory nil)
			     (merchandise-inventory nil)
			     (purchasing-account nil))
  (let* ((raw-amount (get-amount raw-materials-inventory))
	 (merch-amount (get-amount merchandise-inventory))
	 (raw-acct (get-acct raw-materials-inventory))
	 (merch-acct (get-acct merchandise-inventory))
	 (debits (+ raw-amount merch-amount))
	 (credits (get-amount purchasing-account))
	 )
    (if (eq debits credits)
	(list
	 (if (> raw-amount 0) (mk-transaction :account-code raw-acct :balance-sheet-debit raw-amount))
	 (if (> merch-amount 0) (mk-transaction :account-code merch-acct :balance-sheet-debit merch-amount)))
        (format t "mismatch ~a != ~a" debits credits)
	)
    )
  )
;;
