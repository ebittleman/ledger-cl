;;;; Ledger and Tx

(in-package #:ledger-cl)

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

(defun build-accts-table (tbl accts)
  (build-table #'mk-acct #'car tbl accts))

(defun lookup-accts(tables debit credit)
  (get-items (((debit-acct debit :accounts)
	       (credit-acct credit  :accounts))
	      tables)
    (all ("Invalid Account Number" debit-acct credit-acct)
      (list debit-acct credit-acct))))

(defun mk-tx (acct debit-amt credit-amt)
  (list :acct-number (getf acct :number)
	:debit debit-amt
	:credit credit-amt))

(defun add-tx (acc tx)
  (dcons (debit credit acc)
    (cons (+ (getf tx :debit)  debit)
	  (+ (getf tx :credit) credit))))

(defun sum-tx (&rest rest)
  (reduce #'add-tx rest :initial-value (cons 0.0 0.0)))

(defun tx-balanced? (txs)
  (dcons (d1 c1 (apply #'sum-tx txs)) (eq d1 c1)))

(defun append-tx (tbl &rest rest)
  (if (tx-balanced? rest)
      (loop for tx in rest
	    do (vector-push-extend tx tbl))
      (error 'value-error :message "Imbalanced transactions")))
