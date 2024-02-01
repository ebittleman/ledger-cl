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
   #'(lambda (item)
       (setf (gethash (funcall key-func item) tbl)
	     (apply map-func item)))
   items))

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

;; Product

(defun mk-product (sku name inventory-acct cogs-acct &optional (upc nil))
  (list :sku sku
	:name name
	:inventory-acct (getf inventory-acct :number)
	:cogs-acct (getf cogs-acct :number)
	:upc upc))

(defun build-product(sku name &optional (upc nil))
    #'(lambda (inventory-acct cogs-acct)
	(mk-product sku name inventory-acct cogs-acct upc)))

(defun construct-product (tables sku name inventory-acct cogs-acct &optional (upc nil))
  (apply (build-product sku name upc)
	 (lookup-accts tables inventory-acct cogs-acct)))

(defun build-products-table (tbl tables products)
  (build-table (curry #'construct-product tables) #'car tbl products))

(defun mk-inventory-batch (product inventory-acct qty cost)
  (list :sku (getf product :sku)
	:qty qty
	:cost cost
	:inventory-acct (getf inventory-acct :number)
	:cogs-acct (getf product :cogs-acct)))

(defun mk-inventory-allocation (qty batch cogs-acct)
  (list :sku (getf batch :sku)
	:qty qty
	:cost (getf batch :cost)
	:inventory-acct (getf batch :inventory-acct)
	:cogs-acct (getf cogs-acct :number)
	:batch-ref (getf batch :ref)))

(defun get-history (tbl has-sku)
  (let ((key (getf has-sku :sku)))
    (if key
	(let ((history (gethash key tbl)))
	  (if history history
	      (setf (gethash key tbl) (make-array 1 :fill-pointer 0 :adjustable t))))
	(error 'value-error :message "key cannot be nil"))))

(defun put-inventory-tx (tbl tx)
  (vector-push-extend tx (get-history tbl tx)))

(defun receive-product-tx (product qty cost inventory-acct payable-acct)
  (let ((amt (* qty cost)))
    (values (append-ref (mk-inventory-batch product inventory-acct qty cost))
	    (mk-tx inventory-acct amt 0.0)
	    (mk-tx payable-acct 0.0 amt))))

(defun mk-receiver-line (sku qty cost debit credit)
  (list :sku sku :qty qty :cost cost :debit debit :credit credit))

(defun receive-product (tables &key sku qty cost debit credit)
  (get-items (((product        sku    :products)
	       (inventory-acct debit  :accounts)
	       (payable-acct   credit :accounts))
	      tables)
    (all ("Invalid receive-product configuration" product inventory-acct payable-acct)
      (multiple-value-bind (batch debit-tx credit-tx)
	  (receive-product-tx product qty cost inventory-acct payable-acct)
	(list (put-inventory-tx (getf tables :inventory-batch) batch)
	      (append-tx (getf tables :gl-transactions) debit-tx credit-tx))))))


(defun select-vector (from where)
  (loop for x across from
	when (funcall where x)
	  collect x))

(defun batch-match (batch alloc)
  (equal (getf batch :ref) (getf alloc :batch-ref)))

(defun calculate-allocated (tables batch)
  (let* ((allocation-history (get-history (getf tables :inventory-allocations) batch))
	 (filtered (select-vector allocation-history (curry #'batch-match batch))))
    (reduce #'(lambda (val x) (+ val (getf x :qty)))
	    filtered
	    :initial-value 0.0)))

(defun find-batches (tables sku qty-wanted)
  (get-items (((product sku :products)) tables)
    (loop with qty-needed = qty-wanted
          for batch across (get-history (getf tables :inventory-batch) product)
	  for avail = (- (getf batch :qty) (calculate-allocated tables batch))
	  until (<= qty-needed 0)
	  when (> avail 0)
	    do (setf qty-needed (- qty-needed avail))
	  when (> avail 0)
	    collect (cons batch (+ avail (min 0 qty-needed))))))

(defun build-allocation(qty batch)
  #'(lambda (debit-acct credit-acct)
      (let ((amt (* qty (getf batch :cost))))
	(list (append-ref (mk-inventory-allocation qty batch debit-acct))
		(mk-tx credit-acct 0.0 amt)
		(mk-tx debit-acct amt 0.0)))))

(defun construct-allocation (tables debit-acct x)
  (apply (build-allocation (cdr x) (car x))
	 (lookup-accts tables debit-acct (getf (car x) :inventory-acct))))

(defun release-product-tx (tables sku qty-wanted debit-acct)
  (let* ((batches (find-batches tables sku qty-wanted))
         (qty-found (apply #'+ (mapcar #'(lambda (x) (cdr x)) batches))))
    (if (= qty-found qty-wanted)
	(mapcar (curry #'construct-allocation tables debit-acct) batches))))

(defun release-product (tables sku qty-wanted debit-acct)
  (let ((txs (release-product-tx tables sku qty-wanted debit-acct)))
    (loop for it in txs
	  collect
	  (destructuring-bind (inv-tx debit-tx credit-tx) it
	    (list (put-inventory-tx (getf tables :inventory-allocations) inv-tx)
		  (append-tx (getf tables :gl-transactions) debit-tx credit-tx))))))

;; State

(defparameter *account-table* (make-hash-table))
(defparameter *product-table* (make-hash-table :test #'EQUAL))
(defparameter *batch-history-table* (make-hash-table :test #'EQUAL))
(defparameter *allocations-history-table* (make-hash-table :test #'EQUAL))
(defparameter *transactions* (make-array 1 :fill-pointer 0 :adjustable t))

(defparameter *tables*
  (list
   :accounts *account-table*
   :products *product-table*
   :inventory-batch *batch-history-table*
   :inventory-allocations *allocations-history-table*
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

(build-accts-table *account-table* *accounts*)

(defparameter *products*
  (list
   '("PROD-001" "Example Product" 1401 5001)
   '("PROD-002" "Another Product" 1401 5001)
   '("PROD-003" "Mutated List" 1402 5001)
   '("PROD-004" "Last Thing We Work With" 1402 5001 123456)))

;; Interface

(build-products-table *product-table* *tables* *products*)

(using
 (mk-receiver-line "PROD-002" 5.0 36.78 1402 2001)
 (receive-product *tables*))

(release-product *tables* "PROD-002" 6 5001)
