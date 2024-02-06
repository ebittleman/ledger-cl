;;;; Product

(in-package #:ledger-cl)

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

(defun build-locations-table (tbl locations)
  (build-table #'ident #'car tbl locations))

(defun mk-inventory-batch
    (product inventory-acct qty cost location)
  (list :sku (getf product :sku)
	:qty qty
	:cost cost
	:inventory-acct (getf inventory-acct :number)
	:cogs-acct (getf product :cogs-acct)
	:location location))

(defun mk-inventory-allocation (qty batch cogs-acct location)
  (list :sku (getf batch :sku)
	:qty qty
	:cost (getf batch :cost)
	:inventory-abocation
	:batch-ref (getf batch :ref)))

(defun get-history (tbl has-sku)
  (let ((key (getf has-sku :sku)))
    (if key
	(getorset (gethash key tbl)
		  (make-array 1 :fill-pointer 0 :adjustable t))
	(error 'value-error :message "key cannot be nil"))))

(defun put-inventory-tx (tbl tx)
  (vector-push-extend tx (get-history tbl tx)))

(defun receive-product-tx
    (product qty cost
     inventory-acct payable-acct
     location)
  (let ((amt (* qty cost)))
    (values
     (append-ref (mk-inventory-batch product inventory-acct
				     qty cost location))
     (mk-tx inventory-acct amt 0.0)
     (mk-tx payable-acct 0.0 amt))))

(defun mk-receiver-line (sku qty cost debit credit &optional (location :default))
  (list :sku sku
	:qty qty
	:cost cost
	:debit debit
	:credit credit
	:location location))

(defun receive-product (tables &key sku qty cost debit credit (location :default))
  (get-items (((product        sku    :products)
	       (inventory-acct debit  :accounts)
	       (payable-acct   credit :accounts)
	       (loc       location    :locations))
	      tables)
    (all ("Invalid receive-product configuration"
	  product inventory-acct payable-acct loc)
      (multiple-value-bind (batch debit-tx credit-tx)
	  (receive-product-tx product qty cost
			      inventory-acct payable-acct
			      loc)
	(list (put-inventory-tx (getf tables :inventory-batch) batch)
	      (append-tx (getf tables :gl-transactions)
			 debit-tx credit-tx))))))

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
  #'(lambda (debit-acct credit-acct location)
      (let ((amt (* qty (getf batch :cost))))
	(list (append-ref (mk-inventory-allocation qty batch debit-acct location))
		(mk-tx credit-acct 0.0 amt)
		(mk-tx debit-acct amt 0.0)))))

(defun construct-allocation (tables debit-acct loc x)
  (apply (build-allocation (cdr x) (car x))
	 (get-items (((debit   debit-acct                    :accounts)
		      (credit (getf (car x) :inventory-acct) :accounts)
		      (location loc                          :locations))
		     tables)
	   (list debit credit location))))

(defun release-product-tx (tables sku qty-wanted debit-acct location)
  (let* ((batches (find-batches tables sku qty-wanted))
         (qty-found (apply #'+ (mapcar #'(lambda (x) (cdr x)) batches))))
    (if (= qty-found qty-wanted)
	(mapcar (curry #'construct-allocation tables debit-acct location) batches))))

(defun release-product (tables sku qty-wanted debit-acct location)
  (let ((txs (release-product-tx tables sku qty-wanted debit-acct location)))
    (loop for it in txs
	  collect
	  (destructuring-bind (inv-tx debit-tx credit-tx) it
	    (list (put-inventory-tx (getf tables :inventory-allocations) inv-tx)
		  (append-tx (getf tables :gl-transactions) debit-tx credit-tx))))))
