;;;; State

(in-package #:ledger-cl)


(defparameter *account-table* (make-hash-table))
(defparameter *product-table* (make-hash-table :test #'EQUAL))
(defparameter *batch-history-table* (make-hash-table :test #'EQUAL))
(defparameter *allocations-history-table* (make-hash-table :test #'EQUAL))
(defparameter *locations-table* (make-hash-table))

(defparameter *transactions* (make-array 1 :fill-pointer 0 :adjustable t))

(defparameter *tables*
  (list
   :accounts *account-table*
   :products *product-table*
   :inventory-batch *batch-history-table*
   :inventory-allocations *allocations-history-table*
   :locations *locations-table*
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
   '("PROD-001" "Example Product" 1401 5001)
   '("PROD-002" "Another Product" 1401 5001)
   '("PROD-003" "Mutated List" 1402 5001)
   '("PROD-004" "Last Thing We Work With" 1402 5001 123456)))

(defparameter *locations*
  (list
   '(:shipped)
   '(:default)
   ))


;; Interface

(build-accts-table *account-table* *accounts*)
(build-locations-table *locations-table* *locations*)
(build-products-table *product-table* *tables* *products*)

(using
 (mk-receiver-line "PROD-002" 5.0 36.78 1401 2001 :default)
 (receive-product *tables*))

(release-product *tables* "PROD-002" 2 5001 :shipped)
