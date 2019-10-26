#lang racket
(require 2htdp/batch-io)

;;------------------------------------------------------------------
;;------------------ auxiliary functions ---------------------------
;;------------------------------------------------------------------

;; string to boolean
(define (string->boolean str)
  (cond
    [(equal? str "true") #t]
    [else #f])
  )

;; select n-th part of string
(define (getS str n)
  (getN (string-split (string-replace str "." " ")) n)
  )

;; select n=th part of list
(define (getN lst n)
  (cond
    [(null? lst) #f]
    [(eqv? n 0) (car lst)]
    [else (getN (cdr lst) (- n 1))]
    )
  )

;; select sn part of n-th part of list of strings
(define (getNS lst n sn)
  (getS (getN lst n) sn)
  )

;;------------------------------------------------------------------
;;--------------------- struct definition --------------------------
;;------------------------------------------------------------------

(struct AccountType (type currentAccount bankFee minDep monthly period renewable interestRate credit variable span increaseRate hasCheque hasCard transferFee) #:mutable #:transparent)
(struct LoanType (type amount blockingMoney span interest lastLoan minCredit) #:mutable #:transparent)

(struct Customer (id account loans) #:mutable #:transparent)
(struct Account (accountType balance minBalance credit timeOpened reopenTime firstBalance interestRate badTimer) #:mutable #:transparent)
(struct Loan (loanType startMonth hasWithdrawn unpaid) #:mutable #:transparent)

;;------------------------------------------------------------------
;;--------------------- global variables ---------------------------
;;------------------------------------------------------------------

;; Initialize
(define setups `())
(define commands `())

;; primary variables
(define month 0)
(define customers `())
(define loanTypes `())
(define accountTypes `())

;;------------------------------------------------------------------
;;---------------- input processing functions ----------------------
;;------------------------------------------------------------------

(define lines (file->lines (read-line (current-input-port) 'any)))
(define (process lines flag)
  (cond
    [(null? lines) #f]
    [(eqv? (string-length (car lines)) 0) (process (cdr lines) flag)]
    [(equal? "setup" (car lines)) (process (cdr lines) 0)]
    [(equal? "commands" (car lines)) (process (cdr lines) 1)]
    [(eqv? flag 0) (begin (set! setups (append setups (list (car lines)))) (process (cdr lines) 0))]
    [(eqv? flag 1) (begin (set! commands (append commands (list (car lines)))) (process (cdr lines) 1))]
    )
  )

(define (processLines unprocessedLines)
  (cond
    [(null? unprocessedLines) #f]
    [else
     (begin (processCurrentLine (car unprocessedLines) unprocessedLines)
            (processLines (cdr unprocessedLines)))]
    )
  )

(define (processCurrentLine line allLines)
  (cond
    [(null? line) #f]
    [(matchSetupFunction? line) (doSetupFunction allLines)]
    [(matchCommandFunction? line)
     (begin
       (displayln line)
       (doCommandFunction line)
       (displayln customers)
       )
     ]
    [else #f])
  )

(define (matchSetupFunction? line)
  (cond
    [(null? line) #f]
    [(or (equal? (getS line 0) "Account")
         (equal? (getS line 0) "Loan")) #t]
    [else #f])
  )

(define (doSetupFunction multiLines)
  (cond
    [(null? multiLines) #f]
    [(equal? (getNS multiLines 0 0) "Account")
     (set! accountTypes (append accountTypes (list (AccountType
                                                    (getNS multiLines 0 2)
                                                    (string->boolean (getNS multiLines 1 1))
                                                    (string->number (getNS multiLines 2 1))
                                                    (string->number (getNS multiLines 3 1))
                                                    (string->boolean (getNS multiLines 4 1))
                                                    (string->number (getNS multiLines 5 1))
                                                    (string->boolean (getNS multiLines 6 1))
                                                    (string->number (getNS multiLines 7 1))
                                                    (string->number (getNS multiLines 8 1))
                                                    (string->boolean (getNS multiLines 9 1))
                                                    (string->number (getNS multiLines 10 1))
                                                    (string->number (getNS multiLines 11 1))
                                                    (string->boolean (getNS multiLines 12 1))
                                                    (string->boolean (getNS multiLines 13 1))
                                                    (string->number (getNS multiLines 14 1))))))]
    [(equal? (getNS multiLines 0 0) "Loan")
     (set! loanTypes (append loanTypes (list (LoanType
                                              (getNS multiLines 0 2)
                                              (string->number (getNS multiLines 1 1))
                                              (string->number (getNS multiLines 2 1))
                                              (string->number (getNS multiLines 3 1))
                                              (string->number (getNS multiLines 4 1))
                                              (string->number (getNS multiLines 5 1))
                                              (string->number (getNS multiLines 6 1))))))]
    [else #f])
  )

(define (matchCommandFunction? line)
  (cond
    [(or (null? line) (not(or (equal? (getS line 0) "Customer")(equal? (getS line 0) "Time")))) #f]
    [(or (equal? (getS line 2) "by")
         (equal? (getS line 2) "wants")
         (equal? (getS line 2) "adds")
         (equal? (getS line 2) "requests")
         (equal? (getS line 2) "writes")
         (equal? (getS line 2) "spends")
         (equal? (getS line 2) "transfers")
         (equal? (getS line 2) "withdraws")
         (equal? (getS line 2) "closes")
         (equal? (getS line 2) "pays")) #t]
    [else #f])
  )

(define (doCommandFunction line)
  (cond
    [(null? line) #f]
    [(equal? (getS line 2) "by")
     (nextMonth)]
    [(equal? (getS line 2) "wants")
     (let ([accountType (searchAccountType (getS line 9) accountTypes)])
       (unless (equal? accountType #f)
         (openAccount (getS line 1) accountType (string->number (getS line 16))))
       )]
    [(equal? (getS line 2) "adds")
     (let ([usr (searchCustomer (getS line 1) customers)])
       (unless (equal? usr #f)
         (addToAccount usr (string->number (getS line 3))))
       )]
    [(and (equal? (getS line 2) "requests") (equal? (getS line 3) "renewal"))
     (let ([usr (searchCustomer (getS line 1) customers)])
       (unless (equal? usr #f)
         (requestRenewal usr))
       )]
    [(equal? (getS line 2) "writes")
     (let ([usr (searchCustomer (getS line 1) customers)])
       (unless (equal? usr #f)
         (writeCheque usr (string->number (getS line 6)))
         )
       )]
    [(equal? (getS line 2) "spends")
     (let ([usr (searchCustomer (getS line 1) customers)])
       (unless (equal? usr #f)
         (spend usr (string->number (getS line 3)))
         )
       )]
    [(equal? (getS line 2) "trasfers")
     (let ([usr (searchCustomer (getS line 1) customers)])
       (unless (equal? usr #f)
         (transfer usr (string->number (getS line 3)))
         )
       )]
    [(and (equal? (getS line 2) "withdraws") (equal? (getS line 7) "account"))
     (let ([usr (searchCustomer (getS line 1) customers)])
       (unless (equal? usr #f)
         (spend usr (string->number (getS line 3)))
         )
       )]
    [(equal? (getS line 2) "closes")
     (let ([id (getS line 1)])
       (let ([usr (searchCustomer id customers)])
         (unless (equal? usr #f)
           (closeAccount id))
         )
       )]
    [(and (equal? (getS line 2) "requests") (equal? (getS line 4) "loan"))
     (createLoan (getS line 1) (getS line 7))]
    [(equal? (getS line 2) "pays")
     (pays (getS line 1) (string->number (getS line 3)))]
    [(and (equal? (getS line 2) "withdraws") (equal? (getS line 4) "loan"))
     (withdraw (getS line 1))]
    [else (displayln "ERROR")])
  )

;;------------------------------------------------------------------
;;--------------------- getting functions --------------------------
;;------------------------------------------------------------------

;; get last item
(define (lastItem lst)
  (cond [(null? (cdr lst)) (car lst)]
        [else (lastItem (cdr lst))]
        )
  )

;; search for customer by id
;; return false if notfound
(define (searchCustomer id customers)
  (cond
    [(null? customers) #f]
    [(equal? id (Customer-id (car customers))) (car customers)]
    [else (searchCustomer id (cdr customers))]
    )
  )

;; search for AccountType by type
;; return false if notfound
(define (searchAccountType type accountTypes)
  (cond
    [(null? accountTypes) #f]
    [(equal? type (AccountType-type (car accountTypes))) (car accountTypes)]
    [else (searchAccountType type (cdr accountTypes))]
    )
  )

;; search for LoanType by type
;; return false if notfound
(define (searchLoanType type loanTypes)
  (cond
    [(null? loanTypes) #f]
    [(equal? type (LoanType-type (car loanTypes))) (car loanTypes)]
    [(eqv? type (LoanType-type (car loanTypes))) (car loanTypes)]
    [else (searchLoanType type (cdr loanTypes))])
  )

;; delete the customer
;; return list of customers
(define (deleteCustomer id lst)
  (cond
    [(null? lst) `()]
    [(equal? id (Customer-id (car lst))) (cdr lst)]
    [else (cons (car lst) (deleteCustomer id (cdr lst)))]
    )
  )

;; get account type from customer id
;; what if there is no account
;; what if there is no accountType
(define (getAccountType id customers)
  (let ([customer (searchCustomer id customers)])
    (let ([account (Customer-account customer)])
      (let ([accountType (Account-accountType account)])
        accountType)
      )
    ))



(define (deleteLoan loan lst)
  (cond
    [(null? lst) `()]
    [(equal? loan  (car lst)) (cdr lst)]
    [else (cons (car lst) (deleteLoan loan (cdr lst)))]
    )
  )
;;------------------------------------------------------------------
;;--------------------- command functions --------------------------
;;------------------------------------------------------------------

;; Time goes by.
(define (nextMonth)
  (begin (set! month (+ 1 month))
         (spanForIncrease customers)
         (setMinBalance customers)
         (monthlyProfit customers)
         (yearlyProfit customers)
         (loanAdds customers))
  )

(define (loanAdds customers)
  (unless (null? customers)
    (let ([loans (Customer-loans (car customers))]
          [account (Customer-account (car customers))])
      (loansAdd loans account))
    (loanAdds (cdr customers))
    )
  )

(define (loansAdd loans account)
  (unless (null? loans)
    (let ([loan (car loans)])
      (let ([loanType (Loan-loanType loan)])
        (begin
          (when (and(equal?  0 (remainder (- month (Loan-startMonth loan)) 12))(not(equal? (Loan-startMonth loan) month)))
            (set-Loan-unpaid! loan (exact-floor(*(+ 1 (/(LoanType-interest loanType)100))(Loan-unpaid loan)))))
          (when (equal? (Loan-startMonth loan) month)
          (set-Account-balance! account (+ (Account-balance account) (LoanType-amount loanType)))))))
    (loansAdd (cdr loans) account))
  )

(define (spanForIncrease customers)
  (unless (null? customers)
    (let ([account (Customer-account (car customers))])
      (let ([accountType (Account-accountType account)]
            [interestRate (Account-interestRate account)])
        (begin
          (set-Account-badTimer! account (+ (Account-badTimer account) 1))
          (when
              (and (AccountType-variable accountType)
                   (> (Account-badTimer account) (AccountType-span accountType))
                   (>= (+ (Account-reopenTime account) (AccountType-period accountType)) month))
            (set-Account-interestRate! account (+ interestRate (AccountType-increaseRate accountType)))
            (set-Account-badTimer! account 0)
            (set-Account-credit! account (+ (Account-credit account) (AccountType-credit accountType)))))))
    (spanForIncrease (cdr customers))
    )
  )

;; what about first month ? 
(define (setMinBalance customers)
  (unless (null? customers)
    (let ([account (Customer-account(car customers))])
      (when (> (Account-balance account) (Account-minBalance account))
        (set-Account-minBalance! account (Account-balance account))))
    (setMinBalance (cdr customers))
    )
  )

(define (monthlyProfit customers)
  (unless (null? customers)
    (let ([account (Customer-account (car customers))])
      (let ([balance (Account-balance account)]
            [accountType (Account-accountType account)])
        (when (and (not (AccountType-currentAccount accountType))
                   (AccountType-monthly accountType)
                   (>= (+ (Account-reopenTime account) (AccountType-period accountType)) month))
          (set-Account-balance! account (exact-floor (+ balance (* balance (/ (Account-interestRate account) 1200))))))))
    (monthlyProfit (cdr customers))
    )
  )

(define (yearlyProfit customers)
  (unless (null? customers)
    (let ([account (Customer-account (car customers))])
      (let ([balance (Account-balance account)]
            [accountType (Account-accountType account)])
        (begin
          (when (equal?  0 (remainder (- month (Account-timeOpened account)) 12))
            (set-Account-credit! account (+ (Account-credit account) (AccountType-credit accountType))))
          (when (and (equal?  0 (remainder (- month (Account-reopenTime account)) 12))
                     (not (AccountType-currentAccount accountType))
                     (not (AccountType-monthly accountType))
                     (>= (+ (Account-reopenTime account) (AccountType-period accountType)) month))
            (set-Account-balance! account (+ balance (* (Account-minBalance account) (/ (Account-interestRate account) 100))))))))
    (yearlyProfit (cdr customers))
    )
  )

;; Customer 1 wants to create an account of type 1. Customer 1 wants to start with 3000000 tomans.
(define (openAccount id accountType initBalance)
  (let ([fee (AccountType-bankFee accountType)]
        [minBal (AccountType-minDep accountType)])
    (let ([newBal (- initBalance fee)])
      (when (> newBal minBal)
        (let ([newAccount (Account accountType newBal newBal 0  month month newBal (AccountType-interestRate accountType) 0)])
          (set! customers (append customers (list (Customer id newAccount `())))))
        )
      )
    )
  )

;; what if there is no account for this customer
;; Customer 1 adds 1000000 tomans to his account.
(define (addToAccount customer amount)
  (let ([account (Customer-account customer)])
    (let ([balance (Account-balance account)])
      (set-Account-balance! account (+ balance amount))
      )
    )
  )

;; Customer 1 requests renewal.
(define (requestRenewal customer)
  (let ([account (Customer-account customer)])
    (let ([accountType (Account-accountType account)])
      (when (and (AccountType-renewable accountType) (< (+ (Account-reopenTime account) (AccountType-period accountType)) month))
        (set-Account-reopenTime! account month)))
    )
  )

;; Customer 1 writes a cheque for 2000000 Tomans.
(define (writeCheque customer amount)
  (let ([account (Customer-account customer)])
    (let ([balance (Account-balance account)]
          [accountType (Account-accountType account)])
      (if (equal? (AccountType-hasCheque accountType) #t)
          (unless (< (- balance amount) (+(getAll‌BlockingMoney (Customer-loans customer))(AccountType-minDep (Account-accountType account))))
            (set-Account-balance! account (- balance amount))
            (set-Account-badTimer! account 0))
          (set-Account-credit! account (exact-floor (- (Account-credit account) (/ (AccountType-credit accountType) 2)))))
      )
    )
  )

;; Customer 1 spends 70000 Tomans via his card.
;; Customer 1 withdraws 120000 Tomans from his account.
(define (spend customer amount)
  (let ([account (Customer-account customer)])
    (let ([balance (Account-balance account)]
          [accountType (Account-accountType account)])
      (if (equal? (AccountType-hasCard accountType) #t)
          (unless (< (- balance amount) (+(getAll‌BlockingMoney (Customer-loans customer))(AccountType-minDep (Account-accountType account))))
            (set-Account-balance! account (- balance amount))
            (set-Account-badTimer! account 0))
          (when (< (+ (Account-reopenTime account) (AccountType-period accountType)) month)
            (set-Account-credit! account (exact-floor (- (Account-credit account) (/ (AccountType-credit accountType) 2)))))
          )
      )
    )
  )

;; Customer 1 transfers 500000 Tomans.
(define (transfer customer amount)
  (let ([account (Customer-account customer)])
    (let ([balance (Account-balance account)]
          [accountType (Account-accountType account)])
      (let ([amountX (+ amount (AccountType-transferFee accountType))])
        (if (equal? (AccountType-hasCard accountType) #t)
            (unless (< (- balance amountX) (+(getAll‌BlockingMoney (Customer-loans customer))(AccountType-minDep (Account-accountType account))))
              (set-Account-balance! account (- balance amountX))
              (set-Account-badTimer! account 0))
            (when (< (+ (Account-reopenTime account) (AccountType-period accountType)) month)
              (set-Account-credit! account (exact-floor (- (Account-credit account) (/ (AccountType-credit accountType) 2)))))
            )
        
        )
      )
    )
  )

;; Customer 1 closes his account.
(define (closeAccount id)
  (set! customers (deleteCustomer id customers))
  )

;;Customer 1 requests a loan of type 1.
(define (createLoan id loanType)
  (let ([customer (searchCustomer id customers)]
        [loanType (searchLoanType loanType loanTypes)])
    (unless (or (equal? loanType #f) (equal? customer #f))
      (displayln (Account-credit (Customer-account customer)))
      (let ([account (Customer-account customer)])
        (when
            (and
             (>= (Account-credit account) (LoanType-minCredit loanType))
             (>= (Account-balance account) (+(LoanType-blockingMoney loanType)(+(getAll‌BlockingMoney (Customer-loans customer))(AccountType-minDep (Account-accountType account)))))
             (>= (- month (getLastLoanValue (Customer-loans customer))) (LoanType-lastLoan loanType)))
          (displayln "Loan got!")
          (let ([newLoan (Loan loanType (+ month 1) #f (exact-floor(*(+ 1 (/(LoanType-interest loanType)100))(LoanType-amount loanType))))])
            (begin(set-Customer-loans! customer (append (Customer-loans customer) (list newLoan)))
                  (set-Account-credit! account(- (Account-credit account) (LoanType-minCredit loanType)))))
          ))
      )
    )
  )

(define (getLastLoanValue loans)
  (cond
    [(null? loans) -1000000]
    [else
     (if (>= (Loan-startMonth (car loans)) (getLastLoanValue (cdr loans)))
         (Loan-startMonth (car loans))
         (getLastLoanValue cdr loans))])
  )

(define (getLastLoan loans)
  (cond
    [(null? loans) #f]
    [else
     (if (>= (Loan-startMonth (car loans)) (getLastLoanValue (cdr loans)))
         (car loans)
         (getLastLoan cdr loans))])
  )
  
;; Customer 1 pays 1000000 Tomans of his debt.
(define (pays id amount)
  (let ([customer (searchCustomer id customers)])
    (let ([account (Customer-account customer)]
          [loan (getLastLoan (Customer-loans customer))])
      (when (>=(Account-balance account) (+ amount (+(getAll‌BlockingMoney (Customer-loans customer))(AccountType-minDep (Account-accountType account)))))
        (if(>= amount (Loan-unpaid loan))
           (begin(set-Account-balance! account (- (Account-balance account) (Loan-unpaid loan)))
                 (set-Loan-unpaid! loan 0)
                 (set-Customer-loans! customer (deleteLoan loan (Customer-loans customer))))
           (begin(set-Account-balance! account (- (Account-balance account) amount))
                 (set-Loan-unpaid! loan (- (Loan-unpaid loan) amount)))))
      )
    )
  )

(define (getFirstNotWithdrawnLoan loans)
  (cond
    [(null? loans) #f]
    [(equal? #f (Loan-hasWithdrawn (car loans))) (car loans)]
    [else (getFirstNotWithdrawnLoan (cdr loans))])
  )

(define (getAll‌BlockingMoney loans)
  (cond
    [(null? loans) 0]
    [else (let ([loanType (Loan-loanType (car loans))])
            (+ (LoanType-blockingMoney loanType) (getAll‌BlockingMoney (cdr loans)))
            )]
    )
  )

(define (getAll‌UnpainMoney loans)
  (cond
    [(null? loans) 0]
    [else (+ (Loan-unpaid (car loans)) (getAll‌UnpainMoney (cdr loans)))]
    )
  )

;; Customer 1 withdraws the loan.
(define (withdraw id lst)
  (let ([customer (searchCustomer id customers)])
    (let ([loans (Customer-loans customer)]
          [account (Customer-account customer)])
      (let ([loan (getFirstNotWithdrawnLoan loans)])
        (when (and (not (Loan-hasWithdrawn loan)) (<= (+ (AccountType-minDep (Account-accountType account)) (getAll‌BlockingMoney loans)) (Account-balance account)))
          (set-Loan-hasWithdrawn! loan #t))
        )
      )
    )
  )

;;------------------------------------------------------------------
;;--------------------- output functions ---------------------------
;;------------------------------------------------------------------

(define (calculateCustomers lst)
  (cond
    [(null? lst) ""]
    [else (let ([customer (car lst)])
            (let ([loans (Customer-loans customer)]
                  [account (Customer-account customer)])
              (let ([accountType (Account-accountType account)])
                (begin
                  ;;(displayln (Loan-loanType (getLastLoan loans)))
                (format "Customer ~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a"
                        (Customer-id customer)
                        (Account-firstBalance account)
                        (Account-balance account)
                        (+ (Account-reopenTime account) (AccountType-period accountType))
                        (Account-credit account)
                        (if(AccountType-currentAccount accountType) 0 (Account-interestRate account))
                        (if(null? loans) 0 (LoanType-amount (Loan-loanType (getLastLoan loans))))
                        (if(null? loans) 0 (Loan-startMonth (getLastLoan loans)))
                        (getAll‌BlockingMoney loans)
                        (if(null? loans) 0 (+ (Loan-startMonth (getLastLoan loans)) (LoanType-span (Loan-loanType (getLastLoan loans)))))
                        (getAll‌UnpainMoney loans)
                        (calculateCustomers (cdr lst)))
                )
                )
              )
            )]
    )
  )

(define (outputFunction)
  (format "month ~a\n~a" month (calculateCustomers customers)))

;; run
(begin
  (process lines 0)
  (processLines setups)
  (processLines commands)
  (write-file "output.txt" (outputFunction)))