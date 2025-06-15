;;; roulette-calculator.el --- Comprehensive roulette betting calculator -*- lexical-binding: t; read-symbol-shorthands: (("rc-" . "roulette-calculator-")); -*-
;;
;; Copyright (C) 2025 StrawberryTea
;;
;; Author: StrawberryTea <look@strawberrytea.xyz>
;; Maintainer: StrawberryTea <look@strawberrytea.xyz>
;; Created: June 13, 2025
;; Modified: June 13, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc
;; Homepage: https://github.com/LemonBreezes/roulette-calculator
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; A comprehensive roulette calculator supporting American and European roulette
;; with various betting strategies and special bets (voisins, orphelins, tiers)

;;; Code:
(declare-function evil-define-key "evil-core")
(declare-function evil-snipe-local-mode "evil-snipe")

(require 'cl-lib)

;; Helper functions
(defun rc-get-special-bet-chips (bet-type)
  "Get number of chips required for special BET-TYPE."
  (cond
   ((eq bet-type 'voisins) 9)
   ((eq bet-type 'orphelins) 5)
   ((eq bet-type 'tiers) 6)
   (t 1)))

;; Data structures
(defconst rc-european-numbers
  [0 32 15 19 4 21 2 25 17 34 6 27 13 36 11 30 8 23 10 5 24 16 33 1 20 14 31 9 22 18 29 7 28 12 35 3 26]
  "European roulette wheel number sequence.")

(defconst rc-american-numbers
  [0 28 9 26 30 11 7 20 32 17 5 22 34 15 3 24 36 13 1 00 27 10 25 29 12 8 19 31 18 6 21 33 16 4 23 35 14 2 37]
  "American roulette wheel number sequence (00 represented as 37).")

(defconst rc-red-numbers
  [1 3 5 7 9 12 14 16 18 19 21 23 25 27 30 32 34 36]
  "Red numbers on roulette wheel.")

(defconst rc-black-numbers
  [2 4 6 8 10 11 13 15 17 20 22 24 26 28 29 31 33 35]
  "Black numbers on roulette wheel.")

;; Special bet definitions
(defconst rc-voisins-numbers
  [22 18 29 7 28 12 35 3 26 0 32 15 19 4 21 2 25]
  "Numbers covered by Voisins du Zero bet.")

(defconst rc-orphelins-numbers
  [1 20 14 31 9 17 34 6]
  "Numbers covered by Orphelins bet.")

(defconst rc-tiers-numbers
  [27 13 36 11 30 8 23 10 5 24 16 33]
  "Numbers covered by Tiers du Cylindre bet.")

(defvar rc-game-type 'european
  "Current game type: 'european or 'american.")

(defvar rc-bankroll 1000.0
  "Current bankroll amount.")

(defvar rc-bet-history '()
  "History of bets placed.")

(defvar rc-current-bets '()
  "Current bets for this spin.")

(defvar rc-last-cleared-bets '()
  "Last set of bets that were cleared.")

(defvar rc-spin-history '()
  "History of spin results.")

;; Betting structure
(cl-defstruct rc-bet
  type      ; Symbol: 'straight, 'split, 'street, 'corner, 'line, 'dozen, 'column, 'red, 'black, 'even, 'odd, 'low, 'high, 'voisins, 'orphelins, 'tiers
  numbers   ; List of numbers covered
  amount    ; Bet amount
  payout)   ; Payout ratio

;; Core functions
(defun rc-get-payout (bet-type)
  "Return payout ratio for BET-TYPE."
  (cond
   ((eq bet-type 'straight) 35)
   ((eq bet-type 'split) 17)
   ((eq bet-type 'street) 11)
   ((eq bet-type 'corner) 8)
   ((eq bet-type 'line) 5)
   ((memq bet-type '(dozen column)) 2)
   ((memq bet-type '(red black even odd low high)) 1)
   ((eq bet-type 'voisins) nil)  ; Complex bet
   ((eq bet-type 'orphelins) nil) ; Complex bet
   ((eq bet-type 'tiers) nil)     ; Complex bet
   (t 0)))

(defun rc-number-color (number)
  "Return color of NUMBER ('red, 'black, or 'green)."
  (cond
   ((= number 0) 'green)
   ((and (eq rc-game-type 'american) (= number 37)) 'green) ; 00
   ((memq number (append rc-red-numbers nil)) 'red)
   ((memq number (append rc-black-numbers nil)) 'black)
   (t nil)))

(defun rc-is-even (number)
  "Check if NUMBER is even (excluding 0 and 00)."
  (and (> number 0) (< number 37) (= (mod number 2) 0)))

(defun rc-is-odd (number)
  "Check if NUMBER is odd."
  (and (> number 0) (< number 37) (= (mod number 2) 1)))

(defun rc-is-low (number)
  "Check if NUMBER is in low range (1-18)."
  (and (>= number 1) (<= number 18)))

(defun rc-is-high (number)
  "Check if NUMBER is in high range (19-36)."
  (and (>= number 19) (<= number 36)))

(defun rc-get-dozen (number)
  "Get dozen for NUMBER (1, 2, 3, or nil)."
  (cond
   ((and (>= number 1) (<= number 12)) 1)
   ((and (>= number 13) (<= number 24)) 2)
   ((and (>= number 25) (<= number 36)) 3)
   (t nil)))

(defun rc-get-column (number)
  "Get column for NUMBER (1, 2, 3, or nil)."
  (cond
   ((= number 0) nil)
   ((= number 37) nil) ; 00
   ((= (mod number 3) 1) 1)
   ((= (mod number 3) 2) 2)
   ((= (mod number 3) 0) 3)
   (t nil)))

(defun rc-calculate-bet-result (bet winning-number)
  "Calculate result of BET given WINNING-NUMBER."
  (let ((win-amount 0))
    (cond
     ;; Straight up bet
     ((eq (rc-bet-type bet) 'straight)
      (when (memq winning-number (rc-bet-numbers bet))
        (setq win-amount (* (rc-bet-amount bet) (1+ (rc-bet-payout bet))))))

     ;; Split, street, corner, line bets
     ((memq (rc-bet-type bet) '(split street corner line))
      (when (memq winning-number (rc-bet-numbers bet))
        (setq win-amount (* (rc-bet-amount bet) (1+ (rc-bet-payout bet))))))

     ;; Dozen bet
     ((eq (rc-bet-type bet) 'dozen)
      (when (eq (rc-get-dozen winning-number) (car (rc-bet-numbers bet)))
        (setq win-amount (* (rc-bet-amount bet) (1+ (rc-bet-payout bet))))))

     ;; Column bet
     ((eq (rc-bet-type bet) 'column)
      (when (eq (rc-get-column winning-number) (car (rc-bet-numbers bet)))
        (setq win-amount (* (rc-bet-amount bet) (1+ (rc-bet-payout bet))))))

     ;; Color bets
     ((eq (rc-bet-type bet) 'red)
      (when (eq (rc-number-color winning-number) 'red)
        (setq win-amount (* (rc-bet-amount bet) 2))))

     ((eq (rc-bet-type bet) 'black)
      (when (eq (rc-number-color winning-number) 'black)
        (setq win-amount (* (rc-bet-amount bet) 2))))

     ;; Even/odd bets
     ((eq (rc-bet-type bet) 'even)
      (when (rc-is-even winning-number)
        (setq win-amount (* (rc-bet-amount bet) 2))))

     ((eq (rc-bet-type bet) 'odd)
      (when (rc-is-odd winning-number)
        (setq win-amount (* (rc-bet-amount bet) 2))))

     ;; Low/high bets
     ((eq (rc-bet-type bet) 'low)
      (when (rc-is-low winning-number)
        (setq win-amount (* (rc-bet-amount bet) 2))))

     ((eq (rc-bet-type bet) 'high)
      (when (rc-is-high winning-number)
        (setq win-amount (* (rc-bet-amount bet) 2))))

     ;; Special bets
     ((memq (rc-bet-type bet) '(voisins orphelins tiers))
      (when (memq winning-number (rc-bet-numbers bet))
        ;; These are complex bets with multiple sub-bets
        ;; For simplicity, we'll calculate based on the specific number hit
        (setq win-amount (rc-calculate-special-bet-payout bet winning-number)))))

    (- win-amount (rc-bet-amount bet))))

(defun rc-calculate-special-bet-payout (bet winning-number)
  "Calculate payout for special BET types given WINNING-NUMBER."
  (let ((chip-value (/ (rc-bet-amount bet)
                       (rc-get-special-bet-chips (rc-bet-type bet)))))
    (cond
     ((eq (rc-bet-type bet) 'voisins)
      ;; Voisins du Zéro uses 9 chips placed as follows:
      ;; - 2 chips on 0-2-3 trio (pays 11:1, returns 12x per chip)
      ;; - 2 chips on 25-26-28-29 corner (pays 8:1, returns 9x per chip)
      ;; - 1 chip each on splits: 4-7, 12-15, 18-21, 19-22, 32-35 (pays 17:1, returns 18x per chip)
      (cond
       ;; Trio bet on 0-2-3 with 2 chips
       ((memq winning-number '(0 2 3)) 
        (* chip-value 24))  ; 2 chips × 12 (11:1 + stake) = 24 chips return
       
       ;; Corner bet on 25-26-28-29 with 2 chips
       ((memq winning-number '(25 26 28 29)) 
        (* chip-value 18))  ; 2 chips × 9 (8:1 + stake) = 18 chips return
       
       ;; Split bets with 1 chip each
       ;; 4-7 split
       ((memq winning-number '(4 7)) (* chip-value 18))
       ;; 12-15 split
       ((memq winning-number '(12 15)) (* chip-value 18))
       ;; 18-21 split
       ((memq winning-number '(18 21)) (* chip-value 18))
       ;; 19-22 split
       ((memq winning-number '(19 22)) (* chip-value 18))
       ;; 32-35 split
       ((memq winning-number '(32 35)) (* chip-value 18))
       
       (t 0)))  ; Number not covered by Voisins

     ((eq (rc-bet-type bet) 'orphelins)
      ;; Orphelins uses 5 chips placed as follows:
      ;; - 1 chip straight up on 1 (pays 35:1, returns 36x)
      ;; - 1 chip each on splits: 6-9, 14-17, 17-20, 31-34 (pays 17:1, returns 18x per chip)
      ;; Note: 17 is covered by two splits, so it wins twice
      (cond
       ;; Straight up bet on 1
       ((= winning-number 1) 
        (* chip-value 36))  ; 1 chip × 36 (35:1 + stake) = 36 chips return
       
       ;; Number 17 is special - covered by both 14-17 and 17-20 splits
       ((= winning-number 17) 
        (* chip-value 36))  ; Win on two splits: 18 + 18 = 36 chips return
       
       ;; Single split coverage
       ((memq winning-number '(6 9 14 20 31 34)) 
        (* chip-value 18))  ; 1 chip × 18 (17:1 + stake) = 18 chips return
       
       (t 0)))  ; Number not covered by Orphelins

     ((eq (rc-bet-type bet) 'tiers)
      ;; Tiers du Cylindre uses 6 chips on splits: 5-8, 10-11, 13-16, 23-24, 27-30, 33-36
      ;; All are split bets (pays 17:1, returns 18x per chip)
      (if (memq winning-number (rc-bet-numbers bet))
          (* chip-value 18)  ; 1 chip × 18 (17:1 + stake) = 18 chips return
        0))

     (t 0))))

(defun rc-spin ()
  "Simulate a roulette spin and return winning number."
  (let ((max-num (if (eq rc-game-type 'american) 37 36)))
    (random (1+ max-num))))

(defun rc-format-number (number)
  "Format NUMBER for display."
  (cond
   ((= number 37) "00")
   ((= number 0) "0")
   (t (number-to-string number))))

(defun rc-number-display-properties (number)
  "Get display properties for NUMBER."
  (let ((color (rc-number-color number)))
    (cond
     ((eq color 'red) '(face (:foreground "red" :weight bold)))
     ((eq color 'black) '(face (:foreground "gray50" :weight bold)))
     ((eq color 'green) '(face (:foreground "green" :weight bold)))
     (t nil))))

;; Betting strategies
(defun rc-martingale-next-bet (last-bet won-p)
  "Calculate next bet amount using Martingale strategy."
  (if won-p
      1  ; Reset to base bet
    (* last-bet 2)))  ; Double after loss

(defun rc-fibonacci-sequence (n)
  "Generate first N numbers of Fibonacci sequence."
  (let ((seq '(1 1)))
    (dotimes (_ (- n 2))
      (setq seq (append seq (list (+ (nth (- (length seq) 1) seq)
                                     (nth (- (length seq) 2) seq))))))
    seq))

(defun rc-fibonacci-next-bet (position won-p)
  "Calculate next bet using Fibonacci strategy at POSITION."
  (let ((fib-seq (rc-fibonacci-sequence 20)))
    (if won-p
        (nth (max 0 (- position 2)) fib-seq)
      (nth (min (1+ position) (1- (length fib-seq))) fib-seq))))

(defun rc-labouchere-next-bet (sequence won-p last-bet)
  "Calculate next bet using Labouchere strategy."
  (cond
   ((null sequence) 1)  ; Start new sequence
   ((= (length sequence) 1) (car sequence))
   (won-p
    ;; Remove first and last numbers
    (if (> (length sequence) 2)
        (+ (car (cdr sequence)) (car (last (butlast sequence))))
      1))
   (t
    ;; Add last bet to sequence
    (+ (car sequence) (car (last sequence))))))

;; Interactive functions
(defun rc-apply-betting-strategy (won-p)
  "Apply the current betting strategy after a spin.
WON-P indicates whether the last spin was a win."
  (cond
   ;; Martingale strategy
   ((eq rc-strategy-mode 'martingale)
    (let* ((base-bet (plist-get rc-strategy-state :base-bet))
           (current-multiplier (or (plist-get rc-strategy-state :current-multiplier) 1))
           (new-multiplier (if won-p 1 (* current-multiplier 2)))
           (has-even-money nil))
      ;; Update strategy state
      (plist-put rc-strategy-state :current-multiplier new-multiplier)
      (plist-put rc-strategy-state :last-won won-p)
      
      ;; Check if we have even-money bets
      (dolist (bet rc-current-bets)
        (when (member (rc-bet-type bet) '(red black even odd low high))
          (setq has-even-money t)))
      
      (if has-even-money
          ;; Handle even-money bets
          (let ((new-bets '())
                (strategy-paused nil))
            (dolist (bet rc-current-bets)
              (if (member (rc-bet-type bet) '(red black even odd low high))
                  ;; Update even-money bet amount
                  (let ((new-amount (* base-bet new-multiplier)))
                    (if (> new-amount rc-bankroll)
                        (progn
                          (setq rc-strategy-mode nil)
                          (setq strategy-paused t)
                          (push bet new-bets))
                      (push (make-roulette-calculator-bet :type (rc-bet-type bet)
                                                          :numbers (rc-bet-numbers bet)
                                                          :amount new-amount
                                                          :payout (rc-bet-payout bet))
                            new-bets)))
                ;; Keep non-even-money bets unchanged
                (push bet new-bets)))
            (setq rc-current-bets (nreverse new-bets))
            (if strategy-paused
                (format "Martingale: Need $%d (only have $%.2f) - PAUSED"
                        (* base-bet new-multiplier) rc-bankroll)
              (format "Martingale: %s → $%d"
                      (if won-p "WIN" "LOSS")
                      (* base-bet new-multiplier))))
        
        ;; No even-money bets - scale all bets
        (let ((new-bets '())
              (total-needed 0))
          ;; Calculate total needed
          (dolist (bet rc-current-bets)
            (setq total-needed (+ total-needed (* (rc-bet-amount bet) new-multiplier))))
          
          (if (> total-needed rc-bankroll)
              (progn
                (setq rc-strategy-mode nil)
                (format "Martingale: Need $%.2f (only have $%.2f) - PAUSED"
                        total-needed rc-bankroll))
            ;; Scale all bets
            (dolist (bet rc-current-bets)
              (push (make-roulette-calculator-bet :type (rc-bet-type bet)
                                                  :numbers (rc-bet-numbers bet)
                                                  :amount (* (rc-bet-amount bet) new-multiplier)
                                                  :payout (rc-bet-payout bet))
                    new-bets))
            (setq rc-current-bets (nreverse new-bets))
            (format "Martingale: %s → %dx"
                    (if won-p "WIN" "LOSS")
                    new-multiplier))))))
   
   ;; Fibonacci strategy
   ((eq rc-strategy-mode 'fibonacci)
    (let* ((position (or (plist-get rc-strategy-state :position) 0))
           (new-position (if won-p (max 0 (- position 2)) (1+ position)))
           (fib-seq (rc-fibonacci-sequence 20))
           (new-bet (nth new-position fib-seq)))
      (plist-put rc-strategy-state :position new-position)
      (plist-put rc-strategy-state :last-won won-p)
      ;; Similar logic to Martingale for updating bets
      (when (and rc-current-bets 
                 (member (rc-bet-type (car rc-current-bets)) 
                         '(red black even odd low high)))
        (let ((bet-type (rc-bet-type (car rc-current-bets))))
          (setq rc-current-bets '())
          (if (> new-bet rc-bankroll)
              (progn
                (setq rc-strategy-mode nil)
                (format "Fibonacci: Need $%d (only have $%.2f) - PAUSED" 
                        new-bet rc-bankroll))
            (push (make-roulette-calculator-bet :type bet-type
                                                :numbers nil
                                                :amount new-bet
                                                :payout 1)
                  rc-current-bets)
            (format "Fibonacci: %s → Pos %d, $%d" 
                    (if won-p "WIN" "LOSS")
                    new-position new-bet))))))
   
   ;; Labouchere strategy
   ((eq rc-strategy-mode 'labouchere)
    (let* ((sequence (plist-get rc-strategy-state :sequence))
           (bet-type (when (and rc-current-bets 
                                (member (rc-bet-type (car rc-current-bets)) 
                                        '(red black even odd low high)))
                       (rc-bet-type (car rc-current-bets)))))
      (when (and sequence bet-type)
        (if won-p
            ;; Remove first and last numbers
            (setq sequence (if (> (length sequence) 2)
                               (butlast (cdr sequence))
                             nil))
          ;; Add the sum of first and last to the end
          (when sequence
            (setq sequence (append sequence 
                                   (list (+ (car sequence) 
                                            (car (last sequence))))))))
        (plist-put rc-strategy-state :sequence sequence)
        ;; Calculate next bet
        (if (null sequence)
            (progn
              (setq rc-strategy-mode nil)
              "Labouchere: Sequence completed!")
          (let ((new-bet (if (= (length sequence) 1)
                             (car sequence)
                           (+ (car sequence) (car (last sequence))))))
            (setq rc-current-bets '())
            (if (> new-bet rc-bankroll)
                (progn
                  (setq rc-strategy-mode nil)
                  (format "Labouchere: Need $%d (only have $%.2f) - PAUSED" 
                          new-bet rc-bankroll))
              (push (make-roulette-calculator-bet :type bet-type
                                                  :numbers nil
                                                  :amount new-bet
                                                  :payout 1)
                    rc-current-bets)
              (format "Labouchere: %s → Seq %s, $%d" 
                      (if won-p "WIN" "LOSS")
                      sequence new-bet)))))))
   
   ;; Default - no strategy message
   (t nil)))

(defun rc-calculator ()
  "Start the roulette calculator."
  (interactive)
  (switch-to-buffer "*Roulette Calculator*")
  (rc-mode)
  (rc-render-interface))

(defvar rc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'rc-spin-wheel)
    (define-key map "b" 'rc-place-bet)
    (define-key map "c" 'rc-clear-bets)
    (define-key map "t" 'rc-toggle-game-type)
    (define-key map "h" 'rc-show-history)
    (define-key map "?" 'rc-show-help)
    (define-key map "q" 'quit-window)
    (define-key map "r" 'rc-reset-bankroll)
    (define-key map "m" 'rc-martingale-mode)
    (define-key map "f" 'rc-fibonacci-mode)
    (define-key map "l" 'rc-labouchere-mode)
    (define-key map "a" 'rc-analyze-sequence)
    (define-key map "p" 'rc-repeat-last-bet)
    (define-key map "L" 'rc-toggle-strategy-loss-mode)
    map)
  "Keymap for roulette calculator mode.")

(define-derived-mode rc-mode special-mode "Roulette"
  "Major mode for roulette calculator.
\\{rc-mode-map}"
  (setq buffer-read-only t)
  ;; Set up Evil keybindings if Evil is loaded
  (when (and (boundp 'evil-mode) evil-mode)
    (evil-define-key 'normal rc-mode-map
      "s" 'rc-spin-wheel
      "b" 'rc-place-bet
      "c" 'rc-clear-bets
      "t" 'rc-toggle-game-type
      "h" 'rc-show-history
      "?" 'rc-show-help
      "q" 'quit-window
      "r" 'rc-reset-bankroll
      "m" 'rc-martingale-mode
      "f" 'rc-fibonacci-mode
      "l" 'rc-labouchere-mode
      "a" 'rc-analyze-sequence
      "p" 'rc-repeat-last-bet
      "L" 'rc-toggle-strategy-loss-mode)
    (when (boundp 'evil-snipe-local-mode)
      (evil-snipe-local-mode -1))))

(defun rc-calculate-all-outcomes ()
  "Calculate all possible outcomes and their net results for current bets."
  (let ((outcomes '())
        (total-numbers (if (eq rc-game-type 'american) 38 37)))
    ;; Check each possible number
    (dotimes (num total-numbers)
      (let ((net-result 0)
            (total-bet 0))
        ;; Calculate result for each bet
        (dolist (bet rc-current-bets)
          (setq total-bet (+ total-bet (rc-bet-amount bet)))
          (setq net-result (+ net-result (rc-calculate-bet-result bet num))))
        ;; Store outcome
        (push (list :number num :net net-result :total-return (+ total-bet net-result)) outcomes)))
    (nreverse outcomes)))

(defun rc-group-outcomes-by-net ()
  "Group outcomes by net result and calculate probabilities."
  (let ((outcomes (rc-calculate-all-outcomes))
        (grouped '())
        (total-numbers (if (eq rc-game-type 'american) 38 37)))
    ;; Group by net result
    (dolist (outcome outcomes)
      (let* ((net (plist-get outcome :net))
             (existing (assoc net grouped)))
        (if existing
            (setcdr existing (cons outcome (cdr existing)))
          (push (list net outcome) grouped))))
    ;; Calculate probabilities and sort
    (let ((results '()))
      (dolist (group grouped)
        (let* ((net (car group))
               (outcomes (cdr group))
               (count (length outcomes))
               (probability (/ (float count) total-numbers)))
          (push (list :net net
                      :probability probability
                      :count count
                      :numbers (mapcar (lambda (o) (plist-get o :number)) outcomes))
                results)))
      ;; Sort by net result (descending)
      (sort results (lambda (a b) (> (plist-get a :net) (plist-get b :net)))))))

(defun rc-render-interface ()
  "Render the main roulette interface."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "ROULETTE CALCULATOR\n" 'face '(:height 1.5 :weight bold)))
    (insert (propertize "════════════════════════════\n\n" 'face 'bold))

    ;; Game info
    (insert (format "Game Type: %s Roulette\n"
                    (capitalize (symbol-name rc-game-type))))
    (insert (format "Bankroll: $%.2f\n" rc-bankroll))
    
    ;; Show active betting strategy
    (when rc-strategy-mode
      (insert (format "Strategy: %s" (capitalize (symbol-name rc-strategy-mode))))
      (cond
       ((eq rc-strategy-mode 'martingale)
        (let ((base-bet (plist-get rc-strategy-state :base-bet))
              (multiplier (or (plist-get rc-strategy-state :current-multiplier) 1)))
          (if base-bet
              (insert (format " (Base: $%d, Multiplier: %dx)\n" base-bet multiplier))
            (insert (format " (Multiplier: %dx)\n" multiplier)))))
       ((eq rc-strategy-mode 'fibonacci)
        (insert (format " (Position: %d)\n"
                        (or (plist-get rc-strategy-state :position) 0))))
       ((eq rc-strategy-mode 'labouchere)
        (insert (format " (Sequence: %s)\n"
                        (plist-get rc-strategy-state :sequence))))
       (t (insert "\n")))
      ;; Show loss mode for non-even-money bets
      (unless (cl-some (lambda (bet) (member (rc-bet-type bet) 
                                             '(red black even odd low high)))
                       rc-current-bets)
        (insert (format "Loss mode: %s\n" 
                        (if rc-strategy-loss-on-partial-win 
                            "Strict (net loss = loss)"
                          "Lenient (no win = loss)")))))

    (insert "\n")

    ;; Current bets
    (insert (propertize "Current Bets:\n" 'face 'bold))
    (insert "─────────────\n")
    (if (null rc-current-bets)
        (insert "No bets placed\n")
      (let ((total-bet 0))
        (dolist (bet rc-current-bets)
          (setq total-bet (+ total-bet (rc-bet-amount bet)))
          (insert (format "• %s: $%.2f"
                          (capitalize (symbol-name (rc-bet-type bet)))
                          (rc-bet-amount bet)))
          (when (memq (rc-bet-type bet) '(voisins orphelins tiers))
            (insert (format " (%d chips @ $%.2f)"
                            (rc-get-special-bet-chips (rc-bet-type bet))
                            (/ (rc-bet-amount bet)
                               (rc-get-special-bet-chips (rc-bet-type bet))))))
          (when (rc-bet-numbers bet)
            (insert (format " on %s"
                            (if (> (length (rc-bet-numbers bet)) 5)
                                (format "%d numbers" (length (rc-bet-numbers bet)))
                              (mapconcat 'rc-format-number
                                         (rc-bet-numbers bet) ", ")))))
          (insert "\n"))
        (insert (format "\nTotal at risk: $%.2f\n" total-bet))

        ;; Show outcome probabilities
        (insert "\n")
        (insert (propertize "Possible Outcomes:\n" 'face 'bold))
        (insert "──────────────────\n")
        (let ((grouped-outcomes (rc-group-outcomes-by-net)))
          (dolist (outcome grouped-outcomes)
            (let ((net (plist-get outcome :net))
                  (prob (plist-get outcome :probability))
                  (count (plist-get outcome :count))
                  (numbers (plist-get outcome :numbers)))
              (insert (format "• Net: $%+.2f (%.1f%% - %d/%d) on: "
                              net
                              (* 100 prob)
                              count
                              (if (eq rc-game-type 'american) 38 37)))
              (let ((first t))
                (dolist (num (sort numbers '<))
                  (unless first (insert ", "))
                  (setq first nil)
                  (let ((num-str (rc-format-number num)))
                    (insert (apply 'propertize num-str
                                   (rc-number-display-properties num))))))
              (insert "\n")))
          (insert "\n")
          ;; Show expected value
          (let ((ev 0))
            (dolist (outcome grouped-outcomes)
              (setq ev (+ ev (* (plist-get outcome :net) (plist-get outcome :probability)))))
            (insert (format "Expected Value: $%.4f (House Edge: %.2f%%)\n"
                            ev
                            (* -100 (/ ev total-bet))))))))
    (insert "\n")

    ;; Last spin results
    (when rc-spin-history
      (insert (propertize "Last Spins:\n" 'face 'bold))
      (insert "────────────\n")
      (dolist (spin (seq-take rc-spin-history 10))
        (let ((num-str (rc-format-number spin)))
          (insert (apply 'propertize num-str
                         (rc-number-display-properties spin))
                  " ")))
      (insert "\n\n"))

    ;; Commands
    (insert (propertize "Commands:\n" 'face 'bold))
    (insert "─────────\n")
    (insert "s - Spin wheel          b - Place bet         c - Clear bets\n")
    (insert "p - Repeat last bet     t - Toggle game type  h - Show history\n")
    (insert "r - Reset bankroll      a - Analyze sequence  ? - Help\n")
    (insert "m - Martingale mode     f - Fibonacci mode    l - Labouchere mode\n")
    (insert "q - Quit\n")))

(defun rc-spin-wheel ()
  "Spin the roulette wheel and calculate results."
  (interactive)
  (if (null rc-current-bets)
      (message "No bets placed!")
    (let ((result (rc-spin))
          (total-win 0)
          (total-bet 0))
      
      ;; Calculate total bet amount
      (dolist (bet rc-current-bets)
        (setq total-bet (+ total-bet (rc-bet-amount bet))))
      
      ;; Check if player has enough bankroll for another spin with same bets
      (when (> total-bet rc-bankroll)
        (error "Insufficient bankroll for current bets! Clear some bets or add funds."))
      
      ;; Deduct bets from bankroll
      (setq rc-bankroll (- rc-bankroll total-bet))
      
      ;; Add spin to history
      (push result rc-spin-history)

      ;; Calculate winnings for each bet and track if even-money bet won
      (let ((even-money-bet-won nil)
            (has-even-money-bet nil))
        (dolist (bet rc-current-bets)
          (let ((bet-result (rc-calculate-bet-result bet result)))
            (setq total-win (+ total-win bet-result))
            ;; Check if this is an even-money bet
            (when (member (rc-bet-type bet) '(red black even odd low high))
              (setq has-even-money-bet t)
              ;; Check if it won
              (when (> bet-result 0)
                (setq even-money-bet-won t)))))

        ;; Update bankroll with winnings
        (setq rc-bankroll (+ rc-bankroll total-win total-bet))

        ;; Add to history (but keep current bets active)
        (push (list :spin result :bets rc-current-bets :net total-win) rc-bet-history)

        ;; Apply betting strategy if active and get strategy info
        (let ((strategy-msg nil))
          (when rc-strategy-mode
            ;; Determine if this is a win for strategy purposes
            (let ((won (if has-even-money-bet
                           even-money-bet-won
                         ;; For non-even-money bets, check based on custom variable
                         (if rc-strategy-loss-on-partial-win
                             (> total-win 0)  ; Net profit required
                           (> (+ total-bet total-win) 0)))))  ; Any winnings count
              (setq strategy-msg (rc-apply-betting-strategy won))))

          ;; Show result with strategy info in one message
          (rc-render-interface)
          (let ((result-str (rc-format-number result))
                (color (rc-number-color result)))
            (if strategy-msg
                (message "Spin: %s (%s) - Bet: $%.2f, Win: $%.2f, Net: $%.2f | %s"
                         result-str
                         (capitalize (symbol-name color))
                         total-bet
                         (+ total-bet total-win)
                         total-win
                         strategy-msg)
              (message "Spin: %s (%s) - Bet: $%.2f, Win: $%.2f, Net: $%.2f"
                       result-str
                       (capitalize (symbol-name color))
                       total-bet
                       (+ total-bet total-win)
                       total-win))))))))

(defun rc-place-bet ()
  "Place a bet interactively."
  (interactive)
  (let* ((bet-types '(("Straight" . straight)
                      ("Split" . split)
                      ("Street" . street)
                      ("Corner" . corner)
                      ("Line" . line)
                      ("Dozen" . dozen)
                      ("Column" . column)
                      ("Red" . red)
                      ("Black" . black)
                      ("Even" . even)
                      ("Odd" . odd)
                      ("Low (1-18)" . low)
                      ("High (19-36)" . high)
                      ("Voisins du Zero (9 chips)" . voisins)
                      ("Orphelins (5 chips)" . orphelins)
                      ("Tiers du Cylindre (6 chips)" . tiers)))
         (bet-type-name (completing-read "Bet type: "
                                         (mapcar 'car bet-types) nil t))
         (bet-type (cdr (assoc bet-type-name bet-types)))
         amount numbers)

    ;; Handle special bets differently
    (cond
     ((memq bet-type '(voisins orphelins tiers))
      (let* ((chips-needed (rc-get-special-bet-chips bet-type))
             (chip-value (read-number (format "Chip value (need %d chips): $" chips-needed) 10)))
        (setq amount (* chip-value chips-needed))
        (setq numbers (cond
                       ((eq bet-type 'voisins) (append rc-voisins-numbers nil))
                       ((eq bet-type 'orphelins) (append rc-orphelins-numbers nil))
                       ((eq bet-type 'tiers) (append rc-tiers-numbers nil))))))
     (t
      (setq amount (read-number "Bet amount: $" 10))
      (setq numbers (rc-get-bet-numbers bet-type))))

    ;; Check if total bets would exceed bankroll
    (let ((total-bets amount))
      (dolist (bet rc-current-bets)
        (setq total-bets (+ total-bets (rc-bet-amount bet))))
      (when (> total-bets rc-bankroll)
        (error "Insufficient bankroll for total bets")))

    (push (make-roulette-calculator-bet :type bet-type
                                        :numbers numbers
                                        :amount amount
                                        :payout (rc-get-payout bet-type))
          rc-current-bets)

    (rc-render-interface)))

(defun rc-read-roulette-number (prompt)
  "Read a roulette number from user input with PROMPT.
Handles 00 for American roulette."
  (let ((input (read-string prompt)))
    (cond
     ((string= input "00")
      (if (eq rc-game-type 'american)
          37  ; Internal representation of 00
        (error "00 is only valid in American roulette")))
     ((string-match-p "^[0-9]+$" input)
      (let ((num (string-to-number input)))
        (if (and (>= num 0) (<= num 36))
            num
          (error "Number must be between 0 and 36"))))
     (t (error "Invalid input. Enter a number 0-36%s"
               (if (eq rc-game-type 'american) " or 00" ""))))))

(defun rc-get-bet-numbers (bet-type)
  "Get numbers for BET-TYPE interactively if needed."
  (cond
   ((eq bet-type 'straight)
    (list (rc-read-roulette-number 
           (format "Number (0-36%s): " 
                   (if (eq rc-game-type 'american) " or 00" "")))))
   ((eq bet-type 'split)
    (list (rc-read-roulette-number "First number: ")
          (rc-read-roulette-number "Second number: ")))
   ((eq bet-type 'street)
    (let ((first (read-number "First number of street: ")))
      (list first (1+ first) (+ first 2))))
   ((eq bet-type 'corner)
    (let ((nums '()))
      (dotimes (_ 4)
        (push (read-number (format "Number %d: " (1+ (length nums)))) nums))
      (nreverse nums)))
   ((eq bet-type 'line)
    (let ((first (read-number "First number of line: ")))
      (list first (1+ first) (+ first 2)
            (+ first 3) (+ first 4) (+ first 5))))
   ((eq bet-type 'dozen)
    (list (read-number "Dozen (1, 2, or 3): ")))
   ((eq bet-type 'column)
    (list (read-number "Column (1, 2, or 3): ")))
   (t nil)))

(defun rc-clear-bets ()
  "Clear all current bets."
  (interactive)
  (when rc-current-bets
    (setq rc-last-cleared-bets rc-current-bets))
  (setq rc-current-bets '())
  (rc-render-interface)
  (message "All bets cleared"))

(defun rc-toggle-game-type ()
  "Toggle between European and American roulette."
  (interactive)
  (setq rc-game-type
        (if (eq rc-game-type 'european) 'american 'european))
  (rc-render-interface)
  (message "Switched to %s roulette" (capitalize (symbol-name rc-game-type))))

(defun rc-reset-bankroll ()
  "Reset bankroll to initial amount."
  (interactive)
  (let ((amount (read-number "New bankroll amount: $" 1000)))
    (setq rc-bankroll amount)
    (rc-render-interface)))

(defun rc-repeat-last-bet ()
  "Repeat the last cleared bet if there is one."
  (interactive)
  (if (null rc-last-cleared-bets)
      (message "No previous bets to repeat!")
    ;; Check if we have enough bankroll for all the bets
    (let ((total-amount 0))
      (dolist (bet rc-last-cleared-bets)
        (setq total-amount (+ total-amount (rc-bet-amount bet))))
      (dolist (bet rc-current-bets)
        (setq total-amount (+ total-amount (rc-bet-amount bet))))
      (if (> total-amount rc-bankroll)
          (message "Insufficient bankroll to repeat last bets!")
        ;; Add all the last cleared bets to current bets
        (dolist (bet rc-last-cleared-bets)
          (push (copy-roulette-calculator-bet bet) rc-current-bets))
        (rc-render-interface)
        (message "Repeated %d bet(s) totaling $%.2f" 
                 (length rc-last-cleared-bets)
                 (apply '+ (mapcar 'rc-bet-amount rc-last-cleared-bets)))))))

(defun rc-toggle-strategy-loss-mode ()
  "Toggle how betting strategies treat partial wins."
  (interactive)
  (setq rc-strategy-loss-on-partial-win (not rc-strategy-loss-on-partial-win))
  (message "Strategy loss mode: %s"
           (if rc-strategy-loss-on-partial-win
               "Net loss = loss (strict)"
             "No winnings = loss (lenient)"))
  (rc-render-interface))


(defun rc-show-help ()
  "Show help for roulette calculator."
  (interactive)
  (with-help-window "*Roulette Help*"
    (princ "ROULETTE CALCULATOR HELP\n")
    (princ "========================\n\n")
    (princ "Bet Types:\n")
    (princ "----------\n")
    (princ "• Straight: Single number (35:1)\n")
    (princ "  - European: 0-36\n")
    (princ "  - American: 0-36 and 00\n")
    (princ "• Split: Two adjacent numbers (17:1)\n")
    (princ "• Street: Three numbers in a row (11:1)\n")
    (princ "• Corner: Four numbers (8:1)\n")
    (princ "• Line: Six numbers in two rows (5:1)\n")
    (princ "• Dozen: 1-12, 13-24, or 25-36 (2:1)\n")
    (princ "• Column: Vertical column (2:1)\n")
    (princ "• Red/Black, Even/Odd, Low/High (1:1)\n\n")
    (princ "Special Bets:\n")
    (princ "-------------\n")
    (princ "• Voisins du Zero: 17 numbers near zero\n")
    (princ "• Orphelins: 8 numbers not in Voisins or Tiers\n")
    (princ "• Tiers du Cylindre: 12 numbers opposite zero\n\n")
    (princ "Betting Strategies:\n")
    (princ "------------------\n")
    (princ "• Martingale: Double bet after loss\n")
    (princ "• Fibonacci: Follow Fibonacci sequence\n")
    (princ "• Labouchere: Cross off numbers system\n")))

(defvar rc-strategy-mode nil
  "Current betting strategy mode.")

(defvar rc-strategy-state nil
  "State for current betting strategy.")

(defcustom rc-strategy-loss-on-partial-win nil
  "If non-nil, betting strategies consider partial wins as losses.
When t, if you win something but have a net loss, it's treated as a loss.
When nil, only complete losses (winning nothing) are treated as losses."
  :type 'boolean
  :group 'roulette-calculator)

(defun rc-normal-cdf (z)
  "Approximate normal cumulative distribution function for Z-score."
  ;; Using Abramowitz and Stegun approximation
  (let* ((abs-z (abs z))
         (t-val (/ 1.0 (+ 1.0 (* 0.2316419 abs-z))))
         (poly (* t-val 
                  (+ 0.319381530
                     (* t-val (+ -0.356563782
                                 (* t-val (+ 1.781477937
                                             (* t-val (+ -1.821255978
                                                         (* t-val 1.330274429))))))))))
         (cdf (- 1.0 (* poly (exp (- (* 0.5 abs-z abs-z))) 0.3989422804))))
    (if (< z 0) (- 1.0 cdf) cdf)))

(defun rc-parse-number-sequence (input)
  "Parse INPUT string into a list of numbers, splitting by non-digit characters."
  (let ((numbers '())
        (current-num "")
        (i 0)
        (prev-char nil))
    (while (< i (length input))
      (let ((char (aref input i)))
        (if (and (>= char ?0) (<= char ?9))
            (progn
              (setq current-num (concat current-num (char-to-string char)))
              ;; Check for "00" pattern
              (when (and (= char ?0) 
                         prev-char  ; Check that prev-char is not nil
                         (= prev-char ?0)
                         (= (length current-num) 2)
                         (string= current-num "00"))
                (setq numbers (cons 37 numbers))  ; 00 is represented as 37
                (setq current-num "")))
          (when (and (> (length current-num) 0)
                     (not (string= current-num "00")))  ; Don't add "00" as 0
            (push (string-to-number current-num) numbers)
            (setq current-num "")))
        (setq prev-char char))
      (setq i (1+ i)))
    (when (and (> (length current-num) 0)
               (not (string= current-num "00")))
      (push (string-to-number current-num) numbers))
    (nreverse numbers)))

(defun rc-analyze-sequence ()
  "Analyze current bets against a sequence of numbers."
  (interactive)
  (if (null rc-current-bets)
      (message "No bets placed! Place some bets first, then analyze.")
    (let* ((input (read-string "Enter number sequence (separated by any non-digit): "))
           (numbers (rc-parse-number-sequence input))
           (valid-numbers '()))
      
      ;; Validate numbers for the game type
      (dolist (num numbers)
        (cond
         ;; Valid single zero and regular numbers (0-36) for both game types
         ((and (>= num 0) (<= num 36))
          (push num valid-numbers))
         ;; Double zero (represented as 37) only valid for American roulette
         ((and (= num 37) (eq rc-game-type 'american))
          (push num valid-numbers))
         ;; Any other number is invalid
         (t
          (let ((display-num (if (= num 37) "00" (number-to-string num))))
            (message "Warning: Ignoring invalid number %s (not on %s roulette wheel)" 
                     display-num
                     (if (eq rc-game-type 'american) "American" "European"))))))
      
      (setq valid-numbers (nreverse valid-numbers))
      
      (if (null valid-numbers)
          (message "No valid numbers in sequence!")
        (rc-show-sequence-analysis valid-numbers)))))

(defun rc-show-sequence-analysis (numbers)
  "Show analysis of current bets against NUMBERS sequence."
  (with-current-buffer (get-buffer-create "*Roulette Sequence Analysis*")
    (erase-buffer)
    (insert "ROULETTE SEQUENCE ANALYSIS\n")
    (insert "==========================\n\n")
    
    ;; Show current bets
    (insert (propertize "Current Bets:\n" 'face 'bold))
    (insert "─────────────\n")
    (let ((total-bet 0))
      (dolist (bet rc-current-bets)
        (setq total-bet (+ total-bet (rc-bet-amount bet)))
        (insert (format "• %s: $%.2f"
                        (capitalize (symbol-name (rc-bet-type bet)))
                        (rc-bet-amount bet)))
        (when (memq (rc-bet-type bet) '(voisins orphelins tiers))
          (insert (format " (%d chips @ $%.2f)"
                          (rc-get-special-bet-chips (rc-bet-type bet))
                          (/ (rc-bet-amount bet)
                             (rc-get-special-bet-chips (rc-bet-type bet))))))
        (insert "\n"))
      (insert (format "\nTotal bet per spin: $%.2f\n\n" total-bet)))
    
    ;; Show sequence
    (insert (propertize "Number Sequence:\n" 'face 'bold))
    (insert "────────────────\n")
    (let ((first t))
      (dolist (num numbers)
        (unless first (insert ", "))
        (setq first nil)
        (let ((num-str (rc-format-number num)))
          (insert (apply 'propertize num-str
                         (rc-number-display-properties num))))))
    (insert (format "\n(%d numbers)\n\n" (length numbers)))
    
    ;; Calculate results
    (insert (propertize "Results:\n" 'face 'bold))
    (insert "────────\n")
    (let ((running-total 0)
          (wins 0)
          (losses 0)
          (spin-results '()))
      
      (dolist (num numbers)
        (let ((spin-net 0)
              (total-bet 0))
          (dolist (bet rc-current-bets)
            (setq total-bet (+ total-bet (rc-bet-amount bet)))
            (setq spin-net (+ spin-net (rc-calculate-bet-result bet num))))
          
          (setq running-total (+ running-total spin-net))
          (if (>= spin-net 0)
              (setq wins (1+ wins))
            (setq losses (1+ losses)))
          
          (push (list :number num :net spin-net :total running-total) spin-results)))
      
      (setq spin-results (nreverse spin-results))
      
      ;; Show spin-by-spin results
      (insert "Spin-by-spin results:\n")
      (let ((spin-num 1))
        (dolist (result spin-results)
          (let ((num (plist-get result :number))
                (net (plist-get result :net))
                (total (plist-get result :total)))
            (insert (format "%3d. " spin-num))
            (insert (apply 'propertize (rc-format-number num)
                           (rc-number-display-properties num)))
            (insert (format " → Net: $%+.2f, Total: $%+.2f\n" net total))
            (setq spin-num (1+ spin-num)))))
      
      (insert "\n")
      (insert (propertize "Summary:\n" 'face 'bold))
      (insert "────────\n")
      (insert (format "Total spins: %d\n" (length numbers)))
      (insert (format "Wins: %d (%.1f%%)\n" wins (* 100.0 (/ wins (float (length numbers))))))
      (insert (format "Losses: %d (%.1f%%)\n" losses (* 100.0 (/ losses (float (length numbers))))))
      (insert (format "Final P/L: $%+.2f\n" running-total))
      (insert (format "Average per spin: $%+.2f\n" (/ running-total (float (length numbers)))))
      
      ;; Calculate theoretical house edge and expected loss
      (let* ((total-bet-per-spin 0)
             (expected-value 0)
             (total-wagered 0))
        (dolist (bet rc-current-bets)
          (setq total-bet-per-spin (+ total-bet-per-spin (rc-bet-amount bet))))
        
        (setq total-wagered (* total-bet-per-spin (length numbers)))
        
        ;; Calculate expected value using the grouped outcomes
        (let ((grouped-outcomes (rc-group-outcomes-by-net)))
          (dolist (outcome grouped-outcomes)
            (setq expected-value (+ expected-value 
                                    (* (plist-get outcome :net) 
                                       (plist-get outcome :probability))))))
        
        (let* ((theoretical-house-edge (* -100 (/ expected-value total-bet-per-spin)))
               (observed-house-edge (if (zerop total-wagered) 0
                                      (* -100.0 (/ (float running-total) total-wagered))))
               (theoretical-loss (* (length numbers) expected-value))
               (actual-vs-expected (- running-total theoretical-loss)))
          
          (insert "\n")
          (insert (propertize "Statistical Analysis:\n" 'face 'bold))
          (insert "────────────────────\n")
          (insert (format "Observed house edge: %.2f%% (from this sequence)\n" observed-house-edge))
          (insert (format "Theoretical house edge: %.2f%% (mathematical expectation)\n" theoretical-house-edge))
          (insert (format "Total wagered: $%.2f\n" total-wagered))
          (insert (format "Expected loss: $%.2f\n" theoretical-loss))
          (insert (format "Actual result: $%+.2f\n" running-total))
          (insert (format "Variance from expected: $%+.2f\n" actual-vs-expected))
          
          ;; Calculate standard deviation and z-score
          ;; For each spin, calculate variance of outcomes
          (let* ((outcome-values '())
                 (outcome-probs '())
                 (variance 0))
            
            ;; Get all possible outcomes and their probabilities
            (let ((grouped (rc-group-outcomes-by-net)))
              (dolist (outcome grouped)
                (push (plist-get outcome :net) outcome-values)
                (push (plist-get outcome :probability) outcome-probs)))
            
            ;; Calculate variance for single spin
            (let ((i 0))
              (while (< i (length outcome-values))
                (let ((value (nth i outcome-values))
                      (prob (nth i outcome-probs)))
                  (setq variance (+ variance (* prob (expt (- value expected-value) 2)))))
                (setq i (1+ i))))
            
            ;; Standard deviation for n spins
            (let* ((std-dev-single (sqrt variance))
                   (std-dev-total (* std-dev-single (sqrt (length numbers))))
                   (z-score (if (zerop std-dev-total) 0 
                              (/ (- running-total theoretical-loss) std-dev-total)))
                   ;; Calculate probability using normal approximation
                   ;; For two-tailed test
                   (p-value (if (< z-score 0)
                                (* 2 (rc-normal-cdf z-score))
                              (* 2 (- 1 (rc-normal-cdf z-score))))))
              
              (insert "\n")
              (insert (propertize "Probability Analysis:\n" 'face 'bold))
              (insert "────────────────────\n")
              (insert (format "Standard deviation (single spin): $%.2f\n" std-dev-single))
              (insert (format "Standard deviation (%d spins): $%.2f\n" (length numbers) std-dev-total))
              (insert (format "Z-score: %.3f\n" z-score))
              (insert (format "P-value: %.4f\n" p-value))
              (insert (format "Probability of result this extreme: %.2f%%\n" (* 100 p-value)))
              
              (cond
               ((< p-value 0.01)
                (insert "\nThis result is HIGHLY UNUSUAL (p < 0.01)"))
               ((< p-value 0.05)
                (insert "\nThis result is statistically significant (p < 0.05)"))
               ((< p-value 0.10)
                (insert "\nThis result is somewhat unusual (p < 0.10)"))
               (t
                (insert "\nThis result is within normal variance")))))))

      (insert "\n")
      ;; Show best and worst streaks
      (let ((current-streak 0)
            (best-streak 0)
            (worst-streak 0)
            (current-type nil))
        (dolist (result spin-results)
          (let ((net (plist-get result :net)))
            (cond
             ((>= net 0)
              (if (eq current-type 'win)
                  (setq current-streak (1+ current-streak))
                (setq current-streak 1
                      current-type 'win))
              (setq best-streak (max best-streak current-streak)))
             (t
              (if (eq current-type 'loss)
                  (setq current-streak (1+ current-streak))
                (setq current-streak 1
                      current-type 'loss))
              (setq worst-streak (max worst-streak current-streak))))))

        (insert (format "Best winning streak: %d\n" best-streak))
        (insert (format "Worst losing streak: %d\n" worst-streak)))))

  (display-buffer (current-buffer)))

(defun rc-martingale-mode ()
  "Enable Martingale betting strategy."
  (interactive)
  (if (eq rc-strategy-mode 'martingale)
      (progn
        (setq rc-strategy-mode nil)
        (message "Martingale strategy disabled"))
    (setq rc-strategy-mode 'martingale)
    ;; Check if we have even-money bets
    (let ((has-even-money nil))
      (dolist (bet rc-current-bets)
        (when (member (rc-bet-type bet) '(red black even odd low high))
          (setq has-even-money t)))
      
      (if has-even-money
          ;; Traditional base bet for even-money bets
          (let ((base-bet (read-number "Base bet amount: $" 10)))
            (setq rc-strategy-state (list :base-bet base-bet 
                                          :current-multiplier 1
                                          :last-won nil))
            (message "Martingale strategy enabled (base bet: $%d)" base-bet))
        ;; No even-money bets - use current bets as base
        (setq rc-strategy-state (list :base-bet nil
                                      :current-multiplier 1
                                      :last-won nil))
        (message "Martingale strategy enabled (will scale current bets)")))))

(defun rc-fibonacci-mode ()
  "Enable Fibonacci betting strategy."
  (interactive)
  (if (eq rc-strategy-mode 'fibonacci)
      (progn
        (setq rc-strategy-mode nil)
        (message "Fibonacci strategy disabled"))
    (setq rc-strategy-mode 'fibonacci)
    (setq rc-strategy-state '(:position 0 :last-won nil))
    (message "Fibonacci strategy enabled")))

(defun rc-labouchere-mode ()
  "Enable Labouchere betting strategy."
  (interactive)
  (if (eq rc-strategy-mode 'labouchere)
      (progn
        (setq rc-strategy-mode nil)
        (message "Labouchere strategy disabled"))
    (setq rc-strategy-mode 'labouchere)
    (let ((sequence (read-string "Enter number sequence (e.g., 1 2 3 4): ")))
      (setq rc-strategy-state
            (list :sequence (mapcar 'string-to-number (split-string sequence))
                  :original-sequence (mapcar 'string-to-number (split-string sequence))
                  :last-won nil)))
    (message "Labouchere strategy enabled")))

(defun rc-show-history ()
  "Show detailed betting history."
  (interactive)
  (with-current-buffer (get-buffer-create "*Roulette History*")
    (erase-buffer)
    (insert "ROULETTE HISTORY\n")
    (insert "================\n\n")

    (if (null rc-spin-history)
        (insert "No spins yet\n")
      (let ((spin-count 0))
        (dolist (spin (reverse rc-spin-history))
          (insert (format "Spin #%d: " (1+ spin-count)))
          (insert (apply 'propertize (rc-format-number spin)
                         (rc-number-display-properties spin)))
          (insert "\n")
          (setq spin-count (1+ spin-count)))))

    (insert "\n\nStatistics:\n")
    (insert "-----------\n")
    (when rc-spin-history
      (let ((reds 0) (blacks 0) (greens 0)
            (evens 0) (odds 0) (lows 0) (highs 0))
        (dolist (num rc-spin-history)
          (let ((color (rc-number-color num)))
            (cond ((eq color 'red) (setq reds (1+ reds)))
                  ((eq color 'black) (setq blacks (1+ blacks)))
                  ((eq color 'green) (setq greens (1+ greens))))
            (when (rc-is-even num) (setq evens (1+ evens)))
            (when (rc-is-odd num) (setq odds (1+ odds)))
            (when (rc-is-low num) (setq lows (1+ lows)))
            (when (rc-is-high num) (setq highs (1+ highs)))))

        (let ((total (float (length rc-spin-history))))
          (insert (format "Red: %d (%.1f%%)\n" reds (* 100.0 (/ reds total))))
          (insert (format "Black: %d (%.1f%%)\n" blacks (* 100.0 (/ blacks total))))
          (insert (format "Green: %d (%.1f%%)\n" greens (* 100.0 (/ greens total))))
          (insert (format "Even: %d (%.1f%%)\n" evens (* 100.0 (/ evens total))))
          (insert (format "Odd: %d (%.1f%%)\n" odds (* 100.0 (/ odds total))))
          (insert (format "Low: %d (%.1f%%)\n" lows (* 100.0 (/ lows total))))
          (insert (format "High: %d (%.1f%%)\n" highs (* 100.0 (/ highs total)))))))

    (display-buffer (current-buffer))))

(provide 'roulette-calculator)

;; Local Variables:
;; read-symbol-shorthands: (("rc-" . "roulette-calculator-"))
;; End:

;;; roulette-calculator.el ends here
