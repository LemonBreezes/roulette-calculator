;;; roulette-calculator.el --- Comprehensive roulette betting calculator -*- lexical-binding: t -*-
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

(require 'cl-lib)

;; Helper functions
(defun roulette-get-special-bet-chips (bet-type)
  "Get number of chips required for special BET-TYPE."
  (cond
   ((eq bet-type 'voisins) 9)
   ((eq bet-type 'orphelins) 5)
   ((eq bet-type 'tiers) 6)
   (t 1)))

;; Data structures
(defconst roulette-european-numbers
  [0 32 15 19 4 21 2 25 17 34 6 27 13 36 11 30 8 23 10 5 24 16 33 1 20 14 31 9 22 18 29 7 28 12 35 3 26]
  "European roulette wheel number sequence.")

(defconst roulette-american-numbers
  [0 28 9 26 30 11 7 20 32 17 5 22 34 15 3 24 36 13 1 00 27 10 25 29 12 8 19 31 18 6 21 33 16 4 23 35 14 2]
  "American roulette wheel number sequence (00 represented as 37).")

(defconst roulette-red-numbers
  [1 3 5 7 9 12 14 16 18 19 21 23 25 27 30 32 34 36]
  "Red numbers on roulette wheel.")

(defconst roulette-black-numbers
  [2 4 6 8 10 11 13 15 17 20 22 24 26 28 29 31 33 35]
  "Black numbers on roulette wheel.")

;; Special bet definitions
(defconst roulette-voisins-numbers
  [22 18 29 7 28 12 35 3 26 0 32 15 19 4 21 2 25]
  "Numbers covered by Voisins du Zero bet.")

(defconst roulette-orphelins-numbers
  [1 20 14 31 9 17 34 6]
  "Numbers covered by Orphelins bet.")

(defconst roulette-tiers-numbers
  [27 13 36 11 30 8 23 10 5 24 16 33]
  "Numbers covered by Tiers du Cylindre bet.")

(defvar roulette-game-type 'european
  "Current game type: 'european or 'american.")

(defvar roulette-bankroll 1000.0
  "Current bankroll amount.")

(defvar roulette-bet-history '()
  "History of bets placed.")

(defvar roulette-current-bets '()
  "Current bets for this spin.")

(defvar roulette-spin-history '()
  "History of spin results.")

;; Betting structure
(cl-defstruct roulette-bet
  type      ; Symbol: 'straight, 'split, 'street, 'corner, 'line, 'dozen, 'column, 'red, 'black, 'even, 'odd, 'low, 'high, 'voisins, 'orphelins, 'tiers
  numbers   ; List of numbers covered
  amount    ; Bet amount
  payout)   ; Payout ratio

;; Core functions
(defun roulette-get-payout (bet-type)
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

(defun roulette-number-color (number)
  "Return color of NUMBER ('red, 'black, or 'green)."
  (cond
   ((= number 0) 'green)
   ((and (eq roulette-game-type 'american) (= number 37)) 'green) ; 00
   ((memq number (append roulette-red-numbers nil)) 'red)
   ((memq number (append roulette-black-numbers nil)) 'black)
   (t nil)))

(defun roulette-is-even (number)
  "Check if NUMBER is even (excluding 0 and 00)."
  (and (> number 0) (< number 37) (= (mod number 2) 0)))

(defun roulette-is-odd (number)
  "Check if NUMBER is odd."
  (and (> number 0) (< number 37) (= (mod number 2) 1)))

(defun roulette-is-low (number)
  "Check if NUMBER is in low range (1-18)."
  (and (>= number 1) (<= number 18)))

(defun roulette-is-high (number)
  "Check if NUMBER is in high range (19-36)."
  (and (>= number 19) (<= number 36)))

(defun roulette-get-dozen (number)
  "Get dozen for NUMBER (1, 2, 3, or nil)."
  (cond
   ((and (>= number 1) (<= number 12)) 1)
   ((and (>= number 13) (<= number 24)) 2)
   ((and (>= number 25) (<= number 36)) 3)
   (t nil)))

(defun roulette-get-column (number)
  "Get column for NUMBER (1, 2, 3, or nil)."
  (cond
   ((= number 0) nil)
   ((= number 37) nil) ; 00
   ((= (mod number 3) 1) 1)
   ((= (mod number 3) 2) 2)
   ((= (mod number 3) 0) 3)
   (t nil)))

(defun roulette-calculate-bet-result (bet winning-number)
  "Calculate result of BET given WINNING-NUMBER."
  (let ((win-amount 0))
    (cond
     ;; Straight up bet
     ((eq (roulette-bet-type bet) 'straight)
      (when (memq winning-number (roulette-bet-numbers bet))
        (setq win-amount (* (roulette-bet-amount bet) (1+ (roulette-bet-payout bet))))))

     ;; Split, street, corner, line bets
     ((memq (roulette-bet-type bet) '(split street corner line))
      (when (memq winning-number (roulette-bet-numbers bet))
        (setq win-amount (* (roulette-bet-amount bet) (1+ (roulette-bet-payout bet))))))

     ;; Dozen bet
     ((eq (roulette-bet-type bet) 'dozen)
      (when (eq (roulette-get-dozen winning-number) (car (roulette-bet-numbers bet)))
        (setq win-amount (* (roulette-bet-amount bet) (1+ (roulette-bet-payout bet))))))

     ;; Column bet
     ((eq (roulette-bet-type bet) 'column)
      (when (eq (roulette-get-column winning-number) (car (roulette-bet-numbers bet)))
        (setq win-amount (* (roulette-bet-amount bet) (1+ (roulette-bet-payout bet))))))

     ;; Color bets
     ((eq (roulette-bet-type bet) 'red)
      (when (eq (roulette-number-color winning-number) 'red)
        (setq win-amount (* (roulette-bet-amount bet) 2))))

     ((eq (roulette-bet-type bet) 'black)
      (when (eq (roulette-number-color winning-number) 'black)
        (setq win-amount (* (roulette-bet-amount bet) 2))))

     ;; Even/odd bets
     ((eq (roulette-bet-type bet) 'even)
      (when (roulette-is-even winning-number)
        (setq win-amount (* (roulette-bet-amount bet) 2))))

     ((eq (roulette-bet-type bet) 'odd)
      (when (roulette-is-odd winning-number)
        (setq win-amount (* (roulette-bet-amount bet) 2))))

     ;; Low/high bets
     ((eq (roulette-bet-type bet) 'low)
      (when (roulette-is-low winning-number)
        (setq win-amount (* (roulette-bet-amount bet) 2))))

     ((eq (roulette-bet-type bet) 'high)
      (when (roulette-is-high winning-number)
        (setq win-amount (* (roulette-bet-amount bet) 2))))

     ;; Special bets
     ((memq (roulette-bet-type bet) '(voisins orphelins tiers))
      (when (memq winning-number (roulette-bet-numbers bet))
        ;; These are complex bets with multiple sub-bets
        ;; For simplicity, we'll calculate based on the specific number hit
        (setq win-amount (roulette-calculate-special-bet-payout bet winning-number)))))

    (- win-amount (roulette-bet-amount bet))))

(defun roulette-calculate-special-bet-payout (bet winning-number)
  "Calculate payout for special BET types given WINNING-NUMBER."
  (let ((chip-value (/ (roulette-bet-amount bet)
                       (roulette-get-special-bet-chips (roulette-bet-type bet)))))
    (cond
     ((eq (roulette-bet-type bet) 'voisins)
      ;; Voisins uses 9 chips: 2 on 0-2-3, 1 on splits 4-7, 12-15, 18-21, 19-22, 32-35, 2 on corner 25-29
      (cond
       ((memq winning-number '(0 2 3)) (* chip-value 2 12))  ; 2 chips on trio at 11:1
       ((memq winning-number '(25 26 28 29)) (* chip-value 2 9)) ; 2 chips on corner at 8:1
       ((memq winning-number '(4 7 12 15 18 21 19 22 32 35)) (* chip-value 18)) ; 1 chip on splits at 17:1
       (t 0)))

     ((eq (roulette-bet-type bet) 'orphelins)
      ;; Orphelins uses 5 chips: 1 straight on 1, 1 on splits 6-9, 14-17, 17-20, 31-34
      (cond
       ((= winning-number 1) (* chip-value 36))  ; Straight up at 35:1
       ((= winning-number 17) (* chip-value 36)) ; Covered by two splits, wins twice
       ((memq winning-number '(6 9 14 20 31 34)) (* chip-value 18)) ; Splits at 17:1
       (t 0)))

     ((eq (roulette-bet-type bet) 'tiers)
      ;; Tiers uses 6 chips on splits: 5-8, 10-11, 13-16, 23-24, 27-30, 33-36
      (if (memq winning-number (roulette-bet-numbers bet))
          (* chip-value 18)  ; All are split bets at 17:1
        0))

     (t 0))))

(defun roulette-spin ()
  "Simulate a roulette spin and return winning number."
  (let ((max-num (if (eq roulette-game-type 'american) 37 36)))
    (random (1+ max-num))))

(defun roulette-format-number (number)
  "Format NUMBER for display."
  (cond
   ((= number 37) "00")
   ((= number 0) "0")
   (t (number-to-string number))))

(defun roulette-number-display-properties (number)
  "Get display properties for NUMBER."
  (let ((color (roulette-number-color number)))
    (cond
     ((eq color 'red) '(face (:foreground "red" :weight bold)))
     ((eq color 'black) '(face (:foreground "black" :weight bold)))
     ((eq color 'green) '(face (:foreground "green" :weight bold)))
     (t nil))))

;; Betting strategies
(defun roulette-martingale-next-bet (last-bet won-p)
  "Calculate next bet amount using Martingale strategy."
  (if won-p
      1  ; Reset to base bet
    (* last-bet 2)))  ; Double after loss

(defun roulette-fibonacci-sequence (n)
  "Generate first N numbers of Fibonacci sequence."
  (let ((seq '(1 1)))
    (dotimes (_ (- n 2))
      (setq seq (append seq (list (+ (nth (- (length seq) 1) seq)
                                     (nth (- (length seq) 2) seq))))))
    seq))

(defun roulette-fibonacci-next-bet (position won-p)
  "Calculate next bet using Fibonacci strategy at POSITION."
  (let ((fib-seq (roulette-fibonacci-sequence 20)))
    (if won-p
        (nth (max 0 (- position 2)) fib-seq)
      (nth (min (1+ position) (1- (length fib-seq))) fib-seq))))

(defun roulette-labouchere-next-bet (sequence won-p last-bet)
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
(defun roulette-calculator ()
  "Start the roulette calculator."
  (interactive)
  (switch-to-buffer "*Roulette Calculator*")
  (roulette-mode)
  (roulette-render-interface))

(defvar roulette-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'roulette-spin-wheel)
    (define-key map "b" 'roulette-place-bet)
    (define-key map "c" 'roulette-clear-bets)
    (define-key map "t" 'roulette-toggle-game-type)
    (define-key map "h" 'roulette-show-history)
    (define-key map "?" 'roulette-show-help)
    (define-key map "q" 'quit-window)
    (define-key map "r" 'roulette-reset-bankroll)
    (define-key map "v" 'roulette-place-voisins)
    (define-key map "o" 'roulette-place-orphelins)
    (define-key map "i" 'roulette-place-tiers)
    (define-key map "m" 'roulette-martingale-mode)
    (define-key map "f" 'roulette-fibonacci-mode)
    (define-key map "l" 'roulette-labouchere-mode)
    map)
  "Keymap for roulette calculator mode.")

(define-derived-mode roulette-mode special-mode "Roulette"
  "Major mode for roulette calculator.
\\{roulette-mode-map}"
  (setq buffer-read-only t))

(defun roulette-render-interface ()
  "Render the main roulette interface."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "ROULETTE CALCULATOR\n" 'face '(:height 1.5 :weight bold)))
    (insert (propertize "═══════════════════\n\n" 'face 'bold))

    ;; Game info
    (insert (format "Game Type: %s Roulette\n"
                    (capitalize (symbol-name roulette-game-type))))
    (insert (format "Bankroll: $%.2f\n\n" roulette-bankroll))

    ;; Current bets
    (insert (propertize "Current Bets:\n" 'face 'bold))
    (insert "─────────────\n")
    (if (null roulette-current-bets)
        (insert "No bets placed\n")
      (let ((total-bet 0))
        (dolist (bet roulette-current-bets)
          (setq total-bet (+ total-bet (roulette-bet-amount bet)))
          (insert (format "• %s: $%.2f"
                          (capitalize (symbol-name (roulette-bet-type bet)))
                          (roulette-bet-amount bet)))
          (when (memq (roulette-bet-type bet) '(voisins orphelins tiers))
            (insert (format " (%d chips @ $%.2f)"
                            (roulette-get-special-bet-chips (roulette-bet-type bet))
                            (/ (roulette-bet-amount bet)
                               (roulette-get-special-bet-chips (roulette-bet-type bet))))))
          (when (roulette-bet-numbers bet)
            (insert (format " on %s"
                            (if (> (length (roulette-bet-numbers bet)) 5)
                                (format "%d numbers" (length (roulette-bet-numbers bet)))
                              (mapconcat 'roulette-format-number
                                         (roulette-bet-numbers bet) ", ")))))
          (insert "\n"))
        (insert (format "\nTotal at risk: $%.2f\n" total-bet))))
    (insert "\n")

    ;; Last spin results
    (when roulette-spin-history
      (insert (propertize "Last Spins:\n" 'face 'bold))
      (insert "────────────\n")
      (dolist (spin (seq-take roulette-spin-history 10))
        (let ((num-str (roulette-format-number spin)))
          (insert (apply 'propertize num-str
                         (roulette-number-display-properties spin))
                  " ")))
      (insert "\n\n"))

    ;; Commands
    (insert (propertize "Commands:\n" 'face 'bold))
    (insert "─────────\n")
    (insert "s - Spin wheel          b - Place bet         c - Clear bets\n")
    (insert "t - Toggle game type    h - Show history      r - Reset bankroll\n")
    (insert "v - Voisins bet         o - Orphelins bet     i - Tiers bet\n")
    (insert "m - Martingale mode     f - Fibonacci mode    l - Labouchere mode\n")
    (insert "? - Help                q - Quit\n")))

(defun roulette-spin-wheel ()
  "Spin the roulette wheel and calculate results."
  (interactive)
  (if (null roulette-current-bets)
      (message "No bets placed!")
    (let ((result (roulette-spin))
          (total-win 0)
          (total-bet 0))
      (push result roulette-spin-history)

      ;; Calculate total bet amount
      (dolist (bet roulette-current-bets)
        (setq total-bet (+ total-bet (roulette-bet-amount bet))))

      ;; Calculate winnings for each bet
      (dolist (bet roulette-current-bets)
        (let ((bet-result (roulette-calculate-bet-result bet result)))
          (setq total-win (+ total-win bet-result))))

      ;; Update bankroll
      (setq roulette-bankroll (+ roulette-bankroll total-win))

      ;; Add to history and clear current bets
      (push (list :spin result :bets roulette-current-bets :net total-win) roulette-bet-history)
      (setq roulette-current-bets '())

      ;; Show result
      (roulette-render-interface)
      (message "Spin result: %s - Bet: $%.2f, Win: $%.2f, Net: $%.2f"
               (roulette-format-number result) total-bet (+ total-bet total-win) total-win))))

(defun roulette-place-bet ()
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
                      ("High (19-36)" . high)))
         (bet-type-name (completing-read "Bet type: "
                                         (mapcar 'car bet-types) nil t))
         (bet-type (cdr (assoc bet-type-name bet-types)))
         (amount (read-number "Bet amount: $" 10))
         (numbers (roulette-get-bet-numbers bet-type)))

    (when (> amount roulette-bankroll)
      (error "Insufficient bankroll"))

    (push (make-roulette-bet :type bet-type
                              :numbers numbers
                              :amount amount
                              :payout (roulette-get-payout bet-type))
          roulette-current-bets)

    (setq roulette-bankroll (- roulette-bankroll amount))
    (roulette-render-interface)))

(defun roulette-get-bet-numbers (bet-type)
  "Get numbers for BET-TYPE interactively if needed."
  (cond
   ((eq bet-type 'straight)
    (list (read-number "Number (0-36): ")))
   ((eq bet-type 'split)
    (list (read-number "First number: ")
          (read-number "Second number: ")))
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

(defun roulette-clear-bets ()
  "Clear all current bets."
  (interactive)
  (dolist (bet roulette-current-bets)
    (setq roulette-bankroll (+ roulette-bankroll (roulette-bet-amount bet))))
  (setq roulette-current-bets '())
  (roulette-render-interface)
  (message "All bets cleared"))

(defun roulette-toggle-game-type ()
  "Toggle between European and American roulette."
  (interactive)
  (setq roulette-game-type
        (if (eq roulette-game-type 'european) 'american 'european))
  (roulette-render-interface)
  (message "Switched to %s roulette" (capitalize (symbol-name roulette-game-type))))

(defun roulette-reset-bankroll ()
  "Reset bankroll to initial amount."
  (interactive)
  (let ((amount (read-number "New bankroll amount: $" 1000)))
    (setq roulette-bankroll amount)
    (roulette-render-interface)))

(defun roulette-place-voisins ()
  "Place a Voisins du Zero bet."
  (interactive)
  (let* ((chip-value (read-number "Chip value: $" 10))
         (amount (* chip-value 9)))
    (when (> amount roulette-bankroll)
      (error "Insufficient bankroll (need $%.2f for 9 chips @ $%.2f)" amount chip-value))

    (push (make-roulette-bet :type 'voisins
                              :numbers (append roulette-voisins-numbers nil)
                              :amount amount
                              :payout nil)
          roulette-current-bets)

    (setq roulette-bankroll (- roulette-bankroll amount))
    (roulette-render-interface)
    (message "Placed Voisins du Zero bet: 9 chips @ $%.2f = $%.2f total" chip-value amount)))

(defun roulette-place-orphelins ()
  "Place an Orphelins bet."
  (interactive)
  (let* ((chip-value (read-number "Chip value: $" 10))
         (amount (* chip-value 5)))
    (when (> amount roulette-bankroll)
      (error "Insufficient bankroll (need $%.2f for 5 chips @ $%.2f)" amount chip-value))

    (push (make-roulette-bet :type 'orphelins
                              :numbers (append roulette-orphelins-numbers nil)
                              :amount amount
                              :payout nil)
          roulette-current-bets)

    (setq roulette-bankroll (- roulette-bankroll amount))
    (roulette-render-interface)
    (message "Placed Orphelins bet: 5 chips @ $%.2f = $%.2f total" chip-value amount)))

(defun roulette-place-tiers ()
  "Place a Tiers du Cylindre bet."
  (interactive)
  (let* ((chip-value (read-number "Chip value: $" 10))
         (amount (* chip-value 6)))
    (when (> amount roulette-bankroll)
      (error "Insufficient bankroll (need $%.2f for 6 chips @ $%.2f)" amount chip-value))

    (push (make-roulette-bet :type 'tiers
                              :numbers (append roulette-tiers-numbers nil)
                              :amount amount
                              :payout nil)
          roulette-current-bets)

    (setq roulette-bankroll (- roulette-bankroll amount))
    (roulette-render-interface)
    (message "Placed Tiers du Cylindre bet: 6 chips @ $%.2f = $%.2f total" chip-value amount)))

(defun roulette-show-help ()
  "Show help for roulette calculator."
  (interactive)
  (with-help-window "*Roulette Help*"
    (princ "ROULETTE CALCULATOR HELP\n")
    (princ "========================\n\n")
    (princ "Bet Types:\n")
    (princ "----------\n")
    (princ "• Straight: Single number (35:1)\n")
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

(defvar roulette-strategy-mode nil
  "Current betting strategy mode.")

(defvar roulette-strategy-state nil
  "State for current betting strategy.")

(defun roulette-martingale-mode ()
  "Enable Martingale betting strategy."
  (interactive)
  (setq roulette-strategy-mode 'martingale)
  (setq roulette-strategy-state '(:base-bet 10 :current-bet 10 :last-won nil))
  (message "Martingale strategy enabled (base bet: $10)"))

(defun roulette-fibonacci-mode ()
  "Enable Fibonacci betting strategy."
  (interactive)
  (setq roulette-strategy-mode 'fibonacci)
  (setq roulette-strategy-state '(:position 0 :last-won nil))
  (message "Fibonacci strategy enabled"))

(defun roulette-labouchere-mode ()
  "Enable Labouchere betting strategy."
  (interactive)
  (setq roulette-strategy-mode 'labouchere)
  (let ((sequence (read-string "Enter number sequence (e.g., 1 2 3 4): ")))
    (setq roulette-strategy-state
          (list :sequence (mapcar 'string-to-number (split-string sequence))
                :original-sequence (mapcar 'string-to-number (split-string sequence))
                :last-won nil)))
  (message "Labouchere strategy enabled"))

(defun roulette-show-history ()
  "Show detailed betting history."
  (interactive)
  (with-current-buffer (get-buffer-create "*Roulette History*")
    (erase-buffer)
    (insert "ROULETTE HISTORY\n")
    (insert "================\n\n")

    (if (null roulette-spin-history)
        (insert "No spins yet\n")
      (let ((spin-count 0))
        (dolist (spin (reverse roulette-spin-history))
          (insert (format "Spin #%d: " (1+ spin-count)))
          (insert (apply 'propertize (roulette-format-number spin)
                         (roulette-number-display-properties spin)))
          (insert "\n")
          (setq spin-count (1+ spin-count)))))

    (insert "\n\nStatistics:\n")
    (insert "-----------\n")
    (when roulette-spin-history
      (let ((reds 0) (blacks 0) (greens 0)
            (evens 0) (odds 0) (lows 0) (highs 0))
        (dolist (num roulette-spin-history)
          (let ((color (roulette-number-color num)))
            (cond ((eq color 'red) (setq reds (1+ reds)))
                  ((eq color 'black) (setq blacks (1+ blacks)))
                  ((eq color 'green) (setq greens (1+ greens))))
            (when (roulette-is-even num) (setq evens (1+ evens)))
            (when (roulette-is-odd num) (setq odds (1+ odds)))
            (when (roulette-is-low num) (setq lows (1+ lows)))
            (when (roulette-is-high num) (setq highs (1+ highs)))))

        (let ((total (length roulette-spin-history)))
          (insert (format "Red: %d (%.1f%%)\n" reds (* 100.0 (/ reds total))))
          (insert (format "Black: %d (%.1f%%)\n" blacks (* 100.0 (/ blacks total))))
          (insert (format "Green: %d (%.1f%%)\n" greens (* 100.0 (/ greens total))))
          (insert (format "Even: %d (%.1f%%)\n" evens (* 100.0 (/ evens total))))
          (insert (format "Odd: %d (%.1f%%)\n" odds (* 100.0 (/ odds total))))
          (insert (format "Low: %d (%.1f%%)\n" lows (* 100.0 (/ lows total))))
          (insert (format "High: %d (%.1f%%)\n" highs (* 100.0 (/ highs total)))))))

    (display-buffer (current-buffer))))

(provide 'roulette-calculator)

;;; roulette-calculator.el ends here
