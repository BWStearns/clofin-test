(ns cloptions.strategies
  (:require [instaparse.core :as insta]))

(comment "Might be a good time to use prismatic/Schema")

(defrecord Stock [ticker price])
(comment "Example `(Stock. :TSLA 230.00)`")

(defrecord Option [asset option-type qty strike-price expiration premium])
(comment "Example `(Option. TSLA :long-call 100 55.50 some-date 5.50)`")


;; LL1606E11.5             <= Merrill, month is a-l, z-? or whatever. Old month pattern
;; LL160506P00011500       <= Yahoo!
;; 160506C00011500-LL-CALL <= Nasdaq

(def ml-opt-parser (instaparse.core/parser
 "OPT           = ML_OPT | YH_OPT | NASDAQ_OPT;

  ML_OPT        = TICKER, YR, DY, ML_MO, STRIKE_PRICE;
  YH_OPT        = TICKER, YR, DY, MO, DIR, STRIKE_PRICE;
  NASDAQ_OPT    = YR, DY, MO, DIR, STRIKE_PRICE, DASH, TICKER, DASH, DIR;

  STRIKE_PRICE  = NUM;
  DIR           = 'P'|'C'|'PUT'|'CALL';
  TICKER        = LTRS;
  NUM           = D+ { '.', D+ };
  YR            = D, D;
  MO            = D, D;
  ML_MO         = LTR;
  DY            = D, D;
  DASH          = '-';
  <D>           = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9';
  LTRS          = LTR+;
  <LTR>         = #'[A-z]';
"))


(defn x-chop [name & {:keys [x] :or {x "karate"}}]
  (println "I" x "chop" name))


;; (ml-opt-parser "LL1606E11.5")

;; (def p$str->int (comp Integer.parseInt str))
;; Find a way of using ints or decimals instad of floats because floats fuck finance (f3)
;; (def p$str->float (comp Float.parseFloat str))

;; (def option-transforms
;;   {:LTRS str
;;    :}
;;   )

;;
;; Multimethod for various option primitives
;;
;; TODO
;; * DRY the pattern in the profit functions.
;; * Make options->strategy function
;; * Make options chain parser to produce records
;; * Make optimizer for picking from options chains
;;
;; * Write valuation models (Black-Scholes, others?)
;;

(defmulti profit-function :option-type)

(defmethod profit-function :long-call
  [opt]
  (fn [maturity-price]
    (- (max (- maturity-price (:strike-price opt)) 0)
       (:premium opt))))

(defmethod profit-function :long-put
  [opt]
  (fn [maturity-price]
    (- (max (- (:strike-price opt) maturity-price) 0)
       (:premium opt))))

(defmethod profit-function :short-call
  [opt]
  (fn [maturity-price]
    (- (:premium opt)
       (max (- maturity-price (:strike-price opt)) 0))))

(defmethod profit-function :short-put
  [opt]
  (fn [maturity-price]
    (- (:premium opt)
       (max (- (:strike-price opt) maturity-price) 0))))



(defn strategy-fn [options]
  (reduce + options))
