(ns cloptions.strategies
  (:require [clj-time.core :as t]
            [instaparse.core :as insta]
            [clojure.edn]))

(comment "Might be a good time to use prismatic/Schema")

(defrecord Stock [ticker price])
(comment "Example `(Stock. :TSLA 230.00)`")

(defrecord Option [ticker direction strike-price expiration])
(comment "Example `(Option. TSLA :long-call 100 55.50 some-date 5.50)`")


;; LL1606E11.5             <= Merrill, month is a-l, z-? or whatever. Old month pattern
;; LL160506P00011500       <= Yahoo!
;; 160506C00011500-LL-CALL <= Nasdaq

(def bofa-ll "LL1606E11.5")
(def yahoo-ll "LL160506P00011500")
(def nasdaq-ll "160506C00011500-LL-CALL")
(def example-opts [bofa-ll yahoo-ll nasdaq-ll])

;; and then MARKET WATCH has another!!!
;; LLE27164115000

;; I think this might eventually break out a bit to be composed of smaller
;; grammar fragments so it can be reused in non-options contexts.
;; Might need to augment with some combinator rules if bounded-reps are needed.
(def opts-parser (instaparse.core/parser
 "OPT           = ML_OPT | YH_OPT | NASDAQ_OPT;

  ML_OPT        = TICKER, YR, DY, ML_MO, ML_PRICE;
  YH_OPT        = TICKER, YR, DY, MO, DIR, STRIKE_PRICE;
  NASDAQ_OPT    = YR, DY, MO, <DIR>, STRIKE_PRICE, DASH, TICKER, DASH, DIR;

  STRIKE_PRICE  = NUM;
  ML_PRICE      = NUM ?['.' (D | TWODIG)];
  DIR           = 'P'|'C'|'PUT'|'CALL';
  TICKER        = LTRS;
  YR            = TWODIG;
  MO            = TWODIG;
  ML_MO         = #'[A-z]';
  DY            = TWODIG;
  <TWODIG>      = #'\\d\\d';
  <FOURDIG>     = #'\\d\\d\\d\\d';
  <NUM>         = #'[0-9]+';
  <DASH>        = <'-'>;
  <D>           = #'[0-9]';
  <LTRS>        = #'[A-z]+';
  <LTR>         = #'[A-z]';
"))

(defn ml-month_map [ml_mo]
  (println "I'M STILL NOT IMPLEMENTED!")
  [[:month 6][:direction :call]])

(defn ymd->date [map-with-ymd]
  (println map-with-ymd)
  (into map-with-ymd
        {:expiration (apply t/date-time
                            ((juxt :year :month :day) map-with-ymd))}))

(def option-transforms
  {:OPT            (fn [opt] (map->Option (ymd->date opt)))
   :YH_OPT         (fn [& args] (apply hash-map (flatten args)))
   :ML_OPT         (fn [& args] (apply hash-map (flatten args)))
   :NASDAQ_OPT     (fn [& args] (apply hash-map (flatten args)))

   :TICKER         (fn [ticker] [:ticker ticker])
   :MO             (fn [mo] [:month (Integer. mo)])
   :ML_MO          (fn [mo] (ml-month_map mo))
   :DY             (fn [dy] [:day (Integer. dy)])
   :YR             (fn [yr] [:year (+ 2000 (Integer. yr))])
   :DIR            (fn [dir] [:direction ({\P :put \C :call} (first dir))])
   :STRIKE_PRICE   (fn [price] [:strike-price (/ (bigdec (str price)) 1000)])
   :ML_PRICE       (fn [& price] [:strike-price (bigdec (apply str price))])
  })

(defn parse->Option [opt]
  (instaparse.core/transform option-transforms (opts-parser opt)))


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
