(ns cloptions.strategies)

(comment "Might be a good time to use prismatic/Schema")

(defrecord Stock [ticker price])
(comment "Example `(Stock. :TSLA 230.00)`")

(defrecord Option [asset buy-sell qty strike-price expiration premium])
(comment "Example `(Option. TSLA :long-call 100 55.50 some-date 5.50)`")

;;
;; Multimethod for various option primitives
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



(defn strategy-fn [options]
  (reduce + options))
