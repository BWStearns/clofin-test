(ns cloptions.strategies-test
  (:require [clojure.test :refer :all]
            [cloptions.strategies :refer :all]))

(def bofa-ll "LL1606E11.5")
(def yahoo-ll "LL160506P00011500")
(def nasdaq-ll "160506C00011500-LL-CALL")


(deftest options-parser

  (testing "Doesn't parse malformed options"
    (is (= instaparse.gll.Failure (type (opts-parser "FOOBAR!123-456")))))

  (testing "Can parse a BOFA option name"
    (is (= (opts-parser bofa-ll)
           [:OPT
            [:ML_OPT
             [:TICKER
              [:LTRS "L" "L"]]
             [:YR "1" "6"]
             [:DY "0" "6"]
             [:ML_MO "E"]
             [:STRIKE_PRICE
              [:NUM "1" "1" "." "5"]]]])))

  (testing "Can parse a Yahoo option name"
    (is (= (opts-parser yahoo-ll)
           [:OPT
            [:YH_OPT
             [:TICKER
              [:LTRS "L" "L"]]
             [:YR "1" "6"]
             [:DY "0" "5"]
             [:MO "0" "6"]
             [:DIR "P"]
             [:STRIKE_PRICE
              [:NUM "0" "0" "0" "1" "1" "5" "0" "0"]]]])))

  (testing "Can parse a NASDAQ option name"
    (is (= (opts-parser nasdaq-ll)
           [:OPT
            [:NASDAQ_OPT
             [:YR "1" "6"]
             [:DY "0" "5"]
             [:MO "0" "6"]
             [:DIR "C"]
             [:STRIKE_PRICE
              [:NUM "0" "0" "0" "1" "1" "5" "0" "0"]]
             [:DASH "-"]
             [:TICKER
              [:LTRS "L" "L"]]
             [:DASH "-"]
             [:DIR "CALL"]]]))))


(deftest options-parser
  (testing "Identical options parsed from different sources result in the same clojure option record"
    (is (= (parse->Option bofa-ll)
           (parse->Option yahoo-ll)
           (parse->Option nasdaq-ll)))))
