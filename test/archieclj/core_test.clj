(ns archieclj.core-test
  (:require [clojure.test :refer :all]
            [archieclj.core :refer :all]))

(deftest empty-string
  (testing "empty-string"
    (is (= (parse "") {}))))

(deftest empty-string-2
  (testing "more-empty-string"
    (is (= (parse "foo bar") {}))))

(deftest token-detection-1
  (testing "detecting :token"
    (is (= (is-token? ":token") true))))

(deftest token-detection-2
  (testing "detecting token:"
    (is (= (is-token? "token:") true))))

(deftest token-detection-3
  (testing "detecting {token}"
    (is (= (is-token? "{token}") true))))

(deftest token-detection-4
  (testing "detecting [token]"
    (is (= (is-token? "[token]") true))))

(deftest token-detection-5
  (testing "detecting {}"
    (is (= (is-token? "{}") true))))

(deftest token-detection-6
  (testing "detecting []"
    (is (= (is-token? "[]") true))))

(deftest token-detection-7
  (testing "detecting *"
    (is (= (is-token? "*") true))))

(deftest token-detection-8
  (testing "detecting token: value"
    (is (= (is-token? "token: value") true))))

(deftest token-detection-9
  (testing "not detecting token value"
    (is (= (is-token? "token value") false))))

(deftest token-detection-10
  (testing "not detecting \\token: value"
    (is (= (is-token? "\\token: value") false))))

(deftest split-line-test
  (testing "spliting lines"
    (is (= (split-lines "foo\nbar")
           ["foo" "bar"]))))

(deftest command-detection-1
  (testing "detecting command :end"
    (is (= (is-command? ":end") "end"))))

(deftest command-detection-2
  (testing "not detecting command end"
    (is (= (is-command? "end") nil))))

(deftest command-detection-3
  (testing "not detecting \\:end"
    (is (= (is-command? "\\:end") nil))))

(deftest command-detection-4
  (testing "not detecting token: value"
    (is (= (is-command? "token: value") nil))))

(deftest command-matching-1
  (testing "testing command matching :ignore ignore"
    (is (= (is-command? ":ignore" "ignore") true))))

(deftest command-matching-2
  (testing "testing command matching :ignorethis ignore"
    (is (= (is-command? ":ignorethis" "ignore") true))))

(deftest key-detection-1
  (testing "testing token: value"
    (is (= (is-key? "token: value") "token"))))

(deftest key-detection-2
  (testing "not detecting \\token: value"
    (is (= (is-key? "\\token: value") nil))))

(deftest key-detection-3
  (testing "not detecting token value"
    (is (= (is-key? "token value") nil))))

(deftest array-detection-1
  (testing "testing [token]"
    (is (= (is-array? "[token]") "token"))))

(deftest array-detection-2
  (testing "testing [token.two]"
    (is (= (is-array? "[token.two]") "token.two"))))

(deftest array-detection-3
  (testing "not array token"
    (is (= (is-array? "token") nil))))

(deftest scope-detection-1
  (testing "detecting scope {token}"
    (is (= (is-scope? "{token}") "token"))))

(deftest scope-detection-2
  (testing "detecting scope {token.two}"
    (is (= (is-scope? "{token.two}") "token.two"))))

(deftest scope-detection-3
  (testing "detecing scope token"
    (is (= (is-scope? "token") nil))))

(deftest test-key-parsing-1
  (testing "parse-key ['token: value']"
    (is (= (get (parse-key ["token: value"] {}) 1)
           {:token "value"}))))

(deftest test-key-parsing-2
  (testing "parse-key ['token: value' 'token2: value2']"
    (is (= (get (parse-key ["token: value" "token2: value2"] {}) 1)
           {:token "value"}))))
(deftest test-key-parsing-3
  (testing "parse-key ['token: value' 'comment' 'token2: value2']"
    (is (= (get
            (parse-key ["token: value" "comment" "token2: value2"]
                       {}) 1)
           {:token "value"}))))

(deftest test-key-parsing-4
  (testing "parse-key ['token: value' 'newline' ':end']"
    (is (= (get
            (parse-key ["token: value" "newline" ":end"] {})
            1)
           {:token "value\nnewline"}))))
