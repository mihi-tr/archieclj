(ns archieclj.core-test
  (:require [clojure.test :refer :all]
            [archieclj.core :refer :all]))
;; parse

(deftest empty-string
  (testing "empty-string"
    (is (= (parse "") {}))))

(deftest empty-string-2
  (testing "more-empty-string"
    (is (= (parse "foo bar") {}))))

(deftest single-key
  (testing "parsing a single key"
    (is (= (parse "key: value")
           {:key "value"}))))

(deftest two-keys
  (testing "parsing two keys"
    (is (= (parse
            "key: value
             key2: value
             ")
           {:key "value" :key2 "value"}))))

(deftest key-overwriting
  (testing "key overwriting"
    (is (= (parse
            "key: value
             key: value2")
           {:key "value2"}))))

(deftest line-skipping
  (testing "line skipping"
    (is (= (parse
            "key: value
            :skip
            key: not value
            :endskip
            key2: value2
            ")
           {:key "value" :key2 "value2"}))))

(deftest ignorelines
  (testing "ignoring lines"
    (is (= (parse
            "key: value
             :ignore
             key: not value
             key2: also ignored
            ")
           {:key "value"}))))

(deftest basic-scope
  (testing "basic scope parsing"
    (is (= (parse
            "{token}
             key: value")
           {:token {:key "value"}}))))

(deftest basic-scope-2
  (testing "basic scope parsing"
    (is (= (parse
            "key: value
             {token}
             key2: value2")
           {:key "value", :token {:key2 "value2"}}))))

(deftest scope-switching
  (testing "scope-switching"
    (is (= (parse
            "{token}
             key: value
             {token2}
             key2: value2")
           {:token {:key "value"}, :token2 {:key2 "value2"}}))))

(deftest back-to-main-scope
  (testing "switching-back-to-main-scope"
    (is (= (parse
            "{token}
             key: value
             {}
             key2: value2")
           {:token {:key "value"}, :key2 "value2"}))))

(deftest skipping-in-scope
  (testing "skipping in scope"
    (is (= (parse
            "{token}
             key: value
             :skip
             {}
             :endskip
             key2: value2")
           {:token {:key "value", :key2 "value2"}}))))

;; is-token?

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

;; split-lines

(deftest split-line-test
  (testing "spliting lines"
    (is (= (split-lines "foo\nbar")
           ["foo" "bar"]))))

(deftest split-line-test-2
  (testing "whitespace removal in splitlines"
    (is (= (split-lines " foo\nbar ")
           ["foo" "bar"]))))

;; is-command?

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

(deftest command-detection-5
  (testing "not detecting no :command"
    (is (= (is-command? "no :command") nil))))

(deftest command-matching-1
  (testing "testing command matching :ignore ignore"
    (is (= (is-command? ":ignore" "ignore") true))))

(deftest command-matching-2
  (testing "testing command matching :ignorethis ignore"
    (is (= (is-command? ":ignorethis" "ignore") true))))

;; is-key?

(deftest key-detection-1
  (testing "testing token: value"
    (is (= (is-key? "token: value") "token"))))

(deftest key-detection-1a
  (testing "testing token2: value"
    (is (= (is-key? "token2: value") "token2"))))

(deftest key-detection-1b
  (testing "testing to-ken:"
    (is (= (is-key? "to-ken:") "to-ken"))))

(deftest key-detection-1c
  (testing "testing to_ken:"
    (is (= (is-key? "to_ken:") "to_ken"))))
   
(deftest key-detection-2
  (testing "not detecting \\token: value"
    (is (= (is-key? "\\token: value") nil))))

(deftest key-detection-3
  (testing "not detecting token value"
    (is (= (is-key? "token value") nil))))

(deftest key-detection-4
  (testing "not detecting not a: token"
    (is (= (is-key? "not a: token") nil))))

;; is-array?

(deftest array-detection-1
  (testing "testing [token]"
    (is (= (is-array? "[token]") "token"))))

(deftest array-detection-1a
  (testing "testing [token2] value"
    (is (= (is-array? "[token2]") "token2"))))

(deftest array-detection-1b
  (testing "testing [to-ken]"
    (is (= (is-array? "[to-ken]") "to-ken"))))

(deftest array-detection-1c
  (testing "testing [to_ken]"
    (is (= (is-array? "[to_ken]") "to_ken"))))

(deftest array-detection-2
  (testing "testing [token.two]"
    (is (= (is-array? "[token.two]") "token.two"))))

(deftest array-detection-3
  (testing "not array token"
    (is (= (is-array? "token") nil))))

(deftest array-detection-4
  (testing "not detectiong not [array]"
    (is (= (is-array? "not [array]") nil))))

;; is-scope?

(deftest scope-detection-1
  (testing "detecting scope {token}"
    (is (= (is-scope? "{token}") "token"))))

(deftest scope-detection-1a
  (testing "testing {token2} value"
    (is (= (is-scope? "{token2}") "token2"))))

(deftest scope-detection-1b
  (testing "testing {to-ken}"
    (is (= (is-scope? "{to-ken}") "to-ken"))))

(deftest scope-detection-1c
  (testing "testing {to_ken}"
    (is (= (is-scope? "{to_ken}") "to_ken"))))

(deftest scope-detection-2
  (testing "detecting scope {token.two}"
    (is (= (is-scope? "{token.two}") "token.two"))))

(deftest scope-detection-3
  (testing "detecing scope token"
    (is (= (is-scope? "token") nil))))

(deftest scope-detection-4
  (testing "not detecting not {scope}"
    (is (= (is-scope? "not {scope}")
           nil))))

;; parse-key

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

(deftest test-key-parsing-5
  (testing "parse-key ['token: value'] overwriting"
    (is (= (get
            (parse-key ["token: value"] {:token "notvalue"})
            1)
           {:token "value"}))))

(deftest test-key-parsing-6
  (testing "parse-key ['token2: value']"
    (is (= (get
            (parse-key ["token2: value"] {})
            1)
           {:token2 "value"}))))

(deftest test-key-parsing-7
  (testing "parse-key ['to-ken: value']"
    (is (= (get
            (parse-key ["to-ken: value"] {})
            1)
           {:to-ken "value"}))))

(deftest test-key-parsing-8
  (testing "parse-key ['to_ken: value']"
    (is (= (get
            (parse-key ["to_ken: value"] {})
            1)
           {:to_ken "value"}))))

;; skip

(deftest skip-test-1
  (testing "skipping ['zero' ':skip' 'comment' ':endskip' 'one']"
    (is (= 
         (skip ["zero" ":skip" "comment" ":endskip" "one"])
         ["zero" "one"]))))

(deftest skip-test-2
  (testing "skipping multiple blocks"
    (is (=
         (skip ["zero" ":skip" "comment" ":endskip" "one"
                ":skip" "another comment" ":endskip" "two"])
         ["zero" "one" "two"]))))

;; process-scope

(deftest scope-processing-1
  (testing "basic scope processing"
    (is (= (get
            (process-scope ["{token}" "key: value"] {})
            1)
           {:token {:key "value"}}))))

(deftest scope-processing-2
  (testing "switching-scopes"
    (is (= (get
            (process-scope ["{token}" "key: value"
                            "{token2}" "key2: value2"]
                           {})
            1)
           {:token {:key "value"}}))))

(deftest scope-processing-3
  (testing "switching to main scope"
    (is (= (get
            (process-scope ["{token}" "key: value"
                            "{}" "key2: value2"]
                           {})
            1)
           {:token {:key "value"}}))))

(deftest scope-processing-4
  (testing "switching to array"
    (is (= (get
            (process-scope ["{token}" "key: value" "[]" "key2: value2"]
                           {})
            1)
           {:token {:key "value"}}))))
