(ns archieclj.core)
(require '[clojure.string :as string])




(defn split-lines
  "Splits a text file to multiple lines"
  [x]
  (seq (string/split x #"\n")))

(defn is-command?
  "Returns the command if a line contains a command nil-otherwise.
  optionally takes a string and returns true if the command matches the string."
  ([x]
   (if (not (re-find #"\\:([a-zA-Z-]+)" x))
     (get (re-find #":([a-zA-Z-]+)" x) 1)))
  ([x y]
   (let [c (is-command? x)]
     (if c (.startsWith c y)))))

(defn is-key?
  "Returns the key, nil otherwise"
  [x]
  (if (not (re-find #"\\([a-zA-Z-]+):" x))
    (get (re-find #"([a-zA-Z-]+):" x) 1)))

(defn is-array?
  "returns the array if true, nil otherwise"
  [x]
  (get (re-find #"\[([a-zA-Z-.]*)\]"
                x) 1))

(defn is-scope?
  "returns the scope if true, nil otherwise"
  [x]
  (get (re-find #"\{([a-zA-Z-.]*)\}"
                x) 1))

(defn is-item?
  "returns true if the line is a list item, nil otherwise"
  [x]
  (boolean (re-find #"\*" x)))

(defn is-token?
  "Returns true if a line contains a token"
  [x]
  (let [tokens [is-command? is-key? is-array? is-scope? is-item?]]
    (boolean (reduce (fn [r t] (or (t x) r)) false tokens))))

(defn parse-key
  "parses a line with a key and amends the object.
  returns a vector with the rest of the lines as the first object and the object as the second."
  [lines obj]
  (let [m (re-find #"([a-zA-Z.-]+):[ ]{0,1}(.*)" (first lines))
        k (get m 1)
        v (get m 2)
        totoken (drop-while (fn[x] (not (is-token? x))) (rest lines))]
    (println totoken)
    (if (and (first totoken)
             (is-command? (first totoken) "end"))
      [(rest totoken)
       (assoc obj
              (keyword k)
              (string/join
               "\n"
               (cons v
                     (take-while (fn [x]
                                   (not (is-command? x "end")))
                                 (rest lines)))))]
      [(rest lines) (assoc obj (keyword k) v)])))

(defn parse-line
  "Parses a line and returns a changed returns an array of the remaining lines and the modified return object"
  [lines obj]
  (if (< (count lines) 1)
    [[] obj]
    (let [tokens [[is-key? parse-key]]]
      (reduce (fn [x y] (if ((get y 0) (first lines))
                          ((get y 1) lines obj)
                          x))
              [(rest lines) obj] tokens))))

(defn parse
  "Parses an Archieml string and returns a map"
  [x]
  (loop [lines (take-while (fn [d] (not (is-command? d "ignore")))
                           (split-lines x))
         ret {}]
    (if (< (count lines) 1) ret
        (let [po (parse-line (drop-while (fn [x]
                                           (not (is-token? x)))
                                         lines) ret)
              lns (get po 0)
              rt (get po 1)]
          (recur lns rt)))))
