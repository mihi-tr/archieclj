(ns archieclj.core)
(require '[clojure.string :as string])



(declare process-scope)
(declare process-array)

(defn trim-split-lines
  "Splits a text file to multiple lines"
  [x]
  (map string/trim
       (string/split-lines x)))

(defn get-map
  "takes a map and a keyword. Returns the map on the keyword if it exists. otherwise returns an empty map."
  [m kw]
  (let [r (kw m)]
    (if (map? r) r {})))
  
(defn is-command?
  "Returns the command if a line contains a command nil-otherwise.
  optionally takes a string and returns true if the command matches the string."
  ([x]
    (get (re-find #"^:([a-z]+)" x) 1))
  ([x y]
   (let [c (is-command? x)]
     (if c (.startsWith c y)))))

(defn is-key?
  "Returns the key, nil otherwise"
  [x]
  (get (re-find #"^([a-zA-Z0-9-_.]+):" x) 1))

(defn is-array?
  "returns the array if true, nil otherwise"
  [x]
  (get (re-find #"^\[([a-zA-Z0-9-_.]*)\]"
                x) 1))

(defn is-subarray?
  "returns true on subarray, false otherwise"
  [x]
  (and (is-array? x)
       (.startsWith (is-array? x) ".")))

(defn is-scope?
  "returns the scope if true, nil otherwise"
  [x]
  (get (re-find #"^\{([a-zA-Z0-9-_.]*)\}"
                x) 1))

(defn is-item?
  "returns true if the line is a list item, nil otherwise"
  [x]
  (boolean (re-find #"^\*" x)))

(defn is-token?
  "Returns true if a line contains a token"
  [x]
  (let [tokens [is-command? is-key? is-array? is-scope?]]
    (boolean (reduce (fn [r t] (or (t x) r)) false tokens))))

(defn expand-scopes
  "expands the scopes for scoped tokens and sets the value"
  [t obj value]
  (let [s (string/split (if (.startsWith t ".")
                          (subs t 1)
                          t)
                        #"\." 2)
        kw (keyword (first s))]
    (if (get s 1)
      (assoc obj kw (expand-scopes (get s 1) (get-map obj kw) value))
      (assoc obj kw value))))

(defn get-expanded-map
  "expands a scope and returns the map at the end"
  [t obj]
  (let [s (string/split t #"\." 2)
        kw (keyword (first s))]
    (if (get s 1)
      (get-expanded-map (get s 1) (get-map obj kw))
      (get-map obj kw))))

(defn parse-key
  "parses a line with a key and amends the object.
  returns a vector with the rest of the lines as the first object and the object as the second."
  [lines obj]
  (let [m (re-find #"^([a-zA-Z0-9-_.]+):[ ]{0,1}(.*)" (first lines))
        k (get m 1)
        v (get m 2)
        totoken (drop-while (fn[x] (not (is-token? x))) (rest lines))]
    (if (and (first totoken)
             (is-command? (first totoken) "end"))
      [(rest totoken)
       (expand-scopes k
                      obj
                      (string/join
                       "\n"
                       (cons v
                             (take-while (fn [x]
                                           (not (is-command? x "end")))
                                         (rest lines)))))]
      [(rest lines) (expand-scopes k obj v)])))

(defn skip
  "skips all lines from :skip to :endskip. Returns unskipped lines"
  [lines]
  (loop [lns lines r []]
    (if (seq lns)
      (recur 
       (rest (drop-while (fn [x] (not (is-command? x "endskip")))
                         lns))
       (apply conj r (take-while (fn [x] (not (is-command? x "skip")))
                         lns)))
      r)))
                              
(defn parse-line
  "Parses a line and returns a changed returns an array of the remaining lines and the modified return object"
  [lines obj]
  (if (empty? lines)
    [[] obj]
    ;; tokens below declares the parsing behavior for lines
    ;; the first is a matching function. If it returns true
    ;; the second function will be called with lines and obj
    (let [tokens [
                  [is-key? parse-key] ; parse keys
                  [is-scope? process-scope] ; process scopes
                  [is-array? process-array] ; process array
                  ]]
      (reduce (fn [x y] (if ((get y 0) (first lines))
                          ((get y 1) lines obj)
                          x))
              [(rest lines) obj] tokens))))

(defn parse-lines
  "parses lines and returns a map. Takes lines and optionally a hashmap.
  Returns a hashmap with the parsed lines"
  ([lines]
   (parse-lines lines {}))
  ([lines obj]
   (loop [l lines ret obj]
     (if (seq l)
       (let [po (parse-line (drop-while (fn [x]
                                          (not (is-token? x)))
                                        l) ret)
             lns (get po 0)
             rt (get po 1)]
          (recur lns rt))
       ret))))

(defn take-delimited-array
  "takes lines and a check function - ignores subarrays"
  [dfn lines]
  (loop [lns lines sa 0 ret []]
    (if (empty? lns)
          ret
      (if (and (dfn (first lns))
               (<= sa 0))
            ret
        (if (is-subarray? (first lns))
          (recur (rest lns) (+ sa 1) (conj ret (first lns)))
          (if (= (is-array? (first lns)) "")
            (recur (rest lns) (- sa 1) (conj ret (first lns)))
            (recur (rest lns) sa (conj ret (first lns)))))))))

(defn drop-delimited-array
  "drops lines - takes the check function and ignores subarrays"
  [dfn lines]
  (loop [lns lines sa 0]
    (if (empty? lns)
      []
      (if (and (dfn (first lns))
               (<= sa 0))
        lns
        (if (is-subarray? (first lns))
          (recur (vec (rest lns)) (+ sa 1))
          (if (= (is-array? (first lns)) "")
            (recur (vec (rest lns)) (- sa 1))
            (recur (vec (rest lns)) sa)))))))

(defn parse-delimited-array
  "takes lines and returns an array of objects delimited.
  takes lines and a delimiter function
  "
  [lines dfn]
  (let [delimiter (dfn (first lines))]
    (loop [lns lines ret []]
      (if (first lns)
        (recur (drop-delimited-array (fn [x] (= (dfn x)
                                                delimiter))
                           (rest lns))
               (conj ret
                     (parse-lines
                      (cons (first lns)
                            (take-delimited-array (fn [x]
                                                    (= (dfn x)
                                                       delimiter))
                                                  (rest lns))))))
               ret))))

(defn parse-item-array
  "takes lines and returns an array of items"
  [lines]
  (loop [lns lines ret []]
    (if (first lns)
      (let [v (get (re-find #"^* (.*)" (first lns)) 1)
            matchfn (fn [x] (and
                             (not (is-item? x))
                             (not (is-command? x "end"))))
            ltn (take-while matchfn (rest lns))
            al (drop-while matchfn (rest lns))]
        (if (and (first al)
                 (is-command? (first al) "end"))
          (recur (rest al) (conj ret
                                 (string/join
                                  "\n"
                                  (cons v ltn))))
          (recur al (conj ret v))))
      ret)))

(defn parse-array
  "takes lines and parses an array"
  [lines]
  (let [l (drop-while (fn [x]
                        (and
                         (not (is-key? x))
                         (not (is-item? x))
                         (not (is-subarray? x))))
                      lines)]
    (if (first l)
      (if (is-key? (first l))
        (parse-delimited-array l is-key?)
        (if (is-item? (first l))
          (parse-item-array l)
          (parse-delimited-array l is-array?)))
      [])))

(defn take-array
  "takes array while escape is not met"
  [lines]
  (loop [l lines sa 0 ret []]
    (if (empty? l)
      ret
      (let [a (is-array? (first l))]
        (if (not a)
          (if (is-scope? (first l))
            ret
            (recur (rest l) sa (conj ret (first l))))
          (if (and
               (not (= a ""))
               (not (.startsWith a ".")))
            ret
            (if (= a "")
              (if (= sa 0)
                ret
                (recur (rest l) (- sa 1) (conj ret (first l))))
              (recur (rest l) (+ sa 1) (conj ret (first l))))))))))

(defn drop-array
  "drops until escape is met"
  [lines]
  (loop [l lines sa 0]
    (if (empty? l)
      []
      (let [a (is-array? (first l))]
        (if (not a)
          (if (is-scope? (first l))
            l
            (recur (vec (rest l)) sa))
          (if (and
               (not (= a ""))
               (not (.startsWith a ".")))
            l
            (if (= a "")
              (if (= sa 0)
                l
                (recur (vec (rest l)) (- sa 1)))
              (recur (vec (rest l)) (+ sa 1)))))))))
    
(defn process-array
  "processes a scope. Takes lines and an object.
  Returns a vector of lines and an object"
  [lines obj]
  (let [array (is-array? (first lines))
        content (take-array (rest lines))
        rlines (drop-array (rest lines))]
    (if (= "" array)
      [(rest lines) obj]
      [rlines (expand-scopes array obj (parse-array content))])))

(defn process-scope
  "processes a scope. Takes lines and an object.
  returns a vector of lines and an object"
  [lines obj]
  (let [scope (is-scope? (first lines))
        matchfn (fn [x] (and
                         (not (is-scope? x))
                         (not (is-array? x))))
        content (take-while matchfn
                            (rest lines))
        rlines (drop-while matchfn
                                 (rest lines))
        sco (get-expanded-map scope obj)]
    (if (= "" scope)
      [(rest lines) obj]
      [rlines (expand-scopes scope obj (parse-lines content sco))])))

(defn parse
  "Parses an Archieml string and returns a map"
  [x]
  (->> x
      trim-split-lines
      (take-while (fn [d] (not (is-command? d "ignore"))))
      skip
      parse-lines))
         
    
