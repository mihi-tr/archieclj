(ns archieclj.core)
(require '[clojure.string :as string])



(declare process-scope)
(declare process-array)

(defn split-lines
  "Splits a text file to multiple lines"
  [x]
  (map string/trim
       (string/split x #"\n")))

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
  (let [tokens [is-command? is-key? is-array? is-scope? is-item?]]
    (boolean (reduce (fn [r t] (or (t x) r)) false tokens))))

(defn expand-scopes
  "expands the scopes for scoped tokens and sets the value"
  [t obj value]
  (let [s (string/split t #"\." 2)
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

(defn parse-key-array
  "takes lines and returns an array of objects"
  [lines]
  []
  )

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
                         (not (is-item? x))))
                      lines)]
    (if (is-key? (first l))
      (parse-key-array l)
      (parse-item-array l))))


(defn process-array
  "processes a scope. Takes lines and an object.
  Returns a vector of lines and an object"
  [lines obj]
  (let [array (is-array? (first lines))
        matchfn (fn [x] (and
                         (not (is-scope? x))
                         (not (is-array? x))))
        content (take-while matchfn
                            (rest lines))
        rlines (drop-while matchfn
                           (rest lines))]
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
      split-lines
      (take-while (fn [d] (not (is-command? d "ignore"))))
      skip
      parse-lines))
         
    
