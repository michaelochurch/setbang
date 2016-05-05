(ns setbang.core
  (:require [clojure.java.io :as io]
            [clojure.set :as clj-set]
            [clojure.string :as clj-string])
  (:gen-class))

;; Only exists for testing. Generates a non-normalized integer-equivalent set.
(defn int-to-set [n]
  (loop [i 0 acc #{}]
    (if (>= i n) acc (recur (inc i) (conj acc acc)))))

(defn normalize [x]
  (cond (number? x) x
        (set? x)
          (let [s (set (map normalize x))]
            (if (= s (into #{} (range (count x))))
              (count x)
              s))
        :else (map normalize x)))

(defn is-mumber? [x]
  (number? (normalize x)))

(defn as-set [x]
  (cond
   (number? x) (into #{} (range x))
   (set?    x) x
   :else       (into #{} x)))

(def omega (range))

;; bottom : extreme pathological set. Empty but yields no information. Only used
;; for testing.
(def bottom (lazy-seq (loop [] (recur))))

;; we should be able to show "there are at least 20 primes"
;; e.g. (primes)#(20)?


(declare member-check)

(defn equality-check [x1 x2]
  (if (and (number? x1) (number? x2))
    (= x1 x2)
    (let [s1 (if (number? x1) (into #{} (range x1)) x1)
          s2 (if (number? x2) (into #{} (range x2)) x2)]
      (if (set? s1)
        (and (every? #(member-check s1 %) s2)
             (every? #(member-check s2 %) s1))
        (and (every? #(member-check s2 %) s1)
             (every? #(member-check s1 %) s2))))))

(defn member-check [x e]
  (cond (number? x) (and (number? e) (< e x))
        (set? x)    (if (or (number? e) (set? e)) (x e)
                        (some #(equality-check % e) x))
        :else       (some #(equality-check % e) x)))

(defn pair [x1 x2]
  (normalize (hash-set x1 x2)))

(defn choice [x]
  (cond
   (= x 0) [0 0]
   (number? x) (choice (into #{} (range x)))
   (set? x) (let [elt (first x)
                  remt (if (empty? (rest x)) 0 (into #{} (rest x)))]
              [(normalize remt) elt])
   :else (if (empty? x)
           [0 0]
           (let [elt (first x)]
             [(rest x) elt]))))

(defn is-empty-set [x]
  (cond
   (number? x) (= x 0)
   (set?    x) (empty? x)
   :else       (empty? x)))

(defn choose-many [x n]
  (let [s (if (number? x) (into #{} (range x)) x)]
    (loop [remt s out #{} i 0]
      (if (or (>= i n) (is-empty-set remt))
        [(normalize remt) (normalize out)]
        (let [[remt-1 e] (choice remt)]
          (recur remt-1 (conj out e) (inc i)))))))

;; FIXME: insert should reorder inf. sets to avoid long equality-checks.
(defn insert [x e]
  (cond
   (and (number? x) (number? e) (< e x)) x
   (and (number? x) (number? e) (= e x)) (+ x 1)
   (or (number? x) (set? x))
     (let [s (if (number? x) (into #{} (range x)) x)]
       (normalize (conj s e)))
   :else (cons e (remove #(equality-check e (second %)) x))))

(defn has-definite-size? [x]
  (or (number? x) (set? x)))

;; Non-terminating on an infinite set.
(defn get-size-up-to [x n]
  (cond (number? x) x
        (set?    x) (count x)
        :else       (count (take n x))))

(defn gen-union-2 [x1 x2]
  (cond
   (and (number? x1) (number? x2)) (max x1 x2)
   (and (has-definite-size? x1) (has-definite-size? x2))
     (normalize (clj-set/union (as-set x1) (as-set x2)))
     :else
       (if (is-empty-set x1)
         x2
         (lazy-seq
          (let [[x' e] (choice x1)]
            (cons e (remove #(equality-check % e) (gen-union-2 x2 x'))))))))

(defn as-seq [x]
  (cond
   (number? x) (range x)
   (set? x)    (seq x)
   :else       x))

;; [x1 x2 x3 ...] [y1 y2 y3 ... ] [z1 z2 z3 ...] [w1 ...] ->
;; x1 x2 y1 x3 y2 z1 x4 y3 z2 w1 ...
;; "diagonal" approach to make sure everything gets picked.
(defn stagger [xs & [n]]
  (if (empty? xs) ()
      (let [n (or n 1)
            choices (map choice (take n xs))]
        (concat (map second choices)
                (stagger (concat (remove is-empty-set (map first choices))
                                 (drop n xs))
                         (inc n))))))

(defn staggered-union [xs]
  (lazy-seq
   (let [ys (stagger xs)
         y1 (first ys)]
     (cons y1 (remove #(equality-check % y1) ys)))))

(defn gen-union [x-set]
  (cond
   (number? x-set) x-set
   (set?    x-set) (reduce gen-union-2 #{} x-set)
   :else (staggered-union x-set)))

(defn gen-filter [f x]
  (cond
   (number? x) (normalize (into #{} (filter f (range x))))
   (set?    x) (normalize (into #{} (filter f x)))
   :else       (filter f x)))

(defn gen-intersect-2 [x1 x2]
  (cond
   (and (number? x1) (number? x2)) (min x1 x2)
   (has-definite-size? x1) (gen-filter #(member-check x2 %) x1)
   (has-definite-size? x2) (gen-filter #(member-check x1 %) x2)
   :else (gen-filter #(member-check x1 %) x2)))

(defn difference-2 [x1 x2]
  (gen-filter #(not (member-check x2 %)) x1))

(defn exclusive-or-2 [x1 x2]
  (gen-union-2 (difference-2 x1 x2) (difference-2 x2 x1)))

(defn gen-union-map [f x]
  (cond
   (number? x) (gen-union (into #{} (map f (range x))))
   (set?    x) (gen-union (into #{} (map f x)))
   :else       (gen-union (map f x))))

(defn card [x]
  (cond (integer? x) x
        (set? x)     (count x)
        :else (normalize (map-indexed (fn [i e] i) x))))

(defn select-by-bits [n coll]
  (into #{}
        (filter identity
                (map-indexed (fn [i x] (when (bit-test n i) x)) coll))))

(defn power-set-seq [coll]
  (let [limit 60 ;; We'll never generate more than 2^60 things.
        coll' (take limit coll)]
    (map #(normalize (select-by-bits % coll')) (range 0 (bit-shift-left 1 (count coll'))))))

(def large-set-cutoff 100000)

(defn shrink-if-possible [x]
  (cond (number? x) x
        (set?    x) x
        :else (let [z (take (inc large-set-cutoff) x)]
                (if (<= (count z) large-set-cutoff)
                  (normalize (into #{} z))
                  x))))

(defn power-set [x]
  (let [n (get-size-up-to x 17)
        s (if (number? x) (range x) x)]
    (if (> n 16)
      (power-set-seq s)
      (into #{} (power-set-seq s)))))

(defn ordered-pair [x y]
  (normalize (hash-set (hash-set x) (hash-set x y))))

(defn ordered-pair-destructure [x]
  (let [s (if (number? x) (into #{} (range x)) x)]
    (cond
     (empty? s) [0 0]
     (= (count s) 1) [(first s) (first s)]
     :else (let [[v w] (take 2 s)]
             [(first (gen-intersect-2 v w))
              (first (exclusive-or-2 v w))]))))

(defn left-pad [size stack]
  (into [] (concat (repeat (- size (count stack)) 0) stack)))

(defn stack-fn [f arity]
  (fn [stack]
    (let [stack0 (if (> arity (count stack)) (left-pad arity stack) stack)
          idx (- (count stack0) arity)
          args (subvec stack0 idx)
          out (apply f args)]
      (if (vector? out)
        (into (subvec stack 0 idx) out)
        (conj (subvec stack 0 idx) out)))))

(defn rotate [stack direction]
  (let [k (card (last stack))
        stack0 (if (>= k (count stack))
                 (left-pad k (butlast stack))
                 (subvec stack 0 (dec (count stack))))
        idx (- (count stack0) k)]
    (cond
     (= direction :right)
       (into (conj (subvec stack0 0 idx) (last stack0))
             (subvec stack0 idx (- (count stack0) 1)))
     (= direction :left)
       (into (subvec stack0 0 idx)
             (conj (subvec stack0 (inc idx)) (nth stack0 idx))))))

(defn read-1 [stack]
  (let [c (.read *in*)]
    (conj stack c)))

(defn write-1 [ch]
  (.write *out* (card ch))
  [])

(declare run-code)

(defn conditionally-run-code [stack code]
  (let [[then-code else-code] (clj-string/split code #",")
        else-code (or else-code "")
        bool (is-empty-set (last stack))]
    (if bool
      (run-code stack else-code)
      (run-code stack then-code))))

(defn loop-with-code [stack code]
  (loop [st stack]
    (if (is-empty-set (last st))
      st
      (recur (run-code st code)))))

;; Do the union. This is the last part.
(defn set-comprehension [stack code]
  (let [x       (last stack)
        stack-1 (into [] (butlast stack))
        s       (if (number? x) (into #{} (range x)) x)
        f       (fn [e] (last (run-code (conj stack-1 e) code)))]
    (conj stack-1 (shrink-if-possible (gen-union (map f s))))))

(defn run-token [stack token]
  (if (string? token)
    (cond
      (= (.charAt token 0) \()
        (conditionally-run-code stack (.substring token 1 (dec (count token))))
      (= (.charAt token 0) \[)
        (loop-with-code stack (.substring token 1 (dec (count token))))
      (= (.charAt token 0) \{)
        (set-comprehension stack (.substring token 1 (dec (count token)))))
    (case token
      \space stack
      \newline stack
      \return stack
      \0 (conj stack 0)
      \1 (conj stack 1)
      \2 (conj stack 2)
      \3 (conj stack 3)
      \4 (conj stack 4)
      \5 (conj stack 5)
      \6 (conj stack 6)
      \7 (conj stack 7)
      \8 (conj stack 8)
      \9 (conj stack 9)
      \\ ((stack-fn choice 1) stack)
      \/ ((stack-fn insert 2) stack)
      \+ ((stack-fn pair 2) stack)
      \_ ((stack-fn (fn [_] []) 1) stack)
      \~ ((stack-fn (fn [x] [x x]) 1) stack)
      \' ((stack-fn (fn [x] (insert x x)) 1) stack)
      \# ((stack-fn card 1) stack)
      \^ ((stack-fn power-set 1) stack)
      \= ((stack-fn (fn [x y] (if (equality-check x y) 1 0)) 2) stack)
      \? ((stack-fn (fn [x e] (if (member-check x e) 1 0)) 2) stack)
      \> (rotate stack :right)
      \< (rotate stack :left)
      \$ (conj stack omega)
      \` ((stack-fn choose-many 2) stack)
      \& ((stack-fn (fn [x y] (gen-intersect-2 x y)) 2) stack)
      \| ((stack-fn (fn [x y] (gen-union-2 x y)) 2) stack)
      \- ((stack-fn (fn [x y] (difference-2 x y)) 2) stack)
      \. ((stack-fn (fn [x y] (exclusive-or-2 x y)) 2) stack)
      \" ((stack-fn (fn [x] (normalize #{x})) 1) stack)
      \; ((stack-fn (fn [x y] y) 2) stack)
      \% ((stack-fn ordered-pair 2) stack)
      \* ((stack-fn ordered-pair-destructure 1) stack)
      \! ((stack-fn write-1 1) stack)
      \@ (read-1 stack)
      stack)))

(def single-char-token (set "~`!@#$%^&*0-_+=|\\;\"'<>,.?/ \n"))

(defn parse-chunk [code-string idx]
  (let [end (count code-string)
        [open close] (case (.charAt code-string idx)
                       \[ [\[ \]]
                       \{ [\{ \}]
                       \( [\( \)])]
    (loop [cnt 1 i (inc idx)]
      (cond (= cnt 0)  [(.substring code-string idx i) i]
            (>= i end) (throw (Exception. "parsing error"))
            :else (condp = (.charAt code-string i)
                    open  (recur (inc cnt) (inc i))
                    close (recur (dec cnt) (inc i))
                    (recur cnt (inc i)))))))

(defn parse-token [code-string & [idx]]
  (let [idx (or idx 0)]
    (if (>= idx (count code-string))
      nil
      (let [c (.charAt code-string idx)]
        (cond (= c \[) (parse-chunk code-string idx)
              (= c \() (parse-chunk code-string idx)
              (= c \{) (parse-chunk code-string idx)
              :else [c (+ idx 1)])))))

(defn run-code [stack code]
  (loop [stack-state stack idx 0]
    (if-let [[tok idx-1] (parse-token code idx)]
      (recur (run-token stack-state tok) idx-1)
      stack-state)))

(defn set-to-string [x numeric?]
  (if numeric?
    (if (number? x)
      x
      (format "{%s}" (clj-string/join ", " (map #(set-to-string % true) x))))
    (let [s (if (number? x) (into #{} (range x)) x)]
      (apply str (concat '("{") (map #(set-to-string % false) s) '("}"))))))

(defn stack-to-string [x numeric?]
  (apply str "Stack: " (interpose " " (map #(set-to-string % numeric?) x))))

(defn generate-test-case [n & [temp]]
  (let [temp (or temp (inc n))]
    (if (< (rand) (/ 1.0 temp))
      n
      (let [k (rand-int n)]
        (normalize
         (hash-set (generate-test-case k (rand-int k))
                   (generate-test-case (- n k) (rand-int (- n k)))))))))

(def alphanum (set "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))

(defn expand-macros [code macros]
  (loop [c code]
    (let [c1 (reduce (fn [c0 [k v]] (clj-string/replace c0 k v)) c macros)]
      (if (= c1 c) c1 (recur c1)))))

(defn test-code [code1 code2 macros n-trials stack-size]
  (loop [i 0]
    (if (>= i n-trials)
      (do
        (println " All tests passed.")
        true)
      (let [stack (into [] (repeatedly stack-size #(generate-test-case i)))
            code1-m (expand-macros code1 macros)
            code2-m (expand-macros code2 macros)
            result1 (run-code stack code1-m)
            result2 (run-code stack code2-m)]
        (if (and (= (count result1) (count result2))
                 (every? identity (map equality-check result1 result2)))
          (do
            (print \.)
            (recur (inc i)))
          (do
            (println)
            (printf "Test #%d FAILED!\n" i)
            (println)
            (printf "Stack was: %s\n" (stack-to-string stack true))
            (printf "Program #1: %s\nResult #1 %s\n\n" code1-m
                    (stack-to-string result1 true))
            (printf "Program #2: %s\nResult #2 %s\n\n" code2-m
                    (stack-to-string result2 true))))))))

(declare setbang-file)
(defn handle-directive [line stack macros config]
  (cond (clj-string/starts-with? line ":comment")
          [stack macros config]

        (clj-string/starts-with? line ":macro")
          (let [[_ macro-name code] (clj-string/split line #"\s" 3)]
            [stack (assoc macros (str ":" macro-name ":") code) config])

        (clj-string/starts-with? line ":numeric")
          (let [[_ on-off] (clj-string/split line #"\s" 3)]
            (cond (= on-off "off") [stack macros (assoc config :numeric false)]
                  (= on-off "on")  [stack macros (assoc config :numeric true)]
                  :else (println "Warning: numeric directive only takes on or off.")))

        (clj-string/starts-with? line ":verbose")
          (let [[_ on-off] (clj-string/split line #"\s" 3)]
            (cond (= on-off "off") [stack macros (assoc config :verbose false)]
                  (= on-off "on")  [stack macros (assoc config :verbose true)]
                  :else (println "Warning: numeric directive only takes on or off.")))

        
        (clj-string/starts-with? line ":test")
          (let [[_ code1 code2] (clj-string/split line #"\s")
                code2 (or code2 "")]
            (test-code code1 code2 macros 15 4)
            [stack macros config])

        (clj-string/starts-with? line ":exec")
        (let [[_ filename] (clj-string/split line #"\s")]
          (setbang-file filename stack macros config))
        
        (clj-string/starts-with? line ":quit")
          (throw (Exception. "user quit"))
        :else nil))

(defn parse-line [line stack macros config]
  (if (empty? line)
    [stack macros config]
    (if-let [result (and (= (.charAt line 0) \:)
                         (handle-directive line stack macros config))]
      result
      (let [code (expand-macros line macros)]
        (when (:verbose config) (println (apply str (remove #{\space} code))))
        [(run-code stack code) macros config]))))

(defn setbang-repl []
  (loop [stack [] macros {} config {:numeric true}]
    (println (stack-to-string stack (:numeric config)))
    (print "S∈tBang> ")
    (flush)
    (let [line (read-line)
          [stack-1 macros-1 config-1] (parse-line line stack macros config)]
      (recur stack-1 macros-1 config-1))))

(defn setbang-file [filename & [stack macros config]]
  (let [stack  (or stack [])
        macros (or macros {})
        config (or config {})
        rdr (io/reader filename)]
    (loop [s stack m macros c config remt (line-seq rdr)]
      (if (empty? remt)
        [s m c]
        (let [[s1 m1 c1] (parse-line (first remt) s m c)]
          (recur s1 m1 c1 (next remt)))))))

(defn -main []
  (setbang-repl))
