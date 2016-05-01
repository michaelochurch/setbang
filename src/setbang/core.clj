(ns setbang.core
  (:require [clojure.set :as clj-set]))

;; TODO: FIX THIS
;; Set = sequence of includes and excludes...
;; ... possibly finite.
;; ... treat integers as ur-elements?

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
        :else (map (fn [[_ e]] (normalize e)) x)))

(def omega (map #(vector :include %) (range)))

(def loud-empty (map #(vector :exclude %) (range)))

(def bottom (lazy-seq (loop [] (recur))))

;; we should be able to show "there are at least 20 primes"
;; e.g. (primes)#(20)?

(defn force-set [x]
  (cond (number? x) x
        (set?    x) x
        :else (into #{}
                    (mapcat (fn [[inc-or-exc v]]
                              (when (= inc-or-exc :include) [v])) x))))

(declare member-check)

(defn equality-check [x1 x2]
  (let [check-diff (fn [[inc-or-exc elt2] elt1]
                     (if (= inc-or-exc :exclude)
                       (member-check elt1 elt2)
                       (not (member-check elt1 elt2))))]
    (cond
     (identical? x1 x2) true
     (and (or (set? x1) (number? x1))
          (or (set? x2) (number? x2))) (= x1 x2)
     (or (set? x1) (number? x1))
       (and (not (some #(check-diff % x1) x2))
            (= x1 (normalize (force-set x2))))
     (or (set? x2) (number? x2))
       (and (not (some #(check-diff % x2) x1))
            (= x2 (normalize (force-set x1))))

     :else
       (loop [i1 #{} e1 #{} i2 #{} e2 #{} ctr 0 remt1 x1 remt2 x2]
         (if (and (empty? remt1) (empty? remt2))
           (equality-check (normalize i1) (normalize i2))
           (if (even? ctr)
             (let [[c v] (first remt1)]
               (cond (and (= c :include) (some #(equality-check % v) e2))
                       false
                     (and (= c :exclude) (some #(equality-check % v) i2))
                       false 
                     (= c :include) (recur (conj i1 v) e1 i2 e2 (inc ctr) (next remt1) remt2)
                     (= c :exclude) (recur i1 (conj e1 v) i2 e2 (inc ctr) (next remt1) remt2)
                     (= c nil)      (recur i1 e1 i2 e2 (inc ctr) (next remt1) remt2)))
             (let [[c v] (first remt2)]
               (cond (and (= c :include) (some #(equality-check % v) e1))
                       false
                     (and (= c :exclude) (some #(equality-check % v) i1))
                       false 
                     (= c :include) (recur i1 e1 (conj i1 v) e2 (inc ctr) remt1 (next remt2))
                     (= c :exclude) (recur i1 e1 i2 (conj e2 v) (inc ctr) remt1 (next remt2))
                     (= c nil)      (recur i1 e1 i2 e2 (inc ctr) remt1 (next remt2))))))))))

(defn member-check [x e]
  (cond
   (number? x) (and (number? e) (< e x))
   (set?    x)
     (some #(equality-check % e) x)
   :else
     (->> x
          (filter #(equality-check (second %) e))
          ffirst
          (= :include))))

;; (defn int-to-set [n]
;;   (loop [acc #{}, i 0]
;;     (if (>= i n) acc (recur (conj acc acc) (inc i)))))

;; (defn equality-1 [n s]
;;   (cond

;; (defn gen-count [x]
;;   (cond (number? x) x
;;         (set?    x) (count x)
;;         :else       Long/MAX_VALUE))

;; (defn normalize [x]
;;   (if (number? x) x
;;       (let [s (sort-by gen-count (map normalize x))]
;;         (if (= s (range (count s)))
;;           (count s)
;;           (into #{} s)))))

;; (defn normalize [x]
;;   (if (number? x) x
;;       (let [s (into #{} (map normalize x))
;;             n (count s)]
;;         (if (= s (into #{} (range n))) n s))))
;; ;;      (map normalize n

;; (defn mem-check [s e]
;;   )

;; (defn eq-check [s1 s2]
;;   (cond
;;    (number? s1) 






;; Sets can be represented in 3 ways.
;; integer : N
;; set     : #{...}
;; lazy    : {:sequence (...) lazy
;;            :includes #{...}
;;            :excludes #{...}
;;            :include? <>    fn -> bool}

;; (defn non-termination []
;;   (throw (Exception. "Non-termination detected.")))

(defn int-as-set [n]
  (into #{} (range n)))

(defn vec-drop [k v]
  (subvec v 0 (- (count v) k)))

(defn to-int [x]
  (cond (integer? x) x
        (set?     x)
          (let [n (count x)] (when (= x (set (range n))) n))
        :else (non-termination)))

(defn convert-if-int [x]
  (or (to-int x) x))

(defn left-pad [size stack]
  (into [] (concat (repeat (- size (count stack)) 0) stack)))

;; (defn lazy-set-choice [{:keys [sequence includes excludes include?] :as x}]
;;   (if (empty? includes)
;;     (let [reject-fn #(or (not (include? %)) (excludes %))
;;           [rejects seq1] (split-with reject-fn sequence)
;;           choice (first seq1)]
;;       [{:sequence (rest seq1), :include? include?
;;         :includes #{}
;;         :excludes (clj-set/union excludes (set rejects) #{choice})}
;;        choice])
;;     (let [choice (first includes)]
;;       [(-> x (update :includes disj choice)
;;            (update :excludes conj choice))
;;        choice])))

;; (defn lazy-set-seq [lazy-set]
;;   (lazy-seq
;;    (let [[remt choice] (lazy-set-choice lazy-set)]
;;      (cons choice (lazy-set-seq remt)))))

;; (defn choice [x]
;;   (cond (integer? x) (do-choice (int-as-set x))
;;         (set?     x) (let [c (first x)] [(disj x c) c])
;;         :else (lazy-set-choice x)))
                
;; (defn lazy-set-insert [x e]
;;   (-> (update x :includes conj e)
;;       (update :excludes disj e)))

;; ;; insert has a bug. lazy-set-insert has something wrong with it.
;; ;; Also, the :includes and :excludes have rely on set equality. 
;; (defn insert [x e]
;;   (cond (integer? x)
;;           (when-let [n (to-int e)]
;;             (cond (= n x) (inc x)
;;                   (< n x) x
;;                   :else (conj (int-as-set x) e)))
;;         (set? x) (conj x e)
;;         :else (lazy-set-insert x e)))


;; (defn pair [x y]
;;   (let [s (hash-set x y)]
;;     (cond (= s #{0}) 1
;;           (= s #{0 1}) 2
;;           :else s)))

;; (defn set-inc [x]
;;   (cond (integer? x) (+ x 1)
;;         (set? x)     (conj x x)
;;         :else        (update x :strict conj x)))

;; (def force-number 1024)

;; (def omega {:excludes #{}, :includes #{}, :include? (constantly true), :sequence (range)})

;; ;; Stack is irrelevant for nats (it's a constant).
;; (def nats {:strict (set (range force-number)) :lazy (iterate inc force-number) :stack nil})

(defn card [x & [stack]]
  (cond (integer? x) x
        (set? x)     (count x)
        :else        (let [{:keys [strict lazy stack]} x
                           n (count strict)]
                       {:strict (set (range n)) :lazy (iterate inc n) :stack stack})))

(defn select-by-bits [n coll]
  (into #{}
        (filter identity
                (map-indexed (fn [i x] (when (bit-test n i) x)) coll))))

(defn power-set-seq [coll]
  (let [limit 40 ;; We'll never generate more than 2^40 things.
        coll' (take limit coll)]
    (map #(select-by-bits % coll') (range 0 (bit-shift-left 1 (count coll'))))))

;; (defn power-set [x]
;;   (cond (integer? x) (

;; (defn set-to-seq [x]
;;   (cond (number? x) (range x)
;;         (set? x)    (seq x)
;;         :else (let [{:keys [strict lazy]} x]
;;                 (concat (seq strict) lazy))))

;; (declare equality-check)
;; (defn member-check [x e]
;;   (cond (number? x) (> x (to-int e))
;;         (set? x)    (some #(equality-check % e) x)
;;         :else       (some #(equality-check % e) (set-to-seq x))))

;; (defn equality-check [x1 x2]
;;   (if (and (number? x1) (number? x2)) (= x1 x2)
;;       (let [y1 (if (number? x1) (int-as-set x1) x1)
;;             y2 (if (number? x2) (int-as-set x2) x2)]
;;         (cond (and (set? y1) (set? y2)) (= y1 y2)
;;               (set? y1) (not (some #(not (member-check y1 %)) (set-to-seq y2)))
;;               (set? y2) (not (some #(not (member-check y2 %)) (set-to-seq y1)))
;;               :else (if (identical? y1 y2) true (non-termination))))))

;; (defn set-intersect [x1 x2]
;;   (cond (and (number? x1) (number? x2)) (min x1 x2)
;;         (let [y1 (if (number? x1) (int-as-set x1) x1)
;;               y2 (if (number? x2) (int-as-set x2) x2)]
;;           (cond (and (set? x1) (set? x2) 
        

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
