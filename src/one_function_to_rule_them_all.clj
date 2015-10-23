(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))


(defn str-cat [a-seq]
  (if (= 0 (count a-seq))
    (str "")
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (drop 1 (reverse (reduce #(conj (conj %1 x) %2) () a-seq))))

;; or (drop 1 (interleave (repeat x) a-seq))
;; https://clojuredocs.org/clojure.core/interleave


(defn my-count [a-seq]
  (reduce #(when %2 (+ %1 1)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(conj %1 %2) '() a-seq))

(defn min-max-element-helper
  ;Takes value pair vector and comparison value
  [p-vec c-val]
  (cond
   (empty? p-vec) [c-val c-val]
   (< (first p-vec) c-val (last p-vec)) p-vec
   (< c-val (first p-vec)) [c-val (last p-vec)]
   (< (last p-vec) c-val) [(first p-vec) c-val]))

(defn min-max-element [a-seq]
  (reduce min-max-element-helper [] a-seq))


(defn insert [sorted-seq n]
  (loop [i 0
         tmp-seq sorted-seq]
    (cond
     (empty? tmp-seq) (into '() (conj tmp-seq n))
     (< n (first tmp-seq)) (concat (take i sorted-seq) (list n) (drop i sorted-seq))
     (empty? (rest tmp-seq)) (concat sorted-seq (list n))
     :else (recur (inc i) (rest tmp-seq)))))

(defn insertion-sort [a-seq]
  (reduce #(insert %1 %2) '() a-seq))

(defn parity [a-seq]
  (set
   (map first (remove #(= (mod (last %) 2) 0) (frequencies a-seq)))))


(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (my-count x))

(defn my-* [& x]
  (reduce #(* %1 %2) 1 x))


(defn pred-and
  ([] (fn [x] true))
  ([x] #(x %))
  ([x & args] #(reduce (fn [a b] (and a (b %))) (x %) args)))


;; executes first seq and calls function again with rest of the seq. This is just pure madness
(defn my-map [f & a-seq]
  (cond
    (some empty? a-seq) '()
    :else
      (cons (apply f (map first a-seq)) (apply (partial my-map f) (map rest a-seq)))))
