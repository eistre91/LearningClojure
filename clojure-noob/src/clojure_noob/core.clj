(ns clojure-noob.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn announce-treasure-location
  [{:keys [lat lng]}]
  (println (str "Treasure lat: " lat))
  (println (str "Treasure lng: " lng)))

(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}])

(defn matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
                     (set [part (matching-part part)])))))))

(defn better-symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set [part (matching-part part)])))
          []
          asym-body-parts))

(defn hit
  [asym-body-parts]
  (let [sym-parts (better-symmetrize-body-parts asym-body-parts)
        body-part-size-sum (reduce + (map :size sym-parts))
        target (rand body-part-size-sum)]
    (loop [[part & remaining] sym-parts
           accumulated-size (:size part)]
      (if (> accumulated-size target)
        part
        (recur remaining (+ accumulated-size (:size (first remaining))))))))

(map println [(str "Combine" "this")
          (vector "A" "B")
          (vector "A" 1)
          (vector "A" ["B"])
          (list 1 2)
          (list "A" 1)
          (list "A" [1])
          (hash-map :A 1 :B 2)
          (hash-map :A "A" :B 2)
          (hash-set :A "A" :B "B" :C "B")])

(defn add-100
  "Adds 100 to any number."
  [n]
  (+ 100 n))

(println (add-100 10) "\n" (add-100 100) "\n")

(defn dec-maker
  "Creates decrementing functions."
  [n]
  #(- % n))

(defn mapset
  "Applies a function to a collection and returns the result as a set."
  [f coll]
  (loop [curr coll
         result (hash-set)]
    (let [head (first curr) tail (rest curr)]
      (if (empty? curr)
        result
        (recur tail (into result
                          [(f head)]))))))

(def asym-mutant-body-parts [{:name "head" :size 3}
                             {:name "0-eye" :size 1}
                             {:name "0-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "0-shoulder" :size 3}
                             {:name "0-upper-arm" :size 3}])
(defn loop-parts
  "Takes in the string for the part name
  and returns the next part in the symmetry."
  [part, max]
  (if (nil? (re-find #"^\d+" part))
    part
    (clojure.string/replace part #"^\d+-" (str (mod (+ 1 (Integer. (re-find #"^\d+" part))) max) "-"))))

(defn matching-part
  [part, max]
  {:name (loop-parts (:name part) max)
   :size (:size part)})

(defn symmetrize-max-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts, max]
  (reduce (fn [final-body-parts part]
            (into final-body-parts
                  (set (reduce (fn [result t]
                                 (into result
                                       [(matching-part (last result) max)]))
                               [part]
                               (repeat (- max 1) "x")))))
          []
          asym-body-parts))

(defn lousy-logger
  [log-level message]
  (condp = log-level
    :warn (clojure.string/lower-case message)
    :emergency (clojure.string/upper-case message)))

(def warn (partial lousy-logger :warn))

(warn "Red light ahead")