(ns practice-standard-functions.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})
(def c-int (comp :intelligence :attributes))

(defn attr
  [stat]
  (comp stat :attributes))

(println ((attr :intelligence) character))
(println (c-int character))

(defn prepend-operation
  [input function]
  (function input))

(defn mycomp
  [& fs]
  (let [ordered-fs (reverse fs)]
    (fn [& inputs]
      (reduce #(prepend-operation %1 %2) (apply (first ordered-fs) inputs) (rest ordered-fs)))))

(println ((comp inc /) 4 2))
(println ((mycomp inc /) 4 2))

(println ((comp str +) 8 8 8))
(println ((mycomp str +) 8 8 8))

(defn my-assoc-in
  [m [k & ks] v]
  (if (> (count ks) 0)
    (assoc m k (my-assoc-in (m k) ks v))
    (assoc m k v)))

(def users [{:name "James" :age 26}  {:name "John" :age 43}])

(println (assoc-in users [1 :password] "nhoJ"))
(println (my-assoc-in users [1 :password] "nhoJ"))

(println (assoc-in users [2] {:name "Jack" :age 19}))
(println (my-assoc-in users [2] {:name "Jack" :age 19}))

(defn my-update-in
  [m [k & ks] f & args]
  (if (> (count args) 0)
    (if (> (count ks) 0)
      (update m k #(my-update-in % ks f args))
      (let [result-f #(apply (partial f %) args)]
        (update m k result-f)))
    (if (> (count ks) 0)
      (update m k #(my-update-in % ks f))
      (update m k f))))

;the bottom is still an update
;each level up is applying a composed update

(println (update-in users [1 :age] inc))
(println (my-update-in users [1 :age] inc))

(def p {:name "James" :age 26})

(println (update-in p [:age] - 10))
(println (my-update-in p [:age] - 10))

(def name-2 "Erik")
(def movie "Interstellar")

(def practice-eval-list '(println "Erik" "Interstellar"))
(def practice-eval-list-2 (list (read-string "(prn name-2)") (read-string "(prn movie)")))
; practice-eval-list-2 gives nullpointer exception on eval
; but succeeds in printing name and movie
(def practice-eval-list-3 (read-string "(do (prn name-2) (prn movie))"))
; tbe problem with 2 is trying to do ((prn name)) which attempts to
; evaluate the return value of nil from (prn name)

(defn odd-numbers*
  ([] (odd-numbers* 1))
  ([n] (cons n (lazy-seq (odd-numbers* (+ n 2))))))

(def odd-numbers (odd-numbers*))

;(defn operation-present?
;  [infix-list op]
;  (some #(= `op %) `infix-list))

;(defn translate-op
;  [infix-list op]
;  (left-right-of-op infix-list op))

;(defn translate-if-present
;  [infix-list op]
;  (if (operation-present? infix-list op)
;    (left-right-of-op infix-list op)))

;(str "(" op " "
;     (translate-infix (take-while #(not (= % op)) string-infix-list))
;     (translate-infix (rest (drop-while #(not (= % op)) string-infix-list))) ")"))

; quote the list then stringify
; then i'm just reorganizing a string
; then read-string and eval

(require `[clojure.string :as str])
(declare translate-infix)

(defn strip-paren
  [s]
  (str/replace s #"[\(\)]" ""))

(defn translate-op
   [string-infix-list op op-pattern]
   (str "(" op " "
        (translate-infix (str/trim (get (str/split string-infix-list op-pattern 2) 0))) " "
        (translate-infix (str/trim (get (str/split string-infix-list op-pattern 2) 1))) ")"))
; could refactor with an op-pattern map so that I don't have to pass in both

(defn operation-present?
  [string-infix-list op-pattern]
  (re-find op-pattern string-infix-list))

(def length-of-base-case 18)

(defn translate-infix
  [string-infix-list]
  (if (<= (count string-infix-list) length-of-base-case)
    (if (= (count string-infix-list) 1)
      string-infix-list
      (let [exp-vector (str/split (strip-paren string-infix-list) #" ")]
        (str "(" (second exp-vector) " " (first exp-vector) " " (last exp-vector) ")")))
    (if (operation-present? string-infix-list #"clojure\.core/\+")
      (translate-op string-infix-list "clojure.core/+" #"clojure\.core/\+")
      (if (operation-present? string-infix-list #"clojure\.core/-")
        (translate-op string-infix-list "clojure.core/-" #"clojure\.core/-")
        (if (operation-present? string-infix-list #"clojure\.core/\*")
          (translate-op string-infix-list "clojure.core/*" #"clojure\.core/\*")
          (if (operation-present? string-infix-list #"clojure\.core//")
            (translate-op string-infix-list "clojure.core//" #"clojure\.core//")))))))

;(if (operation-present? string-infix-list #"clojure\.core/\*")
;  (translate-op string-infix-list "clojure.core/*" #"clojure\.core/\*")
;  (if (operation-present? string-infix-list #"clojure\.core//")
;    (translate-op string-infix-list "clojure.core//" #"clojure\.core//")
;    (if (operation-present? string-infix-list #"clojure\.core/\+")
;      (translate-op string-infix-list "clojure.core/+" #"clojure\.core/\+")
;      (if (operation-present? string-infix-list #"clojure\.core/-")
;        (translate-op string-infix-list "clojure.core/-" #"clojure\.core/-")))))

(defn begin-translation
  [infix-list]
  (translate-infix (strip-paren (str infix-list))))

; some way to shorten this?
; thing is avoiding calls which translate on multiple ops
; maybe the reduced function works for this
; if the find succeeds, reduced return
; else no return but the reduce will continue

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
