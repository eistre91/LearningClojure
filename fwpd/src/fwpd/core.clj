(ns fwpd.core)

(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name          identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\r\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen \" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(defn glitter-filter-names
  [minimum-glitter records]
  (map :name (glitter-filter minimum-glitter records)))

(defn append
  "Adds a new vampire record."
  [name glitter-index records]
  (conj records {:name name :glitter-index glitter-index}))

(defn extract-validator
  "Pulls a validator to apply for a validator map."
  [key validators]
  (key validator))

(defn validate
  "Validates a vampire record based on provided validator function for each key"
  [validators record]
  (every? identity (map (fn [key]
                          ((extract-validator key validators) record)) vamp-keys)))

;(defn validate
;  "Validates a vampire record based on provided validator function for each key"
;  [validators record]
;  (every? identity (map (fn [key]
;                          (#(contains? % key) record)) vamp-keys)))


(def default-validators {:name #(contains? % :name) :glitter-index #(contains? % :glitter-index)})

(defn map-to-csv
  "Takes the map back to a csv string."
  [records]
  (reduce (fn [result record]
            (str result (clojure.string/join "," [(:name record) (:glitter-index record)]) "\n"))
          "" records))