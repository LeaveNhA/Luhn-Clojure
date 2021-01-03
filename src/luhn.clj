(ns luhn)

(defn- abs [n]
  (max n (- n)))

(defn valid? [number]
  (let [digit-limit 9
        valid-character-limit 2

        mod-by-10 #(mod % 10)
        clear-and-split-numbers #(->> % clojure.string/trim
                                     (re-seq #"\d"))
        index-numbers (partial map-indexed (fn [i e] [(inc i) e]))
        group-numbers (partial group-by (comp even? first))
        trim-numbers (partial map (comp (partial map second) second))
        ->numbers (partial map (partial map #(Integer/parseUnsignedInt %)))
        double-numbers #(update % 1 (partial map (comp (partial * 2))))
        digitize (partial map (partial map (comp #(if (> % digit-limit)
                                     (- % digit-limit)
                                     %))))]
    (if (or (re-find #"[^0-9 ]" number)
            (< (count (re-seq #"\d" number)) valid-character-limit))
      false
      (->> number
           clear-and-split-numbers
           reverse
           index-numbers
           group-numbers
           sort
           trim-numbers
           ->numbers
           vec
           double-numbers
           digitize
           flatten
           (apply +)
           mod-by-10
           zero?))))
