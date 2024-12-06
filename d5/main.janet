(def parser (peg/compile
              ~{:main (* (group (some (* :rule :s))) :s :pages)
                :rule (<- (* :d+ "|" :d+))
                :pages (group (split :s (group (split "," (number :d+)))))}))

(defn ordered? [order-t page]
  (->> (map tuple page (slice page 1))
       (all (fn [[a b]] (get order-t (string a "|" b))))))

(defn fix-order [order-t page]
  (sorted page (fn [a b]
                 (cond
                   (order-t (string a "|" b)) true
                   (order-t (string b "|" a)) false
                   (< (find-index |(= $ a) page) (find-index |(= $ b) page))))))

(defn get-middle [nums] (nums (div (length nums) 2)))

(defn solve [input]
  (let [[order pages] (peg/match parser (string/trimr input))
        order-t (from-pairs (map |[$ true] order))
        p1 (->> (filter |(ordered? order-t $) pages)
                (map get-middle)
                sum)
        p2 (->> (filter |(not (ordered? order-t $)) pages)
                (map (comp get-middle |(fix-order order-t $)))
                sum)]
    [p1 p2]))

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (->> (string/trimr (slurp "d5/ex.txt")) (peg/match parser) pp)
  (solve (slurp "d5/ex.txt"))
  (solve (slurp "d5/in.txt")))
