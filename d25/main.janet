(def parser (peg/compile
              ~{:main (any (* (group :grid) (? "\n")))
                :line (repeat 5 '(+ "#" "."))
                :grid (repeat 7 (* (group :line) (? "\n")))}))

(defn height [grid]
  (seq [x :range [0 (length (first grid))]]
    (dec (sum (seq [y :range [0 (length grid)]]
                (if (= "#" (get-in grid [y x])) 1 0))))))

(defn match [heights-1 heights-2]
  (every? (map |(<= (+ $ $1) 5) heights-1 heights-2)))

(defn solve [input]
  (let [data (peg/match parser (string/trimr input))
        grouped (group-by |(deep= (first $) @["#" "#" "#" "#" "#"]) data)
        {true locks false keys_} grouped]
    (count true? (seq [lock :in locks k :in keys_]
                   (match (height lock) (height k))))))

(defn main [&] (->> (file/read stdin :all) solve pp))
