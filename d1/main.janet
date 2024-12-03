(def parser (peg/compile ~(split "\n" (group (split :s (any (number :d+)))))))

(defn total-distance [xs1 xs2] (+ ;(map (comp math/abs (partial -)) xs1 xs2)))

(defn sim-score [xs1 xs2]
  (let [freq (frequencies xs2)]
    (+ ;(map |(* $ (get freq $ 0)) xs1))))

(defn solve [input]
  (let [parsed (peg/match parser (string/trimr input))
        left (sort (map first parsed))
        right (sort (map last parsed))]
    [(total-distance left right) (sim-score left right)]))

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (solve (slurp "d1/ex.txt"))
  (solve (slurp "d1/in.txt")))

(assert (deep= (sim-score [1] []) 0))
(assert (deep= (sim-score [1] [1]) 1))
(assert (deep= (sim-score [2] [1 2 3 2]) 4))
(assert (deep= (sim-score [5 3 2] [1 2 3 2]) 7))

(assert (deep= (total-distance [] []) 0))
(assert (deep= (total-distance [1] []) 0))
(assert (deep= (total-distance [1] [1]) 0))
(assert (deep= (total-distance [2 3] [1 5]) 3))

(assert (deep= (peg/match parser "1 2") @[@[1 2]]))
(assert (deep= (peg/match parser "10 22") @[@[10 22]]))
(assert (deep= (peg/match parser "1 2\n4 5") @[@[1 2] @[4 5]]))
