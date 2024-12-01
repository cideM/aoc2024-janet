(def p1-peg (peg/compile
              ~{:main (* (some :line) -1)
                :line (* :num-pair (? :s))
                :num (/ (<- :d+) ,scan-number)
                :num-pair (group (* :num :s+ :num))}))

(assert (deep= (peg/match p1-peg "1 2") @[@[1 2]]))
(assert (deep= (peg/match p1-peg "10 22") @[@[10 22]]))
(assert (deep= (peg/match p1-peg "1 2\n4 5") @[@[1 2] @[4 5]]))

(defn total-distance [list1 list2]
  (+ ;(map |(math/abs (- $ $1)) list1 list2)))

(assert (deep= (total-distance [] []) 0))
(assert (deep= (total-distance [1] []) 0))
(assert (deep= (total-distance [1] [1]) 0))
(assert (deep= (total-distance [2 3] [1 5]) 3))

(defn sim-score [list1 list2]
  (let [freq (frequencies list2)]
    (+ ;(map |(* $ (get freq $ 0)) list1))))

(assert (deep= (sim-score [1] []) 0))
(assert (deep= (sim-score [1] [1]) 1))
(assert (deep= (sim-score [2] [1 2 3 2]) 4))
(assert (deep= (sim-score [5 3 2] [1 2 3 2]) 7))

(defn solve [input]
  (let [parsed (peg/match p1-peg input)
        left (sort (map first parsed))
        right (sort (map last parsed))]
    [(total-distance left right) (sim-score left right)]))

(comment
  (solve (slurp "d1/ex.txt"))
  (solve (slurp "d1/in.txt")))

(defn main [&]
  (->> (file/read stdin :all)
       solve
       pp))
