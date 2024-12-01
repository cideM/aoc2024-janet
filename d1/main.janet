(def p1-peg (peg/compile
              ~{:main (* (some :line) -1)
                :line (* :num-pair (? :s))
                :num (/ (<- :d+) ,scan-number)
                :num-pair (group (* :num :s+ :num))}))

(assert (deep= (peg/match p1-peg "1 2") @[@[1 2]]))
(assert (deep= (peg/match p1-peg "10 22") @[@[10 22]]))
(assert (deep= (peg/match p1-peg "1 2\n4 5") @[@[1 2] @[4 5]]))

(defn solve [input]
  (let [parsed (peg/match p1-peg input)
        left (sort (map first parsed))
        right (sort (map last parsed))
        p1 (+ ;(map |(math/abs (- $ $1)) left right))
        freq (frequencies right)
        p2 (+ ;(map |(* $ (or ($ freq) 0)) left))]
    [p1 p2]))

(defn main [& args] (let [filepath (1 args)
                          input (slurp filepath)]
                      (pp (solve input))))

(solve (slurp "d1/ex.txt"))
(solve (slurp "d1/in.txt"))

