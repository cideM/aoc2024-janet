(defn walk [grid [x y] [dx dy] times]
  (string ;(seq [n :range-to [0 times]]
             (get-in grid [(+ y (* dy n)) (+ x (* dx n))]))))

(defn solve [input]
  (let [vec-p1 [[-1 -1] [0 -1] [1 -1] [1 0] [1 1] [0 1] [-1 1] [-1 0]]
        vec-p2 [[-1 -1] [1 -1] [1 1] [-1 1]]
        pattern-p2 {"AMASASAM" true "AMAMASAS" true "ASAMAMAS" true "ASASAMAM" true}
        g (peg/match ~(split "\n" (group (some ':w))) (string/trimr input))
        p1 (+ ;(seq [y :range [0 (length g)]
                     x :range [0 (length (g y))]
                     word :in (map |(walk g p $ 3) vec-p1)
                     :when (= "XMAS" word)] 1))
        p2 (+ ;(seq [y :range [0 (length g)] x :range [0 (length (g y))]
                     word :in (map |(walk g p $ 1) vec-p2)
                     :when (count pattern-p2 word)] 1))]
    [p1 p2]))

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (def g (->> (slurp "d4/in.txt") string/trimr (peg/match parser)))
  (walk g [0 0] [1 0] 3)
  (words g [0 0])
  (solve (slurp "d4/ex.txt"))
  (solve (slurp "d4/in.txt")))
