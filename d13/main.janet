(defn is-int? [n] (= 0 (% n 1)))

(defn solve-claw [[xa ya xb yb x y]]
  (let [M (math/abs (/ (- (* x ya) (* y xa)) (- (* xb ya) (* yb xa))))
        N (/ (- x (* xb M)) xa)] [M N]))

(defn solve [input]
  (let [data (->> (string/trimr input)
                  (peg/match ~(split "\n\n" (group (some (+ (number :d+) 1))))))
        adjust (fn [[xa ya xb yb x y]]
                 [xa ya xb yb (+ x 10000000000000) (+ y 10000000000000)])
        score (fn [[a b]] (+ a (* 3 b)))
        p1 (sum (seq [nums :in data
                      :let [[a b] (solve-claw nums)]
                      :when (and (is-int? a) (is-int? b) (<= a 100) (<= b 100))]
                  (score [a b])))
        p2 (sum (seq [nums :in data
                      :let [[a b] (solve-claw (adjust nums))]
                      :when (and (is-int? a) (is-int? b))]
                  (score [a b])))]
    [p1 p2]))

(defn main [&] (->> (file/read stdin :all) solve pp))
