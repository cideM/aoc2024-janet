(def parser (peg/compile ~(split :s (any (group (* (line) (column) '1))))))

(defn gen-antinodes
  ``
	Generate a list of [y x] tuple antinodes for the given antennas. Takes
	two named parameters:
	  * `:len` determines how many antinodes to generate per antenna
	  * `:include-self` determines if the antenna itself is included in
	    the list of antinodes
  ``
  [antennas &named len include-self]
  (default len 1)
  (let [from (if include-self 0 1)
        antinodes (seq [[y x ch] :in antennas
                        [y2 x2 ch2] :in antennas
                        n :range-to [from len]
                        :let [[dy dx] [(- y2 y) (- x2 x)]]
                        :when (and (= ch ch2) (not= x x2) (not= y y2))]
                    [(+ y2 (* n dy)) (+ x2 (* n dx))])]
    antinodes))

(defn solve [input]
  (let [g (peg/match parser (string/trimr input))
        max-x (max ;(map (fn [[_ x _]] x) g))
        max-y (max ;(map first g))
        in-grid? (fn [[y x &opt]]
                   (and (>= y 1) (<= y max-y) (>= x 1) (<= x max-x)))
        antennas (filter (fn [[_ _ ch]] (not= ch ".")) g)
        antinodes (gen-antinodes antennas) antinodes-p2 (gen-antinodes antennas
                                    :len (max max-x max-y)
                                    :include-self true)
        p1 (->> (distinct antinodes) (filter in-grid?) length)
        p2 (->> (distinct antinodes-p2) (filter in-grid?) length)]
    [p1 p2]))

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (peg/match parser (slurp "d8/ex.txt"))
  (solve (slurp "d8/ex.txt"))
  (solve (slurp "d8/in.txt")))
