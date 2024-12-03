(def parser (peg/compile ~(split "\n" (group (split :s (any (number :d+)))))))

(defn safe? [nums]
  (let [diffs (map (partial -) (slice nums 1) nums)]
    (and (or (all neg? diffs) (all pos? diffs))
         (all (comp truthy? (partial get (range 0 4)) math/abs) diffs))))

(defn without [xs i] (tuple ;(slice xs 0 i) ;(slice xs (inc i))))

(defn safe-ish? [xs] (find (comp safe? (without xs)) (range 0 (length xs))))

(defn solve [input]
  (let [data (peg/match parser (string/trimr input))]
    [(count safe? data) (count safe-ish? data)]))

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (solve (slurp "d2/ex.txt"))
  (solve (slurp "d2/in.txt")))

(assert (deep= (peg/match parser "1 2\n3 4") @[@[1 2] @[3 4]]))
(assert (deep= (peg/match parser "1 2\n") @[@[1 2] @[]]))
(assert (deep= (peg/match parser "10 22\n3 14 22") @[@[10 22] @[3 14 22]]))

(assert (= (safe? [7 6 4 2 1]) true))
(assert (= (safe? [1 2 7 8 9]) false))
(assert (= (safe? [9 7 6 2 1]) false))
(assert (= (safe? [1 3 2 4 5]) false))
(assert (= (safe? [8 6 4 4 1]) false))
(assert (= (safe? [1 3 6 7 9]) true))

(assert (= (without [1 2 3] 0) [2 3]))
(assert (= (without [1 2 3] 1) [1 3]))
(assert (= (without [1 2 3] 2) [1 2]))

(assert (= (safe-ish? [7 6 4 2 1]) true))
(assert (= (safe-ish? [1 2 7 8 9]) false))
(assert (= (safe-ish? [9 7 6 2 1]) false))
(assert (= (safe-ish? [1 3 2 4 5]) true))
(assert (= (safe-ish? [8 6 4 4 1]) true))
