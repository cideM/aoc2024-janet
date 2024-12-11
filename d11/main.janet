(def parser (peg/compile ~(split :s (number :d+))))

(defn split [num]
  (let [s (string num) half (/ (length s) 2)]
    [(scan-number (string/slice s 0 half))
     (scan-number (string/slice s half))]))

(defn change-stone [stone]
  (cond
    (= stone 0) [1]
    (->> (string stone) length even?) (split stone)
    [(* stone 2024)]))

(defn change-stones
  "Takes a `stones` frequency map. Changes each stone into any number of
  new stones, which inherit the original stone's frequency. Lastly, all new
  stones are merged by adding the frequencies of similar stones"
  [stones]
  (var merged @{})
  (loop [[stone count] :pairs stones changed-stone :in (change-stone stone)]
    (update merged changed-stone |(+ (or $ 0) count)))
  merged)

# There's a single 0 which turns into a single 1. There's also three 10s,
# each of which turns into a 1 and a 0. After merging stones, we're left
# with four 1s and three 0s.
(assert (deep= (change-stones {0 1 10 3}) @{1 4 0 3}))

(defn gen-stones [stones]
  (var cur stones)
  (generate [_ :iterate true] (set cur (change-stones cur)) cur))

(defn solve [input]
  (let [stones (peg/match parser (string/trimr input))
        p1 (->> (frequencies stones) gen-stones (take 25) last values sum)
        p2 (->> (frequencies stones) gen-stones (take 75) last values sum)]
    [p1 p2]))

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (peg/match parser (slurp "d11/ex.txt"))
  (solve (slurp "d11/ex.txt"))
  (solve (slurp "d11/in.txt")))

(assert (= (split 10) [1 0]))
(assert (= (split 1000) [10 0]))
