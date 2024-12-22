(defn to-decimal [bin]
  (sum (seq [i :down-to [(dec (length bin)) 0]
             :let [pow (- (dec (length bin)) i)]
             :when (= (bin i) 1)]
         (math/pow 2 pow))))

(defn bxor-binary [a b]
  (let [[longer shorter] (if (> (length a) (length b)) [a b] [b a])
        rev-longer (reverse longer)
        rev-shorter (reverse shorter)]
    (reverse (seq [[i n] :pairs rev-longer
                   :let [n2 (or (get rev-shorter i) 0)]] (bxor n n2)))))

(defn to-binary [n]
  (if (= 0 n) [0]
    (do
      (var v n)
      (reverse (seq [_ :iterate true :until (= 0 v) :let [rem (% v 2)]]
                 (set v (div v 2)) rem)))))

# Janet doesn't have good support for 64 bit integers unfortunately, see
# https://janet.zulipchat.com/#narrow/channel/399615-general/topic/Advent.20of.20Code

(defn bxor-64 [a b]
  (to-decimal (bxor-binary (to-binary a) (to-binary b))))

(defn windows [list size]
  (var i 0)
  (generate [_ :iterate true :until (> i (- (length list) size))
             :let [win (slice list i (+ i size))]]
    (+= i 1)
    win))

(defn merge-with [func & colls]
  (var out @{})
  (each c colls
    (eachp [k v] c
      (if (get out k)
        (update out k |(func $ v))
        (put out k v))))
  out)

(defn gen [init]
  (var n init)
  (generate [_ :iterate true]
    (set n (% (bxor-64 n (* n 64)) 16777216))
    (set n (% (bxor-64 n (div n 32)) 16777216))
    (set n (% (bxor-64 n (* n 2048)) 16777216))))

(defn process [n]
  (var win-prices @{})
  (let [seqs [n ;(take 2000 (gen n))]]
    (each secrets-win (windows seqs 5)
      (let [[a b c d e] (map |(% $ 10) secrets-win)
            diffs [(- b a) (- c b) (- d c) (- e d)]]
        (when (not (get win-prices diffs))
          (put win-prices diffs (% e 10)))))
    [(last seqs) win-prices]))

(defn solve [input]
  (let [nums (peg/match ~(split :s (number :d+)) (string/trimr input))
        results (map process nums)
        p1 (sum (map first results))
        window-results (merge-with + ;(map last results))
        p2 (->> (sorted (pairs window-results) (fn [[_ a] [_ b]] (> a b)))
                first)]
    [p1 p2]))

(defn main [&] (->> (file/read stdin :all) solve pp))

