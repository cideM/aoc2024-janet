(defn bxor64 [a b] (int/to-number (bxor (int/u64 a) (int/u64 b))))

(defn next-secret [init]
  (var n init)
  (generate [_ :iterate true]
    (set n (% (bxor64 n (* n 64)) 16777216))
    (set n (% (bxor64 n (div n 32)) 16777216))
    (set n (% (bxor64 n (* n 2048)) 16777216))))

(defn ones-digit [n] (% n 10))

(defn gen-diffs
  "Given a starting number `init` generate an infinite list of sliding windows
  of size 5 over `next-secret`s. For each window, compute the 4 differences
  between the numbers, and the final price (the last number)."
  [init]
  (def fib (next-secret init))
  (var win @[-1 init ;(take 3 fib)])
  (generate [_ :iterate true]
    (array/remove win 0)
    (array/push win (resume fib))
    (let [[a b c d e] (map ones-digit win)]
      {:diffs [(- b a) (- c b) (- d c) (- e d)] :price e})))

(defn count-windows
  "How many sliding windows are there in a list of size `list-length` given
  windows of size `size`?"
  [list-length size] (- list-length (dec size)))

(defn solve-p2 [nums]
  (var prices @{})
  (let [this-many (count-windows 2000 5)]
    (each n nums
      (var seen @{})
      (loop [{:diffs diffs :price p} :in (take this-many (gen-diffs n))
             :when (not (seen diffs))]
        (put seen diffs true)
        (update prices diffs |(+ (or $ 0) p)))))
  (max ;(values prices)))

(defn solve [input]
  (let [nums (peg/match ~(split :s (number :d+)) (string/trimr input))]
    (print (sum (map |(->> (next-secret $) (take 2000) last) nums)))
    (print (solve-p2 nums))))

(defn main [&] (->> (file/read stdin :all) solve))
