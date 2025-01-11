(def parser (peg/compile ~(some (+ (number :d+) 1))))

(defn run [[a b c] program]
  (var ip 0)
  (var regs @{"a" a "b" b "c" c})
  (var out @[])
  (loop [_ :iterate true :until (>= ip (length program))
         :let [opcode (program ip)
               operand (program (inc ip))
               combo {0 0 1 1 2 2 3 3 4 (regs "a") 5 (regs "b") 6 (regs "c")}
               bxor64 |(int/to-number (bxor (int/u64 $) (int/u64 $1)))]]
    (match opcode
      0 (->> (combo operand) (math/pow 2) (div (regs "a")) (put regs "a"))
      1 (put regs "b" (bxor64 (regs "b") operand))
      2 (put regs "b" (% (combo operand) 8))
      3 (when (not= 0 (regs "a")) (set ip operand))
      4 (put regs "b" (bxor64 (regs "b") (regs "c")))
      5 (array/push out (string (% (combo operand) 8)))
      6 (->> (combo operand) (math/pow 2) (div (regs "a")) (put regs "b"))
      7 (->> (combo operand) (math/pow 2) (div (regs "a")) (put regs "c")))
    (when (not (and (= 3 opcode) (not= 0 (regs "a"))))
      (+= ip 2)))
  [out regs])

(defn quine [[a b c] program]
  (var a_ (+ a (math/pow 8 15)))
  (var pow 13)
  (var match-index (dec (length program)))
  (var program_ @[])
  (loop [_ :iterate true :while (> match-index 0)
         :before (+= a_ (math/pow 8 pow))
         :let [output (first (run [a_ b c] program))
               want (string ;(slice program match-index (length program)))
               got (string ;(slice output match-index (length program)))]
         :when (deep= want got)]
    (array/insert program_ 0 (output match-index))
    (set pow (max 0 (dec pow)))
    (-= match-index 1))
  a_)

(defn solve [input]
  (let [[a b c & program] (peg/match parser (string/trimr input))
        [out regs] (run [a b c] program)
        a_ (quine [a b c] program)]
    [(string/join out ",") a_]))

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (solve (slurp "d17/ex.txt"))
  (peg/match parser (slurp "d17/ex.txt")))
