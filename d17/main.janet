(def parser (peg/compile ~(some (+ (number :d+) 1))))

(defn run [[a b c] program]
  (var ip 0)
  (var regs @{"a" a "b" b "c" c})
  (var out @[])
  (loop [_ :iterate true :until (>= ip (length program))
         :let [opcode (program ip)
               operand (program (inc ip))
               combo-ops {0 |0 1 |1 2 |2 3 |3 4 |(regs "a") 5 |(regs "b")
                          6 |(regs "c")}]]
    (cond
      (= 0 opcode) (->> ((combo-ops operand))
                        (math/pow 2)
                        (div (regs "a"))
                        (put regs "a"))
      (= 1 opcode) (put regs "b" (as-> (regs "b") _
                                       (bxor (int/u64 _) operand)
                                       (int/to-number _)))
      (= 2 opcode) (put regs "b" (% ((combo-ops operand)) 8))
      (= 3 opcode) (when (not= 0 (regs "a")) (set ip operand))
      (= 4 opcode) (put regs "b" (int/to-number (bxor (int/u64 (regs "b"))
                                                      (int/u64 (regs "c")))))
      (= 5 opcode) (array/push out (string (% ((combo-ops operand)) 8)))
      (= 6 opcode) (->> ((combo-ops operand))
                        (math/pow 2)
                        (div (regs "a"))
                        (put regs "b"))
      (= 7 opcode) (->> ((combo-ops operand))
                        (math/pow 2)
                        (div (regs "a"))
                        (put regs "c")))
    (if (and (= 3 opcode) (not= 0 (regs "a")))
      ()
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
  (math/pow 2 12)
  (solve (slurp "d17/ex.txt"))
  (peg/match parser (slurp "d17/ex.txt")))
