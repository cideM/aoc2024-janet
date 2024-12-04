# TODO: use only-tags when it is released, so the filter later is unnecessary

# We start with a tagged capture to have 'true' on the capture stack
# for later. This does not advance the parser. Next, we parse a
# repeating sequence of:
# - discard all characters until you reach the target pattern (to :skip-until)
# - parse one of three options
#
# The 'mul-instr' grabs the latest capture tagged with :do and passes
# it to the short-fn that constructs a struct. The second parameter in
# the short-fn are the two numbers from 'mul-pat'. We're effectively
# adding the last do() or don't() match to each mul() we see.
(defn mul-instr [do nums] {:do do :nums nums})

(def parser
  (peg/compile
    ~{:main (* (constant true :do) (some (+ :do :dont :mul 1)))
      :do (* "do()" (constant true :do))
      :dont (* "don't()" (constant false :do))
      :mul (cmt (* (-> :do) (group :mul-pat)) ,mul-instr)
      :mul-pat (* "mul(" (number :d+) "," (number :d+) ")")}))

(defn solve [input]
  (let [parsed (->> (string/trimr input) (peg/match parser) (filter struct?))
        p1 (+ ;(seq [x :in parsed] (* ;(x :nums))))
        p2 (+ ;(seq [x :in parsed :when (x :do)] (* ;(x :nums))))]
    [p1 p2]))

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (solve "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
  (->> (string/trimr (slurp "d3/in.txt")) (peg/match parser) (filter struct?) pp)
  (solve (slurp "d3/ex.txt"))
  (solve (slurp "d3/in.txt")))
