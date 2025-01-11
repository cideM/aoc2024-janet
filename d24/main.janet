(def parser (peg/compile
              ~{:main (* (sub (to :sep) (group (split "\n" :init)))
                         :sep
                         (group (split "\n" :conn)))
                :gate (* (range "az") (2 (range "09")))
                :sep (2 :s) # separates init and connection block
                :init (* ':gate ":" :s (number :d+))
                :conn (group (* ':gate :s
                                '(+ "AND" "XOR" "OR") :s
                                ':gate " -> "
                                ':gate))}))

(comment
  (pp (string/trimr (slurp "d24/ex.txt")))
  (peg/match parser (string/trimr (slurp "d24/ex.txt"))))

(defn to-decimal [bin]
  (sum (seq [i :down-to [(dec (length bin)) 0]
             :let [pow (- (dec (length bin)) i)]
             :when (= (bin i) 1)]
         (math/pow 2 pow))))

(defn to-binary [n]
  (if (= 0 n) [0]
    (do
      (var v n)
      (reverse (seq [_ :iterate true :until (= 0 v) :let [rem (% v 2)]]
                 (set v (div v 2)) rem)))))

(def ops {"AND" band "OR" bor "XOR" bxor})

(defn results
  "Returns the binary numbers stored in the various x00, z01, ... keys"
  [state]
  (->> (pairs state)
       (group-by |(string/slice (first $) 0 1))
       pairs
       (map (fn [[k values]] [k (->> (sorted values |(< (first $) (first $1)))
                                     (map last)
                                     reverse)]))
       from-pairs))

(assert (deep= (results {"z01" 1 "z00" 0 "x03" 1 "x02" 1 "x01" 0 "x00" 0})
               @{"z" @[1 0] "x" @[1 1 0 0]}))

(defn run [init-state conns]
  (var q (array/slice conns))
  (var state (table/clone init-state))
  (loop [_ :iterate true :until (empty? q)
         :let [[a op b c] (first q)
               gate-fn (ops op)]]
    (array/remove q 0)
    (if (and (state a) (state b))
      (put state c (gate-fn (state a) (state b)))
      (array/push q [a op b c])))
  state)

(defn filter-conns
  "Show only connections belonging to trees that end in `z-values`."
  [conns z-values]
  (let [g (from-pairs (seq [[a _ b c] :in conns] [c [a b]]))]
    (var queue (array/slice z-values))
    (as-> (catseq [_ :iterate true :until (empty? queue)
                   :let [cur (first queue) next (g cur)]
                   :after (do
                            (array/remove queue 0)
                            (when next (array/push queue ;next)))]
                  (if next
                    [[cur true] [(first next) true] [(last next) true]]
                    [[cur true]])) _
          (from-pairs _)
          (filter |(get _ (last $)) conns))))

(assert (deep= (filter-conns [["a" "XOR" "b" "c"]] ["c"])
               @[["a" "XOR" "b" "c"]]))
(assert (deep= (filter-conns [["a" "XOR" "b" "c"]] ["d"])
               @[]))
(assert (deep= (filter-conns [["a" "XOR" "b" "c"]
                              ["e" "AND" "f" "a"]] ["c"])
               @[["a" "XOR" "b" "c"]
                 ["e" "AND" "f" "a"]]))

(defn digraph
  "Generates a DOT language graph for all the given connections. Any zXX
  values are arranged at the same height. `incorrect` should be a list of
  node names (most likely z00, z01, ...). These nodes will be colored red."
  [conns incorrect]
  (string/join
    ["digraph G {"
     "  {rank=same; z00; z01; z02; z03; z04; z05; z06; 
	     z07; z08; z09; z10; z11; z12; z13; z14; z15; 
	     z16; z17; z18; z19; z20; z21; z22; z23; z24; 
	     z25; z26; z27; z28; z29; z30; z31; z32; z33; z34; 
	     z35; z36; z37; z38; z39; z40; z41; z42; z43; z44; z45}"

     ;(seq [z :in incorrect]
        (string "  " z " [shape=circle, style=filled, fillcolor=red]"))

     ;(seq [[a op b c] :in conns]
        (string (string "  " `{` a ";" b `}`) " -> " c " [label=" op "]" ";"))
     "}"]
    "\n"))

(defn state-from-binary
  "Convert the binary number `num` into a table that can be used as the state
  for `run`. Each slot in the binary number is converted into a k/v pair. If
  `char` is 'z' the pairs will be z00: 1, z01: 0, ..."
  [char num]
  (->> (seq [[i n] :pairs (reverse num)] [(string/format "%s%02d" char i) n])
       from-pairs))

(defn fixed-len-binary
  "Convert `n` to a padded binary number"
  [n len]
  (let [binary (reverse (to-binary n))]
    (assert (<= (length binary) len))
    (reverse (seq [i :range [0 len]] (or (get binary i) 0)))))

(assert (deep= (fixed-len-binary 2 5) @[0 0 0 1 0]))
(assert (deep= (fixed-len-binary 2 2) @[1 0]))

(defn gen-states []
  (let [rng (math/rng)]
    (generate [_ :iterate true
               :let [n1 (math/rng-int rng 100000000)
                     n2 (math/rng-int rng 100000000)
                     state (merge-into @{}
                                       (->> (fixed-len-binary n1 46)
                                            (state-from-binary "x"))
                                       (->> (fixed-len-binary n2 46)
                                            (state-from-binary "y")))]]
      [n1 n2 state])))

(defn find-incorrect-input
  "Your input may not uncover all incorrectly wired up gates. This
	function tries to find more incorrect initial states"
  [conns]
  (find (fn [[n1 n2 state]]
          (let [got (->> (run state conns) results (get "z"))
                want (to-binary (+ n1 n2))]
            (not (deep= want got))))
        (gen-states)))

(defn generate-debug-graph
  "Parse the input for today's puzzle and find a state (x and y values) that
  results in an incorrect output. This state is used to write a DOT graph to
  `g.dot` which you can turn into an .svg file with `dot -Tsvg g.dot  -o
  output.svg`. Then `open output.svg` to start solving part 2"
  [input]
  (let [[init conns] (->> (string/trimr input)
                          (peg/match parser))
        [n1 n2 state] (find-incorrect-input conns)
        want (fixed-len-binary (+ n1 n2) 46)
        got (as-> (run state conns) _
                  (results _)
                  (get _ "z"))
        z-values-different (->> (map tuple got (slice want 1))
                                (filter |(not= ;$))
                                (state-from-binary "z")
                                pairs
                                (map first))]
    (if (= 0 (length z-values-different))
      (pp "you solved it")
      (spit "g.dot" (digraph (filter-conns conns z-values-different)
                             z-values-different)))))

(defn solve [input]
  (let [[init conns] (->> (string/trimr input) (peg/match parser))
        state (from-pairs init)
        p1 (as-> (run (from-pairs init) conns) _
                 (results _)
                 (get _ "z")
                 (to-decimal _))]
    [p1]))

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (generate-debug-graph (slurp "d24/in.txt")))
