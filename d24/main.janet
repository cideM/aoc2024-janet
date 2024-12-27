(def parser (peg/compile
              ~{:main (* :init-block "\n" :conns-block)
                :gate (repeat 3 (range "09" "az"))
                :op (+ "AND" "XOR" "OR")
                :init (group (* ':gate ":" :s (number :d+)))
                :init-block (group (some (* :init "\n")))
                :conn (group (* ':gate :s ':op :s ':gate :s "->" :s ':gate))
                :conns-block (group (some (* :conn (? "\n"))))}))

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

(defn bor-binary [a b]
  (let [[longer shorter] (if (> (length a) (length b)) [a b] [b a])
        rev-longer (reverse longer)
        rev-shorter (reverse shorter)]
    (reverse (seq [[i n] :pairs rev-longer
                   :let [n2 (or (get rev-shorter i) 0)]] (bor n n2)))))

(defn band-binary [a b]
  (let [[longer shorter] (if (> (length a) (length b)) [a b] [b a])
        rev-longer (reverse longer)
        rev-shorter (reverse shorter)]
    (reverse (seq [[i n] :pairs rev-longer
                   :let [n2 (or (get rev-shorter i) 0)]] (band n n2)))))

(defn to-binary [n]
  (if (= 0 n) [0]
    (do
      (var v n)
      (reverse (seq [_ :iterate true :until (= 0 v) :let [rem (% v 2)]]
                 (set v (div v 2)) rem)))))

# Janet doesn't have good support for 64 bit integers unfortunately, see
# https://janet.zulipchat.com/#narrow/channel/399615-general/topic/Advent.20of.20Code

(defn bor-64 [a b]
  (to-decimal (bor-binary (to-binary a) (to-binary b))))

(defn bxor-64 [a b]
  (to-decimal (bxor-binary (to-binary a) (to-binary b))))

(defn band-64 [a b]
  (to-decimal (band-binary (to-binary a) (to-binary b))))

(def ops {"AND" band-64 "OR" bor-64 "XOR" bxor-64})

(defn state-to-num [state prefix]
  (->> (pairs state)
       (filter |(string/has-prefix? prefix (first $)))
       sorted
       (map last)
       reverse
       to-decimal))

(defn run [init-state conns]
  (var q (array/slice conns))
  (var state (table/clone init-state))
  (loop [_ :iterate true :until (empty? q)
         :let [[a op b c] (first q)
               gate (ops op)]]
    (if (and (state a) (state b))
      (do (put state c (gate (state a) (state b))) (array/remove q 0))
      (do (array/remove q 0) (array/push q [a op b c]))))
  state)

(defn state-to-zs [state]
  (let [zs (->> (pairs state)
                (filter (fn [[k _]] (string/has-prefix? "z" k))))
        zs_ (sorted zs (fn [[k _] [k2 _]] (< k k2)))]
    (map last zs_)))

(defn build-prev [conns]
  (var prev @{})
  (loop [[a _ b c] :in conns]
    (put prev c [a b]))
  prev)

(defn path [prev n]
  (var out @[n])
  (var q @[n])
  (loop [_ :iterate true :until (empty? q) :after (array/remove q 0)]
    (when (prev (first q))
      (array/push out ;(prev (first q)))
      (array/push q ;(prev (first q)))))
  out)

(defn dot [conns]
  (spit "g.dot" (string/join ["digraph G {"
                              "{rank=same; z00; z01; z02; z03; z04; z05; z06; z07; z08; z09; z10; z11; z12; z13; z14; z15; z16; z17; z18; z19; z20; z21; z22; z23; z24; z25; z26; z27; z28; z29; z30; z31; z32; z33; z34; z35; z36; z37; z38; z39; z40; z41; z42; z43; z44; z45}"

                              "z11 [shape=circle, style=filled, fillcolor=red]"
                              "z12 [shape=circle, style=filled, fillcolor=red]"
                              "z13 [shape=circle, style=filled, fillcolor=red]"
                              "z14 [shape=circle, style=filled, fillcolor=red]"
                              "z23 [shape=circle, style=filled, fillcolor=red]"
                              "z24 [shape=circle, style=filled, fillcolor=red]"
                              "z25 [shape=circle, style=filled, fillcolor=red]"
                              "z36 [shape=circle, style=filled, fillcolor=red]"
                              "z37 [shape=circle, style=filled, fillcolor=red]"
                              "z38 [shape=circle, style=filled, fillcolor=red]"
                              "z39 [shape=circle, style=filled, fillcolor=red]"
                              "z40 [shape=circle, style=filled, fillcolor=red]"

                              ;(seq [[a op b c] :in conns] (string (string `{` a ";" b `}`) " -> " c " [label=" op "] " ";"))
                              "}"]
                             "\n")))

(defn num-to-state [n char]
  (var arr (array/new-filled 46 0))
  (let [bin-n (reverse (to-binary n))]
    (loop [i :range-to [0 45] :until (> (inc i) (length bin-n))]
      (put arr i (bin-n i))))
  (from-pairs (seq [[i n] :pairs arr] [(string/format "%s%02d" char i) n])))

(comment
  (def [init conns] (->> (string/trimr (slurp "d24/in.txt"))
                         (peg/match parser)))
  (def prevs (build-prev conns))
  (def focus (merge-into @{}
                         (from-pairs (map |[$ true] (path prevs "z15")))
                         (from-pairs (map |[$ true] (path prevs "z16")))
                         (from-pairs (map |[$ true] (path prevs "z17")))))

  (def rng (math/rng))

  (each [k v] (pairs (merge-into @{}
                                 (num-to-state 88315886 "x")
                                 (num-to-state 24513730 "y")))
    (pp (string k ": " v)))

  (loop [_ :range-to [0 100]
         :let [n1 (math/rng-int rng 100000000)
               n2 (math/rng-int rng 100000000)
               state (merge-into @{}
                                 (num-to-state n1 "x")
                                 (num-to-state n2 "y"))
               got (to-binary (state-to-num (run state conns) "z"))
               want (to-binary (+ n1 n2))]
         :when (not (deep= got want))] (pp [got want n1 n2]))

  (dot (filter (fn [[_ _ _ c]] (true? (get focus c))) conns)))

(defn solve [input]
  (let [[init conns] (->> (string/trimr input)
                          (peg/match parser))
        state (from-pairs init)
        final-state (run state conns)
        x (state-to-num state "x")
        y (state-to-num state "y")
        got (state-to-num final-state "z")
        want (to-binary (+ x y))
        want-zs (from-pairs (seq [[i n] :pairs want]
                              [(string/format "z%02d" i) n]))
        p2-init (merge-into @{} want-zs state)]
    (pp ["got " (to-binary got)])
    (pp ["want" want])
    [(state-to-num final-state "z")]))

# qjq XOR kbj -> z05
# srp OR jcf -> frn
# ======
# x16 AND y16 -> vtj
# y16 XOR x16 -> wnf
# ======
# x21 AND y21 -> gmq
# mjj XOR fqr -> z21
# ======
# bkq AND jhv -> wtt
# jhv XOR bkq -> z39

(comment (string/join (sorted ["qnw" "qff" "z23" "qqp" "fbq" "z36" "z16" "pbv"]) ","))

# ("got " @[1 0 1 1 0 0 1 1 1 1 0 1 0 0 1 1 1 0 1 1 0 1 1 0 0 1 0 0 1 0 0 0 1 1 1 1 1 0 1 1 1 0 0 1 1 0])
# ("want" @[1 0 1 1 0 1 0 0 0 0 0 1 0 0 1 1 1 0 1 1 1 0 0 0 0 1 0 0 1 0 0 1 0 0 0 1 1 0 1 1 1 0 0 1 1 0])

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (peg/match parser (string/trimr (slurp "d24/ex.txt"))))

# x11 XOR y11 -> qnw
# x11 AND y11 -> qff
# ==========
# cts XOR bcd -> z23
# wdr OR jcd -> qqp
# ==========
# jdd AND rbm -> fbq
# jdd XOR rbm -> z36

