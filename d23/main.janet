(def parser (peg/compile ~(split :s (group (* ':w+ "-" ':w+)))))

(defn build-graph [conns]
  (var g @{})
  (let [insert |(update g $ (fn [t] (merge-into (or t @{}) {$1 true})))]
    (loop [[l r] :in conns]
      (insert l r)
      (insert r l)))
  g)

(defn build-longest-sets [g]
  (let [ks (keys g) one-sets (map |[$] ks)
        func (fn [xs x] (if (all |(get-in g [x $]) xs) [;xs x] xs))]
    (map (fn [set] (reduce func set ks)) one-sets)))

(defn build-sets-three [g]
  (let [two (seq [[n ns] :pairs g [k _] :pairs ns] [n k])]
    (seq [[a b] :in two [c ns] :pairs g :when (and (ns a) (ns b))]
      [;(sorted [a b c])])))

(defn solve [input]
  (let [conns (peg/match parser (string/trimr input))
        graph (build-graph conns)
        p1 (->> (build-sets-three graph)
                distinct
                (count (fn [x] (find |(string/has-prefix? "t" $) x))))
        p2 (as-> (build-longest-sets graph) _
                 (sorted _ |(> (length $) (length $1)))
                 (first _)
                 (sorted _)
                 (string/join _ ","))]
    [p1 p2]))

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (peg/match parser (string/trimr (slurp "d23/ex.txt"))))
