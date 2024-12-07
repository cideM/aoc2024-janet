(def parser (peg/compile
              ~{:main (split "\n" (group :line))
                :line (* (number :d+) ": " (split " " (number :d+)))}))

(defn solvable? [make-opts [target a b & rest]]
  (let [opts (make-opts a b)]
    (if (empty? rest)
      (some |(= target $) opts)
      (some |(solvable? make-opts [target $ ;rest]) opts))))

(defn solve [input]
  (let [data (peg/match parser (string/trimr input))
        make-opts-p1 |[(* $ $1) (+ $ $1)]
        make-opts-p2 |[(* $ $1) (+ $ $1) (scan-number (string $ $1))]]
    [(->> (filter |(solvable? make-opts-p1 $) data) (map first) sum)
     (->> (filter |(solvable? make-opts-p2 $) data) (map first) sum)]))

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (peg/match parser "190: 10 10")
  (peg/match parser (slurp "d7/ex.txt"))
  (solve (slurp "d7/ex.txt"))
  (solve (slurp "d7/in.txt")))
