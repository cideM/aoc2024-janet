(def parser (peg/compile ~(* (group (any (* ':a+ (? ", "))))
                             :s :s
                             (group (split :s ':a+)))))

(var cache @{})
(defn check [s patterns]
  (cond
    (truthy? (get cache s)) (cache s)
    (= s "") (do (put cache s 1) 1)
    (let [result (sum (seq [p :in patterns
                            :when (string/has-prefix? p s)]
                        (check (string/replace p "" s) patterns)))]
      (put cache s result)
      result)))

(defn solve [input]
  (let [[patterns designs] (->> (string/trimr input) (peg/match parser))
        results (map |(check $ patterns) designs)]
    [(count |(> $ 0) results) (sum results)]))

(defn main [&] (->> (file/read stdin :all) solve pp))
