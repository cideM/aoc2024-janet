(def parser (peg/compile ~(some (number :d))))

(defn fixed-size-1-layout
  "Even numbers are converted into :file blocks of size 1, odd numbers into
  :space blocks of size 1."
  [nums]
  (seq [[i block-size] :pairs nums _ :range [0 block-size]]
    (if (even? i)
      {:type :file :size 1 :id (div i 2)}
      @{:type :space :space-left 1 :items @[]})))

(defn variable-size-layout
  "Each block is the size of the number in `nums`. Even -> :file, odd ->
  :space."
  [nums]
  (seq [[i block-size] :pairs nums]
    (if (even? i)
      {:type :file :size block-size :id (div i 2)}
      @{:type :space :space-left block-size :items @[]})))

(defn compact
  "Starting from the right, try to move each :file block into a :space block
  that has enough free space. Try each :file block exactly once."
  [layout]
  (var arr (array/slice layout))
  (loop [i :down-to [(dec (length arr)) 0]
         :let [cur (arr i)
               fit? |(and (= ($ :type) :space) (<= (cur :size) ($ :space-left)))
               free-space-idx (and (= :file (cur :type))
                                   (find-index fit? arr))]
         :when (truthy? free-space-idx)]
    (update arr free-space-idx
            |(do (array/push ($ :items) cur)
               (update $ :space-left |(- $ (cur :size)))
               $))
    (put arr i @{:type :space :space-left (cur :size) :items @[]}))
  arr)

(defn checksum
  "Convert a diskmap (flat list of numbers and nils) into a checksum."
  [diskmap] (+ ;(seq [[i n] :pairs diskmap :when (truthy? n)] (* i n))))

(defn to-diskmap
  "Convert a list of blocks into a flat list of numbers and nils. A :file
  block of size 3 gets turned into the block ID repeated 3 times. A space
  block is nil repeated :space-left times."
  [item]
  (if (= (item :type) :file)
    (array/new-filled (item :size) (item :id))
    (flatten [;(map to-diskmap (item :items))
              ;(array/new-filled (item :space-left) nil)])))

(defn solve [input]
  (let [data (peg/match parser (string/trimr input))
        p1 (->> (fixed-size-1-layout data)
                compact (map to-diskmap) flatten checksum)
        p2 (->> (variable-size-layout data)
                compact (map to-diskmap) flatten checksum)]
    [p1 p2]))

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (peg/match parser (slurp "d9/ex.txt")))
