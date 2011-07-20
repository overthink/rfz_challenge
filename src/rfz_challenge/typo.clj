(ns rfz_challenge.typo)

; ... after the tree question, I found this second problem asked by the same SO
; user, so I tried that too.
;
; http://codereview.stackexchange.com/questions/3473/how-can-i-improve-this-code/3533#3533

(def data (slurp "typo.txt"))

(defn same-pos 
  "Return all the elements of xs and ys that match, position-wise.  e.g.
  (same-pos [\\a \\x \\c] [\\a \\r \\c]) returns [\\a \\c].  Returns nil if xs
  and ys have different lengths."
  [xs ys]
  (when (= (count xs) (count ys))
    (filter (complement nil?) (map #(if (= %1 %2) %1) xs ys))))

(def answer
  (let [ps (partition 5 1 data)]             ; build seq of sliding 5-tuples
    (->> (for [x ps y ps] (same-pos x y))    ; n^2 FTW
         (filter (comp (partial = 4) count)) ; 4 of 5 positions must match
         (first))))

(println (apply str answer))  ; i2+r

