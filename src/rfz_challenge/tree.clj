(ns rfz_challenge.tree)

; I discovered this "ReadyForZero challenge" by offering an improved version of
; someone else's Clojure code.
;
; http://codereview.stackexchange.com/questions/3475/how-can-i-improve-this-code/3478#3478

(def tree (slurp "tree.txt"))

(defn parse [s]
  (apply + (->> s
             (partition-all 2)
             (map (comp str first))
             (filter (partial re-find #"\d"))
             (map #(Integer/parseInt %)))))

(println (parse tree))

