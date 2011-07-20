(ns frz_challenge.subseq)

(set! *warn-on-reflection* true)

; Finally found the source of these SO questions, and though I'd try the
; remaining one.  I found it harder to write sane Clojure code for this one.

;The following sequence of digits sums to 5824. Break up the sequence into the
;LARGEST number of contiguous subsequences such that the sums of each of the
;subsequences are equal. Enter the number of subsequences below to continue.

(def data (slurp "subseq.txt"))

(defn safe-int 
  "Parse c into an int if possible.  nil otherwise."
  [c] (try (Integer/parseInt (str c)) (catch Exception e nil)))

; data as integers
(def xs (keep identity (map safe-int data)))

(defn take-sum
  "Take as many contiguous items from the head of xs as necessary until their
  sum is exactly n.  Return [head tail] where head is a seq of items that sum
  to n and tail is the rest of xs."
  [n xs]
  (when (not-empty xs)
    (let [f (fn [cur xs]
              (when-let [[head & tail] (seq xs)]
                (let [newcur (conj cur head)
                      sum    (apply + newcur)]
                  (cond
                    (= sum n) [newcur (if (nil? tail) [] tail)]
                    (> sum n) nil
                    :else (recur newcur tail)))))]
      (f [] xs))))

(defn equal-subseqs
  "Returns a lazy seq of contiguous subsequences of xs where each subsequence
  sums to n.  Returns nil if subsequences can't be formed to satisfy this."
  [n xs]
  (let [[head tail :as xs] (take-sum n xs)]
    (cond
      (nil? xs) nil
      :else (lazy-seq (cons head (equal-subseqs n tail))))))

;; cop-out -- technically equal-subseqs isn't correct at the moment.  It should
;; return nil if it can't consume the entire input xs, but it's not... so I
;; just run it against all potential sums, sort by number of segments
;; created, and take the one that creates the most segments.
(->> (map (fn [x] [x (equal-subseqs x xs)]) (reductions + xs)) 
  (map (fn [[n xs]] [n (count xs)])) 
  (sort-by second) 
  (last) 
  (second)
  (println))
 kk:w
