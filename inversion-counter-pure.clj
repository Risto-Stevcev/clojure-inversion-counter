(use 'clojure.java.io)


(defn merge_and_count' [left right inversions]
  (if (empty? right) (list left inversions)
      (if (empty? left) (list right inversions)
          (if (<= (first left) (first right)) 
              (let [result (merge_and_count' (rest left) right inversions)]
                   (list (cons (first left) (first result)) (second result)))
              (let [result (merge_and_count' left (rest right) (+ inversions (count left)))]
                   (list (cons (first right) (first result)) (second result)))
                       ))))

(defn inversion_count [list' list_len]
  (if (or (empty? list') (nil? (next list'))) (list list' 0)
      (let [mid (quot list_len 2)
            left (inversion_count (take mid list') mid)
            right (inversion_count (drop mid list') (- list_len mid))]
           (merge_and_count' (first left) (first right) (+ (second left) (second right)))
                                 )))

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(defn get-lines [fname]
  (with-open [r (reader fname)]
    (doall (map parse-int (line-seq r)))))

(let [list (get-lines "IntArray.txt")
      result (inversion_count list 100000)]
  (print (second result)))
