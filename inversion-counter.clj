(use 'clojure.java.io)

(def inversions 0)

(defn merge_and_count' [left right left_len]
  (if (empty? right) left
      (if (empty? left) right
          (if (<= (first left) (first right)) 
              (cons (first left)  (merge_and_count' (rest left) right (- left_len 1)))
              (let [_ (def inversions (+ inversions left_len))]
               (cons (first right) (merge_and_count' left (rest right) left_len)))
                  ))))

(defn inversion_count [list]
  (if (or (empty? list) (nil? (next list))) list
      (let [mid (quot (count list) 2)]
           (merge_and_count' (inversion_count (take mid list)) 
                             (inversion_count (drop mid list)) mid)
                                 )))

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(defn get-lines [fname]
  (with-open [r (reader fname)]
    (doall (map parse-int (line-seq r)))))

(let [list (get-lines "IntArray.txt")
      _ (inversion_count list)]
  (print inversions))
