(use 'clojure.java.io)


(defn merge_and_count [left right inversions]
  (loop [l left r right inv inversions result []]
    (cond (and (empty? r) (empty? l)) [result inv]
          (empty? r) [(apply conj result l) inv]
          (empty? l) [(apply conj result r) inv]
          (<= (first l) (first r)) (recur (rest l) r inv (conj result (first l)))
          :else (recur l (rest r) (+ inv (count l))  (conj result (first r))))))

(defn inversion_count [list' list_len]
  (if (or (empty? list') (nil? (next list'))) (list list' 0)
      (let [mid (quot list_len 2)
            left (inversion_count (take mid list') mid)
            right (inversion_count (drop mid list') (- list_len mid))]
           (merge_and_count (first left) (first right) (+ (second left) (second right)))
                                 )))

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(defn get-lines [fname]
  (with-open [r (reader fname)]
    (doall (map parse-int (line-seq r)))))

(let [list (get-lines "IntArray.txt")
      result (inversion_count list 100000)]
  (print (second result)))
