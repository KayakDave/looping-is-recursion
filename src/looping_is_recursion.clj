(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp] 
          (if (zero? exp)
            acc
            (recur (* base acc) base (dec exp))))]
    (helper 1 base exp)))

 (defn last-element [a-seq]
  (let [helper (fn [a-seq] 
          (if (empty? (rest a-seq))
            (first a-seq)
            (recur (rest a-seq))))]
    (if (empty? a-seq)
      nil
     (helper a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) 
      true
    (or (empty? seq1) (empty? seq2)) 
      false
    (== (first seq1) (first seq2))
      (recur (rest seq1) (rest seq2))
    :else
      false ))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         a-seq a-seq]
    (cond
      (empty? a-seq)
        nil
      (pred (first a-seq))
        idx
      :else
        (recur (inc idx) (rest a-seq)))))

(defn avg [a-seq]
  (loop [count 0
         total 0
         a-seq a-seq]
    (if (empty? a-seq)
      (/ total count)
      (recur (inc count) (+ (first a-seq) total ) (rest a-seq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem] 
    (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))]
    (loop [occur #{}
         a-seq a-seq]
      (if (empty? a-seq)
        occur
      (recur (toggle occur (first a-seq)) (rest a-seq)))))) 
      
(defn fast-fibo [n]
  (cond 
    (zero? n)
      0
    (= n 1)
      1
    :else
      (loop [n (- n 2)
             pprev 0
             prev 1]
        (if (zero? n)
          (+ prev pprev)
          (recur (dec n) prev (+ prev pprev))))))

(defn cut-at-repetition [a-seq]
  (loop [seen #{}
         result []
         a-seq a-seq]
    (let [firstseq (first a-seq)]
      (if (or (empty? a-seq) (contains? seen firstseq))
        result
        (recur (conj seen firstseq) (conj result firstseq) (rest a-seq))))))
