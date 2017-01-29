(ns looping-is-recursion)

(defn power [base exp]
  (let [p-help
        (fn [base acc limit]
          (if (zero? limit)
            acc
            (recur base (* base acc) (dec limit))))]
    (p-help base 1 exp)))

(defn singleton? [a-seq] (= 1 (count a-seq)))

(defn last-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (last-element (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [seq-tail
        (fn [s1, s2 eq]
          (cond
            (not eq) false
            (empty? s1) (empty? s2)
            (empty? s2) (empty? s1)
            :else (recur
                    (rest s1)
                    (rest s2)
                    (= (first s1) (first s2)))))]
    (seq-tail seq1 seq2 true)))

(defn find-first-index [pred a-seq]
  (if (empty? a-seq)
    nil
    (loop [seq (rest a-seq)
           elem (first a-seq)
           i 0]
      (cond
        (pred elem) i
        (empty? seq) nil
        :else (recur (rest seq) (first seq) (inc i))))))

(defn avg [a-seq]
  (if (empty? a-seq)
    nil
    (loop [seq a-seq
           count 0
           sum 0]
      (if (empty? seq)
        (/ sum count)
        (recur (rest seq) (inc count) (+ sum (first seq)))))))

(defn parity [a-seq]
  (let [toggle
        (fn [x-set elem]
          (if (contains? x-set elem)
            (remove #{elem} x-set)
            (conj x-set elem)))]
    (loop [a-set #{}
           seq a-seq]
      (if (empty? seq)
        a-set
        (recur (set (toggle a-set (first seq))) (rest seq))))))

(defn fast-fibo [n]
  (loop [n1 0
        n2 1
        count n]
        (if (zero? count)
          n1
          (recur n2 (+ n1 n2) (dec count)))))

(defn cut-at-repetition [a-seq]
  (if (empty? a-seq)
    []
    (loop [x-set #{}
          x-seq (rest a-seq)
          ret []
          elem (first a-seq)]
      (cond
        (contains? x-set elem) ret
        (empty? x-seq) (conj ret elem)
        :else (recur
                (conj x-set elem)
                (rest x-seq)
                (conj ret elem)
                (first x-seq))))))
