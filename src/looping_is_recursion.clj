(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (if (zero? k)
                   acc
                   (recur (* acc n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [elem a-seq]
                 (if (empty? a-seq)
                   elem
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [s1 s2]
                 (cond
                   (and (empty? s1) (empty? s2))
                     true
                   (= (first s1) (first s2))
                     (recur (rest s1) (rest s2))
                   :else
                     false))]
    (if (= (count seq1) (count seq2))
      (helper seq1 seq2)
      false)))


(defn find-first-index [pred a-seq]
  (loop [indx 0
         sq a-seq]
    (cond
      (empty? sq)
        nil
      (pred (first sq))
        indx
      :else
        (recur (inc indx) (rest sq)))))

(defn avg [a-seq]
  (if (empty? a-seq)
    nil
    (loop [cnt 0
           acc 0
           sq a-seq]
      (cond
        (empty? sq)
          (/ acc cnt)
        :else
          (recur (inc cnt) (+ acc (first sq)) (rest sq))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [res #{}
         sq a-seq]
    (cond
      (empty? sq)
        res
      (contains? res (first sq))
        (recur (disj res (first sq)) (rest sq))
      :else
        (recur (conj res (first sq)) (rest sq)))))

(defn fast-fibo [n]
  (loop [n1 0
         n2 1
         idx n]
    (cond
      (< idx 1)
        n1
      (<= idx 2)
        (+ n1 n2)
      :else
        (recur n2 (+ n1 n2) (dec idx)))))

(defn cut-at-repetition [a-seq]
  (loop [a-set #{}
         res '[]
         sq a-seq]
    (cond
      (empty? sq)
        res
      (contains? a-set (first sq))
        res
      :else
        (recur (conj a-set (first sq)) (conj res (first sq)) (rest sq)))))

