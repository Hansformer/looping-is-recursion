(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp) acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp))
  )

(defn power2 [base exp]
  (if (zero? exp) 1
    (* base (power base (dec exp))))
  )


(defn last-element [a-seq]
  (let [helper (fn [e tail]
                 (if (empty? tail) e
                 (recur (first tail) (rest tail))))]
    (helper nil a-seq))
  )

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (cond
                   (and (empty? seq1) (empty? seq2)) true
                   (or (empty? seq1) (empty? seq2)) false
                   (not (= (first seq1) (first seq2))) false
                   :else (recur (rest seq1) (rest seq2))))]
    (helper seq1 seq2))
  )

(defn find-first-index [pred a-seq]
  (loop [i 0
         seq a-seq]
    (cond
      (empty? seq) nil
      (pred (first seq)) i
      :else (recur (inc i) (rest seq))))
  )

(defn avg [a-seq]
  (loop [i 0
         sum 0
         seq a-seq]
    (if (empty? seq)
      (/ sum i)
      (recur (inc i) (+ sum (first seq)) (rest seq))))
  )

(defn toggle-helper [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [seq1 #{}
         seq2 a-seq]
    (if (empty? seq2) seq1
      (recur (toggle-helper seq1 (first seq2)) (rest seq2))))
  )

(defn fast-fibo [n]
  (loop [i 0
         cur 1
         pv 0
         pv2 0]
    (cond
      (= i n) pv
      :else (recur (inc i) (+ cur pv) cur pv)))
  )

(defn already? [list elem]
  (some #{elem} list)
  )

(defn cut-at-repetition [a-seq]
  (loop [seq a-seq
         elems #{}
         newlist '[]]
    (if (empty? seq) newlist
      (recur (rest seq)
             (conj elems (first seq))
             (if (contains? elems (first seq)) newlist
               (conj newlist (first seq))))))
  )

; _---DDD.:DDDDasd
