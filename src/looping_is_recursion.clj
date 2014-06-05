(ns looping-is-recursion)

(defn power [base exp]
  (loop [exp exp acc 1]
    (if (= exp 0)
      acc
      (recur (dec exp) (* acc base)))))

(defn last-element [a-seq]
  (cond
   (empty? a-seq) nil
   (= (count a-seq) 1) (first a-seq)
   :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2)) false
   (not= (first seq1) (first seq2)) false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [a-seq a-seq index 0]
    (cond
     (empty? a-seq) nil
     (pred (first a-seq)) index
     :else (recur (rest a-seq) (inc index)))))

(defn avg [a-seq]
  (if (empty? a-seq)
    nil
    (loop [b-seq a-seq sum 0 cnt 0]
      (if (empty? b-seq)
        (/ sum cnt)
        (recur (rest b-seq) (+ sum (first b-seq)) (inc cnt))))))


(defn parity [a-seq]
  (letfn [(toggle [a-set elem]
    (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))]
    (loop [a-seq a-seq acc #{}]
      (if (empty? a-seq)
        acc
        (recur (rest a-seq) (toggle acc (first a-seq)))))))

;; (defn fib [n]
;;   (cond
;;    (= n 0) 0
;;    (= n 1) 1
;;    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn fast-fibo [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else
   (loop [n (- n 1) fibs '(0 1)]
     (let [fib (apply + (take 2 fibs))]
       (if (= n 0)
         fib
         (recur (dec n) (cons fib fibs)))))))

(defn cut-at-repetition [a-seq]
  (loop [a-seq a-seq elems [] known #{}]
    (let [elem (first a-seq)]
      (if (or (empty? a-seq)
              (get known elem))
        elems
        (recur (rest a-seq)
               (conj elems elem)
               (conj known elem))))))


