(ns forth-simple)


;;-- Stack Manipulation ----------------------------------------------------------


(defn top
  "Returns the word on the top of the stack, or the keyword `:empty` if the
  stack is empty.
  "
  [state]
  (if (seq (:stack state))
    (-> state :stack last)
    :empty))


(defn pop*
  "Removes the top item from the stack.
  Returns the updated state.
  "
  [state]
  (if (seq (:stack state))
    (update state :stack pop)
    (do (println "** stack is empty **")
        state)))


(defn popn
  "Returns a vector with two items.
  The first item is a vector of the top n items on the stack.
  The second item is the state with the top n items removed from the stack.
  "
  [state n]
  (reduce (fn [[items state] _]
            [(conj items (top state))
             (pop* state)])
          [[] state]
          (range n)))


(defn pop1
  "Returns a vector with two items: the top of the stack and the state with an item popped.
  "
  [state]
  [(top state) (pop* state)])


(defn pop2
  "Returns a vector with three items: the top two items from the stack
  and the update state with two items popped.
  "
  [state]
  (let [[[a b] state] (popn state 2)]
    [a b state]))


(defn push
  "Pushes `word` onto the stack.
  Returns the updated state.
  "
  [state word]
  (update state :stack conj word))


(defn update1
  "Pops the top of the stack, passes it through `f`, and pushes the result onto the stack.
  "
  [state f]
  (push (pop* state) (f (top state))))


(defn update2
  "Pops the top two items off the stack, passes them through `f`, and pushes the result onto the stack.
  "
  [state f]
  (let [[a b state] (pop2 state)]
    (push state (f a b))))


;;-- Buffer Parsing ----------------------------------------------------------


(defn next-char
  "Returns the next char from the buffer, or `nil` if the buffer is empty.
  "
  [state]
  (get (:buf state) (:bufp state)))


(defn consume-char
  "Advances the buffer pointer.
  Returns the updated state.
  "
  [state]
  (update state :bufp inc))


;;-- Builtins ----------------------------------------------------------
;;
;; All builtins take a state and return an updated state
;;


(defn xword
  [state]
  (let [want (top state)]
    (loop [state (pop* state)
           word ""]
      (let [c (next-char state)]
        (if (or (not c)
                (= want (int c)))
          (-> state
              consume-char
              (push word))
          (recur (consume-char state)
                 (str word c)))))))


(defn xinterpret
  [state]
  (let [word (top state)
        state (pop* state)
        command (get-in state [:dict word])
        number (when (string? word)
                 (parse-long word))]
    (cond
      command (command state)
      number  (push state number)
      :else   (do (println word "?")
                  state))))


(defn xdot
  [state]
  (println (top state))
  (pop* state))


(defn xdots
  [state]
  (println (pr-str (:stack state)))
  state)


(defn xdup
  [state]
  (push state (top state)))


(defn xswap
  [state]
  (let [[a b state] (pop2 state)]
    (-> state (push a) (push b))))


(def init-dict
  {"." xdot
   ".s" xdots
   "+" #(update2 % +)
   "-" #(update2 % -)
   "*" #(update2 % *)
   "/" #(update2 % quot)
   "%" #(update2 % rem)
   "dup" xdup
   "interpret" xinterpret
   "swap" xswap
   "word" xword})


;;-- REPL ----------------------------------------------------------


(loop [state {:stack []
              :dict init-dict
              :buf ""
              :bufp -1}]
  ;(pr state)
  ;(println)
  (println (pr-str (:stack state)))
  (print "OK ")
  (flush)
  (let [line (read-line)]
    (recur (loop [state (assoc state :buf line :bufp 0)]
             (if (next-char state)
               (-> state
                   (push 32)
                   xword
                   xinterpret
                   recur)
               state)))))
