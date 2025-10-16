(ns forth-simple2)


;;-- Stack Manipulation ----------------------------------------------------------


(defn top
  "Returns the word on the top of the stack, or `nil` if the stack is empty.
  "
  [state]
  (-> state :stack last))


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
  The first item is a list of the top n items on the stack with the previous
  top-of-stack as the last element.
  The second item is the state with the top n items removed from the stack.
  "
  [state n]
  (reduce (fn [[items state] _]
            [(conj items (top state))
             (pop* state)])
          ['() state]
          (range n)))


(defn push
  "Pushes `word` onto the stack.
  Returns the updated state.
  "
  [state word]
  (update state :stack conj word))


(defn updater
  "Returns a function that updates the state by popping `n` items
  from the stack, applying `f` to them, and pushing the result onto
  the stack.

  Note the order of arguments: the previous top-of-stack is the _last_ argument.
  "
  [f n]
  (fn [state]
    (let [[args state] (popn state n)]
      (push state (apply f args)))))


(defn updater*
  "Like `updater`, but the result of `f` is a sequence of items to be pushed
  onto the stack.  The resulting top-of-stack is the last item in the sequence.
  "
  [f n]
  (fn [state]
    (let [[args state] (popn state n)]
      (reduce push state (apply f args)))))


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


(def init-dict
  {"." xdot
   ".s" xdots
   "+" (updater + 2)
   "-" (updater - 2)
   "*" (updater * 2)
   "/" (updater quot 2)
   "%" (updater rem 2)
   "dup" (updater* (fn [a] [a a]) 1)
   "interpret" xinterpret
   "swap" (updater* (fn [a b] [b a]) 2)
   "word" xword})


;;-- REPL ----------------------------------------------------------


(loop [state {:stack []
              :dict init-dict
              :buf ""
              :bufp -1}]
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
