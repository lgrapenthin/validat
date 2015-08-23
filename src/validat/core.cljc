(ns validat.core
  "A check is a function checking its only argument, returning nil if
  the check passes and a delay of a vector of errors otherwise.  An
  error has this format:
  
     [identifier optional*]

  Unqualified identifier keywords are reserved for validat."
  (:refer-clojure :exclude [and or]))

(defn- catching
  [f]
  (fn [x]
    (try (f x)
         (catch #?(:clj Throwable, :cljs :default) e
                [[:exception e]]))))

(defn and
  "Create a checker that returns all failing checks if one check fails.
  Throwing checks are marked as [:exception e]."
  [& checks]
  (fn [x]
    (loop [[check & checks] checks]
      (if check
        (if-let [error (check x)]
          (delay
           (into @error
                 (comp (keep (catching #(% x)))
                       (mapcat deref))
                 checks))
          (recur checks))))))

(defn or
  "Create a checker that returns the result of every check as [:or &
  errors] if all checks fail."
  [& checks]
  (fn [x]
    (loop [[check & checks] checks
           errors []]
      (if check
        (if-let [error (check x)]
          (recur checks
                 (conj errors error)))
        (if (seq errors)
          (delay [(into [:or] (mapcat deref) errors)]))))))

(defn check
  "Create a check from a pred with a fixed error."
  [pred error]
  (fn [x] (if (pred x) nil (delay (list error)))))

;; MAPS
(defn exclusive-keys
  "Create a check for a map having no other keys than ks.  Returns all
  illegal keys as [:at key [:invalid-field]] if the check fails."
  [ks]
  (let [allowed? (set ks)
        make-error (fn [k]
                     [:at k [:invalid-field]])]
    (fn [m]
      (loop [[k & ks] (keys m)]
        (if k
          (if (allowed? k)
            (recur ks)
            (delay
             (into [(make-error k)]
                   (comp (remove allowed?)
                         (map make-error))
                   ks))))))))

(defn at-key
  "Return a check that checks key k in a map with check.  Mode can be
  one of :optional, :required.  

  General error [:at k & errors]
  Error for missing a required key: [:at k [:required]]."
  [mode k check]
  (let [wrapped-check
        (fn [v]
          (if-let [err (check v)]
            (delay [(into [:at k] @err)])))]
    (case mode
      :optional
      (fn [m]
        (if (contains? m k)
          (wrapped-check (get m k))))
      :required
      (fn [m]
        (if (contains? m k)
          (wrapped-check (get m k))
          (delay [[:at k [:required]]]))))))

(defn map-check
  "General purpose check builder for maps.  

  m must be a map k->check.  Check may be a map for nested
  checking.

  The following special keys are supported in m

  :validat.core/required - Keys that are non-optional keys or the
  keyword :all, meaning :all keys in m

  :validat.core/exclusive? - If non-nil, no other keys than those in m
  are allowed.  Defaults to true.

  Errors are specified in exclusive-keys and at-key."
  [m]
  (let [required (set (::required m))
        exclusive? (::exclusive? m true)
        m (dissoc m ::exclusive? ::required)
        required (case required
                   :all (set (keys m))
                   required)]
    (->> (cond->> (map (fn [[k ch]]
                         (at-key (if (contains? required k)
                                   :required
                                   :optional)
                                 k
                                 (cond-> ch
                                   (map? ch) map-check)))
                       m)
           exclusive? (cons (exclusive-keys (keys m))))
         (apply and))))

(defn- error-map*
  [errors]
  (if (seq errors)
    (let [by-k (group-by second errors)]
      (zipmap (keys by-k)
              (map (fn [errs]
                     (let [errs (mapcat (partial drop 2) errs)]
                       (cond-> errs
                         (every? (comp #{:at} first) errs)
                         (error-map*))))
                   (vals by-k))))))

(defn error-map
  "Assuming that errors are a (derefed) result of map validation,
  recursively create a map with all errors so that they can be found
  at the keys they occured in.  Useful for looking up errors via
  get-in when doing form-validation and displaying multiple errors at
  once."
  [errors]
  (assert (every? #(= :at (first %)) errors)
          (str "Require only :at errors to build error-map: "
               (pr-str errors)))
  
  (error-map* errors))

(comment
  (def check-str (check string? [:string]))
  (def check-num (check number? [:number]))
  (def check-pos (check pos? [:pos]))

  (def check-my-map (comp error-map
                          #(some-> % deref)
                          (map-check {:foo {:baz (and check-num
                                                      check-pos)
                                            ::required :all}
                                      :bertrand (or check-str
                                                    check-num) 
                                      :bar check-num
                                      ::required [:foo]
                                      ::exclusive? true})))

  (check-my-map {:foo {:c -32
                       :baz -100}})

  ;; {:foo {:baz ([:pos])}}

  (check-my-map {:foo {:c -32
                       :baz 100}})

  ;; nil

  (check-my-map {:foo {:baz 100}
                 :bar "bazinga"})

  ;; {:bar ([:number])}

  (check-my-map {:foo {:c 32}})

  ;; {:foo {:baz ([:required])}}

  (check-my-map {:foo {:baz 100}
                 :bar 333
                 :baz :baz})

  ;; {:baz ([:invalid-field])}

  (check-my-map {:foo {:baz -100}
                 :bertrand :foo})
  )
