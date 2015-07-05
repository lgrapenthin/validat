(ns validat.core
  "Simple custom validation without too much hustle."
  (:refer-clojure :exclude [and or]))

;; A checker returns a preferably lazy seq of errors or nil if the
;; value passes.  Errors should follow this format:

;;   [identifier optional*]

;;   Non namespaced identifier keywords are reserved for validat.

(defn and
  "Composes a lazy checker that returns any errors returned by checkers."
  [& checkers]
  (fn [x]
    (not-empty (mapcat (catching #(% x)) checkers))))

(defn or
  "Composes a lazy checker that returns the errors by checkers
  as [[:or & errors]]."
  [& checkers]
  (fn [x]
    (let [errors (mapcat (catching #(% x)) checkers)]
      (if (every? seq errors)
        [(cons :or errors)]))))

(defn checker
  "Creates a checker from a pred."
  [pred error]
  (fn [x] (if (pred x) nil (list error))))

(defn- catching
  [f]
  (fn [x]
    (try (f x)
         (catch #?(:clj Throwable, :cljs :default) e
                [[:exception e]]))))

;; Maps
(defn exclusive-keys
  "Return checker for a map having no other keys than ks."
  [ks]
  (let [ks (set ks)]
    (fn [m]
      (let [ks (remove ks (keys m))]
        (if (empty? ks)
          nil
          (map (fn [k]
                 [:at k [:invalid-field]])
               ks))))))

(defn at-key
  "Return a checker that checks key k in a map with checker.  Mode can
  be one of :optional, :required."
  [mode k checker]
  (let [wrapped-checker
        (fn [v]
          (if-let [errs (checker v)]
            [(concat [:at k] errs)]))]
    (case mode
      :optional
      (fn [m]
        (if (contains? m k)
          (wrapped-checker (get m k))))
      :required
      (fn [m]
        (if (contains? m k)
          (wrapped-checker (get m k))
          [[:at k [:required]]])))))

(defn map-checker
  "General purpose checker builder for maps.  

  m must be a map k->checker.  Checker may be a map for nested
  checking.

  The following special keys are supported in m

  :validat.core/required - Keys that are non-optional keys or the
  keyword :all, meaning :all keys in m

  :validat.core/exclusive? - If non-nil, no other keys than those in m
  are allowed.

  By default, all keys are optional."
  [m]
  (let [required (::required m)
        exclusive? (::exclusive? m)
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
                                   (map? ch) map-checker)))
                       m)
           exclusive? (cons (exclusive-keys (keys m))))
         (apply _and))))

(defn- error-map*
  [errors]
  (if (seq errors)
    (let [by-k (group-by second errors)]
      (zipmap (keys by-k)
              (into ()
                    (map (fn [errs]
                           (let [errs (mapcat (partial drop 2) errs)]
                             (cond-> errs
                               (every? (comp #{:at} first) errs)
                               (error-map*)))))
                    (vals by-k))))))

(defn error-map
  "Assuming that errors are a result of map validation, recursively
  create a map with all errors so that they can be found at the keys
  they occured in.  Useful for looking up errors via get-in when doing
  form-validation and displaying multiple errors at once."
  [errors]
  #_(when-let [invalid (some #(not= :at (first %)) errors)]
      (throw (IllegalArgumentException.
              (str "Can't create error map with error " invalid))))
  (assert (every? #(= :at (first %)) errors))
  (error-map* errors))

(comment
  (def check-str (checker string? [:string]))
  (def check-num (checker number? [:number]))
  (def check-pos (checker pos? [:pos]))

  (def check-my-map (comp error-map
                          (map-checker {:foo {:baz (and check-num
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

  (check-my-map {:foo {:baz 100}
                 :bertrand :foo})
  )
