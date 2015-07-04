(ns validat.core
  "Simple custom validation without too much hustle.")

;; A checker returns a preferably lazy seq of errors or nil if the
;; value passes.  Errors should follow this format:

;;   [identifier optional*]

;;   Non namespaced identifier keywords are reserved for validat.

(defn checker
  "Creates a checker from a  pred."
  [pred error]
  (fn [x] (if (pred x) nil (list error))))

(defn- catching
  [f]
  (fn [x]
    (try (f x)
         (catch #?(:clj Throwable, :cljs :default) e
           [[:exception e]]))))

(defn- _and
  [& checkers]
  (fn [x]
    (if-let [err (seq (mapcat (catching #(% x)) checkers))]
      [(cons :all-of err)])))

(defn- _or
  [& checkers]
  (fn [x]
    (let [r (map (catching #(% x)) checkers)]
      (if (some nil? r)
        nil
        [(cons :one-of (apply concat r))]))))

;; Maps
(defn exclusive-keys
  "Return checker for a map having no other keys than ks."
  [& ks]
  (let [ks (set ks)]
    (fn [m]
      (let [ks (remove ks (keys m))]
        (if (empty? ks)
          nil
          [[:invalid-keys ks]])))))

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
      (comp wrapped-checker #(get % k)))))

(comment
  (def check-str (checker string? [::string]))
  (def check-num (checker number? [::number]))

  (def check-my-map (_and (exclusive-keys :foo :bar)
                          (at-key :optional :foo check-str)
                          (at-key :required :bar check-num))))
