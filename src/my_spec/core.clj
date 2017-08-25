(ns my-spec.core
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.spec.gen.alpha :as gen])
  (:import (java.util.Date))
  (:gen-class))

(s/conform even? 1000)

(s/valid? even? 10)

(s/valid? inst? (Date.))

(s/def ::suit  #{:club :diamond :heart :spade})
(s/conform ::suit :club)

(s/def ::date inst?)
(s/valid? ::date (Date.)) ;; true

;; And
(s/def ::big-even (s/and int? even? #(> % 1000)))
(s/valid? ::big-even :foo) ;; false
(s/valid? ::big-even 10) ;; false
(s/valid? ::big-even 10000) ;; true

;; Or
(s/def ::name-or-id
 (s/or :name string?
       :id int?))

(s/valid? ::name-or-id "abc") ;; true
(s/valid? ::name-or-id 100) ;; true
(s/valid? ::name-or-id :foo) ;; false

(s/conform ::name-or-id "abc")

;; nillable
(s/valid? string? nil) ;; false
(s/valid? (s/nilable string?) nil) ;; true

;; explain
(s/explain ::suit 42)
(s/explain-str ::suit 42) ;; "val: 42 fails spec: :my-spec.core/suit predicate: #{:spade :heart :diamond :club}\n:clojure.spec.alpha/spec  :my-spec.core/suit\n:clojure.spec.alpha/value  42\n"
(s/explain-data ::suit 42) ;; #:clojure.spec.alpha{:problems [{:path [], :pred #{:spade :heart :diamond :club}, :val 42, :via [:my-spec.core/suit], :in []}], :spec :my-spec.core/suit, :value 42}

;; Entity Maps
(def email-regex #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$")
(s/def ::email-type (s/and string? #(re-matches email-regex %)))

(s/def ::acctid int?)
(s/def ::first-name string?)
(s/def ::last-name string?)
(s/def ::email ::email-type)

(s/def ::person
 (s/keys :req [::first-name ::last-name ::email]
         :opt [::phone]))

(s/valid? ::person
 {::first-name "Elon"
    ::last-name "Musk"
    ::email "elon@example.com"})
;; true

;; Fails required key check
(s/explain-str ::person
 {::first-name "Elon"})
;; "val: #:my-spec.core{:first-name \"Elon\"} fails spec: :my-spec.core/person predicate: (contains? % :my-spec.core/last-name)\nval: #:my-spec.core{:first-name \"Elon\"} fails spec: :my-spec.core/person predicate: (contains? % :my-spec.core/email)\n:clojure.spec.alpha/spec  :my-spec.core/person\n:clojure.spec.alpha/value  #:my-spec.core{:first-name \"Elon\"}\n"

;; Fails attribute conformance
(s/explain-str ::person
 {::first-name "Elon"
  ::last-name "Musk"
  ::email "N/A"})
;; "In: [:my-spec.core/email] val: \"N/A\" fails spec: :my-spec.core/email-type at: [:my-spec.core/email] predicate: (re-matches email-regex %)\n:clojure.spec.alpha/spec  :my-spec.core/person\n:clojure.spec.alpha/value  #:my-spec.core{:first-name \"Elon\", :last-name \"Musk\", :email \"N/A\"}\n"


(s/def :unq/person
 (s/keys :req-un [::first-name ::last-name ::email]
         :opt-un [::phone]))

(s/conform :unq/person
 {:first-name "Elon"
  :last-name "Musk"
  :email "elon@example.com"})


(defrecord Person [first-name last-name email phone])

(s/explain-str :unq/person
 (->Person "Elon" "Musk" nil nil))
;; "In: [:email] val: nil fails spec: :my-spec.core/email-type at: [:email] predicate: string?\n:clojure.spec.alpha/spec  :unq/person\n:clojure.spec.alpha/value  #my_spec.core.Person{:first-name \"Elon\", :last-name \"Musk\", :email nil, :phone nil}\n"


;; Collections
(s/conform (s/coll-of keyword?) [:a :b :c])

(s/def ::vnum3 (s/coll-of number? :kind vector? :count 3 :distinct true :into #{}))
(s/conform ::vnum3 [1 2 3])

(s/def ::scores (s/map-of string? int?))
(s/conform ::scores {"Sally" 123, "Bob" 453})


;; Validation

(defn person-name
  [person]
  {:pre [(s/valid? ::person person)]
   :post [(s/valid? string? %)]}
 (str (::first-name person) " " (::last-name person)))

(person-name 42)
;; AssertionError Assert failed: (s/valid? :my-spec.core/person person)  my-spec.core/person-name (form-init6455016088873379088.clj:105)

(person-name {::first-name "Elon" ::last-name "Musk" ::email "elon@example.com"})
;; "Elon Musk"


;; Assertions

(defn person-name-2
  [person]
  (let [p (s/assert ::person person)]
   (str (::first-name p) " " (::last-name p))))

(s/check-asserts true)
(person-name-2 100)


;; Spec'ing functions

(defn ranged-rand
 "Returns random it in range sart <= rand < end."
  [start end]
  (+ start (long (rand (- end start)))))

(s/fdef ranged-rand
 :args (s/and (s/cat :start int? :end int?)
        #(< (:start %) (:end %)))
 :ret int?
 :fn (s/and #(>= (:ret %) (-> % :args :start))
      #(< (:ret %) (-> % :args :end))))

(stest/instrument `ranged-rand)

(ranged-rand 9 1)

(stest/check `ranged-rand)

;; Sampling generators

(gen/generate (s/gen int?))
(gen/generate (s/gen string?))

(gen/sample (s/gen string?))

;; Exercise
(s/exercise (s/cat :k keyword? :ns (s/+ number?)) 5)

(s/exercise-fn `ranged-rand)
