#! /usr/bin/env janet

(comment import ./args :prefix "")
(defn a/parse-args
  [args]
  (def the-args (array ;args))
  #
  (def head (get the-args 0))
  #
  (when (or (not head) (= head "-h") (= head "--help"))
    (break {:show-help true}))
  #
  (when (or (not head) (= head "-v") (= head "--version"))
    (break {:show-version true}))
  #
  (array/remove the-args 0)
  #
  (def [opts file-path]
    (if-not (and (string/has-prefix? "{" head)
                 (string/has-suffix? "}" head))
      [@{} head]
      (let [parsed
            (try (parse (string "@" head))
              ([e] (eprint e)
                   (errorf "failed to parse options: %n" head)))]
        (assertf (and parsed (table? parsed))
                 "expected table but found: %s" (type parsed))
        (def opts parsed)
        (def new-head (get the-args 0))
        (array/remove the-args 0)
        (assertf new-head "expected a command but found none: %n" args)
        [opts new-head])))
  #
  (def input
    (if (= "-" file-path)
      stdin
      (do
        (assertf (= :file (os/stat file-path :mode))
                 "not a file path: %s" file-path)
        #
        (os/realpath file-path))))
  # XXX: improve feedback message?
  (assertf (<= 2 (length the-args))
           "need at least two more arguments: %n" the-args)
  #
  (def [path-str value-str] the-args)
  (assertf (parse-all path-str) "could not parse: %n" path-str)
  #
  (var path (parse-all path-str))
  # XXX: using `tuple?` below, but the expected input is a tuple
  #      that represents a short-fn.  things of the form |(...)
  #      can be used, e.g.
  #
  #        |(= (get $ :name) "niche")
  #
  #      because the parser "expands" this to:
  #
  #        (short-fn (= (get $ :name) "niche"))
  #
  (let [first-step (first path)]
    (if (symbol? first-step)
      (assertf (string/has-prefix? "@" first-step)
               "leading symbol item must start with @: %n" first-step)
      (assertf (or (keyword? first-step)
                   (nat? first-step)
                   (tuple? first-step))
               "unexpected type for first step in path: %n" first-step)))
  #
  (assertf (all |(or (keyword? $) (nat? $) (tuple? $))
                (slice path 1))
           "only keywords, natural numbers, tuples allowed: %n"
           (slice path 1))
  #
  (when (not= "nil" value-str)
    (assertf (parse value-str) "could not parse: %n" value-str))
  #
  (array/remove the-args 0)
  (array/remove the-args 0)
  #
  (def top-level-index
    (let [first-step (get path 0)]
      (if (and (symbol? first-step)
               (string/has-prefix? "@" (slice first-step 0 1)))
        (let [scanned (scan-number (slice first-step 1))]
          (set path [;(slice path 1)])
          (if (number? scanned)
            scanned
            # XXX: haven't settled on whether a keyword is better here
            nil))
        0)))
  #
  (merge opts
         {:input input
          :top-level-index top-level-index
          # XXX: is this `eval` use likely to be a problem?
          :path (eval path)
          :value-str value-str
          :rest the-args}))


(comment import ./jtraverse :prefix "")
(comment import ./jget :prefix "")
(comment import ./jipper :prefix "")
(comment import ./helpers :prefix "")
# based on code by corasaurus-hex

# `slice` doesn't necessarily preserve the input type

# XXX: differs from clojure's behavior
#      e.g. (butlast [:a]) would yield nil(?!) in clojure
(defn j/h/butlast
  [indexed]
  (if (empty? indexed)
    nil
    (if (tuple? indexed)
      (tuple/slice indexed 0 -2)
      (array/slice indexed 0 -2))))

(comment

  (j/h/butlast @[:a :b :c])
  # =>
  @[:a :b]

  (j/h/butlast [:a])
  # =>
  []

  )

(defn j/h/rest
  [indexed]
  (if (empty? indexed)
    nil
    (if (tuple? indexed)
      (tuple/slice indexed 1 -1)
      (array/slice indexed 1 -1))))

(comment

  (j/h/rest [:a :b :c])
  # =>
  [:b :c]

  (j/h/rest @[:a])
  # =>
  @[]

  )

# XXX: can pass in array - will get back tuple
(defn j/h/tuple-push
  [tup x & xs]
  (if tup
    [;tup x ;xs]
    [x ;xs]))

(comment

  (j/h/tuple-push [:a :b] :c)
  # =>
  [:a :b :c]

  (j/h/tuple-push nil :a)
  # =>
  [:a]

  (j/h/tuple-push @[] :a)
  # =>
  [:a]

  )

(defn j/h/to-entries
  [val]
  (if (dictionary? val)
    (pairs val)
    val))

(comment

  (sort (j/h/to-entries {:a 1 :b 2}))
  # =>
  @[[:a 1] [:b 2]]

  (j/h/to-entries {})
  # =>
  @[]

  (j/h/to-entries @{:a 1})
  # =>
  @[[:a 1]]

  # XXX: leaving non-dictionaries alone and passing through...
  #      is this desirable over erroring?
  (j/h/to-entries [:a :b :c])
  # =>
  [:a :b :c]

  )

# XXX: when xs is empty, "all" becomes nil
(defn j/h/first-rest-maybe-all
  [xs]
  (if (or (nil? xs) (empty? xs))
    [nil nil nil]
    [(first xs) (j/h/rest xs) xs]))

(comment

  (j/h/first-rest-maybe-all [:a :b])
  # =>
  [:a [:b] [:a :b]]

  (j/h/first-rest-maybe-all @[:a])
  # =>
  [:a @[] @[:a]]

  (j/h/first-rest-maybe-all [])
  # =>
  [nil nil nil]

  # XXX: is this what we want?
  (j/h/first-rest-maybe-all nil)
  # =>
  [nil nil nil]

  )


(comment import ./locations :prefix "")
# bl - begin line
# bc - begin column
# bp - begin position
# el - end line
# ec - end column
# ep - end position
(defn j/l/make-attrs
  [& items]
  (zipcoll [:bl :bc :bp :el :ec :ep]
           items))

(defn j/l/atom-node
  [node-type peg-form]
  ~(cmt (capture (sequence (line) (column) (position)
                           ,peg-form
                           (line) (column) (position)))
        ,|[node-type (j/l/make-attrs ;(slice $& 0 -2)) (last $&)]))

(defn j/l/reader-macro-node
  [node-type sigil]
  ~(cmt (capture (sequence (line) (column) (position)
                           ,sigil
                           (any :non-form)
                           :form
                           (line) (column) (position)))
        ,|[node-type (j/l/make-attrs ;(slice $& 0 3) ;(slice $& -5 -2))
           ;(slice $& 3 -5)]))

(defn j/l/collection-node
  [node-type open-delim close-delim]
  ~(cmt
     (capture
       (sequence
         (line) (column) (position)
         ,open-delim
         (any :input)
         (choice ,close-delim
                 (error
                   (replace (sequence (line) (column) (position))
                            ,|(string/format
                                (string "line: %p column: %p position: %p "
                                        "missing %p for %p")
                                $0 $1 $2 close-delim node-type))))
         (line) (column) (position)))
     ,|[node-type (j/l/make-attrs ;(slice $& 0 3) ;(slice $& -5 -2))
        ;(slice $& 3 -5)]))

(def j/l/loc-grammar
  ~@{:main (sequence (line) (column) (position)
                     (some :input)
                     (line) (column) (position))
     #
     :input (choice :non-form
                    :form)
     #
     :non-form (choice :whitespace
                       :comment)
     #
     :whitespace ,(j/l/atom-node :whitespace
                             '(choice (some (set " \0\f\t\v"))
                                      (choice "\r\n"
                                              "\r"
                                              "\n")))
     # :whitespace
     # (cmt (capture (sequence (line) (column)
     #                         (choice (some (set " \0\f\t\v"))
     #                                 (choice "\r\n"
     #                                         "\r"
     #                                         "\n"))
     #                         (line) (column)))
     #      ,|[:whitespace (make-attrs ;(slice $& 0 -2)) (last $&)])
     #
     :comment ,(j/l/atom-node :comment
                          '(sequence "#"
                                     (any (if-not (set "\r\n") 1))))
     #
     :form (choice # reader macros
                   :fn
                   :quasiquote
                   :quote
                   :splice
                   :unquote
                   # collections
                   :array
                   :bracket-array
                   :tuple
                   :bracket-tuple
                   :table
                   :struct
                   # atoms
                   :number
                   :constant
                   :buffer
                   :string
                   :long-buffer
                   :long-string
                   :keyword
                   :symbol)
     #
     :fn ,(j/l/reader-macro-node :fn "|")
     # :fn (cmt (capture (sequence (line) (column)
     #                             "|"
     #                             (any :non-form)
     #                             :form
     #                             (line) (column)))
     #          ,|[:fn (make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
     #             ;(slice $& 2 -4)])
     #
     :quasiquote ,(j/l/reader-macro-node :quasiquote "~")
     #
     :quote ,(j/l/reader-macro-node :quote "'")
     #
     :splice ,(j/l/reader-macro-node :splice ";")
     #
     :unquote ,(j/l/reader-macro-node :unquote ",")
     #
     :array ,(j/l/collection-node :array "@(" ")")
     # :array
     # (cmt
     #   (capture
     #     (sequence
     #       (line) (column)
     #       "@("
     #       (any :input)
     #       (choice ")"
     #               (error
     #                 (replace (sequence (line) (column))
     #                          ,|(string/format
     #                              "line: %p column: %p missing %p for %p"
     #                              $0 $1 ")" :array))))
     #       (line) (column)))
     #   ,|[:array (make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
     #      ;(slice $& 2 -4)])
     #
     :tuple ,(j/l/collection-node :tuple "(" ")")
     #
     :bracket-array ,(j/l/collection-node :bracket-array "@[" "]")
     #
     :bracket-tuple ,(j/l/collection-node :bracket-tuple "[" "]")
     #
     :table ,(j/l/collection-node :table "@{" "}")
     #
     :struct ,(j/l/collection-node :struct "{" "}")
     #
     :number ,(j/l/atom-node :number
                         ~(drop (sequence (cmt (capture (some :num-char))
                                               ,scan-number)
                                          (opt (sequence ":" (range "AZ" "az"))))))
     #
     :num-char (choice (range "09" "AZ" "az")
                       (set "&+-._"))
     #
     :constant ,(j/l/atom-node :constant
                           '(sequence (choice "false" "nil" "true")
                                      (not :name-char)))
     #
     :name-char (choice (range "09" "AZ" "az" "\x80\xFF")
                        (set "!$%&*+-./:<?=>@^_"))
     #
     :buffer ,(j/l/atom-node :buffer
                         '(sequence `@"`
                                    (any (choice :escape
                                                 (if-not "\"" 1)))
                                    `"`))
     #
     :escape (sequence "\\"
                       (choice (set `"'0?\abefnrtvz`)
                               (sequence "x" (2 :h))
                               (sequence "u" (4 :h))
                               (sequence "U" (6 :h))
                               (error (constant "bad escape"))))
     #
     :string ,(j/l/atom-node :string
                         '(sequence `"`
                                    (any (choice :escape
                                                 (if-not "\"" 1)))
                                    `"`))
     #
     :long-string ,(j/l/atom-node :long-string
                              :long-bytes)
     #
     :long-bytes {:main (drop (sequence :open
                                        (any (if-not :close 1))
                                        :close))
                  :open (capture :delim :n)
                  :delim (some "`")
                  :close (cmt (sequence (not (look -1 "`"))
                                        (backref :n)
                                        (capture (backmatch :n)))
                              ,=)}
     #
     :long-buffer ,(j/l/atom-node :long-buffer
                              '(sequence "@" :long-bytes))
     #
     :keyword ,(j/l/atom-node :keyword
                          '(sequence ":"
                                     (any :name-char)))
     #
     :symbol ,(j/l/atom-node :symbol
                         '(some :name-char))
     })

(comment

  (get (peg/match j/l/loc-grammar " ") 3)
  # =>
  [:whitespace @{:bl 1 :el 1 :bc 1 :bp 0 :ec 2 :ep 1} " "]

  (get (peg/match j/l/loc-grammar "true?") 3)
  # =>
  [:symbol @{:bl 1 :el 1 :bc 1 :bp 0 :ec 6 :ep 5} "true?"]

  (get (peg/match j/l/loc-grammar "nil?") 3)
  # =>
  [:symbol @{:bl 1 :el 1 :bc 1 :bp 0 :ec 5 :ep 4} "nil?"]

  (get (peg/match j/l/loc-grammar "false?") 3)
  # =>
  [:symbol @{:bl 1 :el 1 :bc 1 :bp 0 :ec 7 :ep 6} "false?"]

  (get (peg/match j/l/loc-grammar "# hi there") 3)
  # =>
  [:comment @{:bl 1 :el 1 :bc 1 :bp 0 :ec 11 :ep 10} "# hi there"]

  (get (peg/match j/l/loc-grammar "1_000_000") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :bp 0 :ec 10 :ep 9} "1_000_000"]

  (get (peg/match j/l/loc-grammar "8.3") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :bp 0 :ec 4 :ep 3} "8.3"]

  (get (peg/match j/l/loc-grammar "1e2") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 3 :bp 0 :ec 4} "1e2"]

  (get (peg/match j/l/loc-grammar "0xfe") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 4 :bp 0 :ec 5} "0xfe"]

  (get (peg/match j/l/loc-grammar "2r01") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 4 :bp 0 :ec 5} "2r01"]

  (get (peg/match j/l/loc-grammar "3r101&01") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 8 :bp 0 :ec 9} "3r101&01"]

  (get (peg/match j/l/loc-grammar "2:u") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 3 :bp 0 :ec 4} "2:u"]

  (get (peg/match j/l/loc-grammar "-8:s") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 4 :bp 0 :ec 5} "-8:s"]

  (get (peg/match j/l/loc-grammar "1e2:n") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 5 :bp 0 :ec 6} "1e2:n"]

  (get (peg/match j/l/loc-grammar "printf") 3)
  # =>
  [:symbol @{:bl 1 :el 1 :bc 1 :ep 6 :bp 0 :ec 7} "printf"]

  (get (peg/match j/l/loc-grammar ":smile") 3)
  # =>
  [:keyword @{:bl 1 :el 1 :bc 1 :ep 6 :bp 0 :ec 7} ":smile"]

  (get (peg/match j/l/loc-grammar `"fun"`) 3)
  # =>
  [:string @{:bl 1 :el 1 :bc 1 :ep 5 :bp 0 :ec 6} "\"fun\""]

  (get (peg/match j/l/loc-grammar "``long-fun``") 3)
  # =>
  [:long-string @{:bl 1 :el 1 :bc 1 :ep 12 :bp 0 :ec 13} "``long-fun``"]

  (get (peg/match j/l/loc-grammar "@``long-buffer-fun``") 3)
  # =>
  [:long-buffer
   @{:bl 1 :el 1 :bc 1 :bp 0 :ec 21 :ep 20}
   "@``long-buffer-fun``"]

  (get (peg/match j/l/loc-grammar `@"a buffer"`) 3)
  # =>
  [:buffer @{:bl 1 :el 1 :bc 1 :ep 11 :bp 0 :ec 12} "@\"a buffer\""]

  (get (peg/match j/l/loc-grammar "@[8]") 3)
  # =>
  [:bracket-array @{:bl 1 :el 1 :bc 1 :ep 4 :bp 0 :ec 5}
   [:number @{:bl 1 :el 1 :bc 3 :ep 3 :bp 2 :ec 4} "8"]]

  (get (peg/match j/l/loc-grammar "@{:a 1}") 3)
  # =>
  [:table @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}
   [:keyword @{:bl 1 :el 1 :bc 3 :ep 4 :bp 2 :ec 5} ":a"]
   [:whitespace @{:bl 1 :el 1 :bc 5 :ep 5 :bp 4 :ec 6} " "]
   [:number @{:bl 1 :el 1 :bc 6 :ep 6 :bp 5 :ec 7} "1"]]

  (get (peg/match j/l/loc-grammar "~x") 3)
  # =>
  [:quasiquote @{:bl 1 :el 1 :bc 1 :ep 2 :bp 0 :ec 3}
   [:symbol @{:bl 1 :el 1 :bc 2 :ep 2 :bp 1 :ec 3} "x"]]

  (get (peg/match j/l/loc-grammar "' '[:a :b]") 3)
  # =>
  [:quote @{:bl 1 :el 1 :bc 1 :ep 10 :bp 0 :ec 11}
   [:whitespace @{:bl 1 :el 1 :bc 2 :ep 2 :bp 1 :ec 3} " "]
   [:quote @{:bl 1 :el 1 :bc 3 :ep 10 :bp 2 :ec 11}
    [:bracket-tuple @{:bl 1 :el 1 :bc 4 :ep 10 :bp 3 :ec 11}
     [:keyword @{:bl 1 :el 1 :bc 5 :ep 6 :bp 4 :ec 7} ":a"]
     [:whitespace @{:bl 1 :el 1 :bc 7 :ep 7 :bp 6 :ec 8} " "]
     [:keyword @{:bl 1 :el 1 :bc 8 :ep 9 :bp 7 :ec 10} ":b"]]]]

  )

(def j/l/loc-top-level-ast
  (put (table ;(kvs j/l/loc-grammar))
       :main ~(sequence (line) (column) (position)
                        :input
                        (line) (column) (position))))

(defn j/l/par
  [src &opt start single]
  (default start 0)
  (if single
    (if-let [[bl bc bp tree el ec ep]
             (peg/match j/l/loc-top-level-ast src start)]
      @[:code (j/l/make-attrs bl bc bp el ec ep) tree]
      @[:code])
    (if-let [captures (peg/match j/l/loc-grammar src start)]
      (let [[bl bc bp] (slice captures 0 3)
            [el ec ep] (slice captures -4)
            trees (array/slice captures 3 -4)]
        (array/insert trees 0
                      :code (j/l/make-attrs bl bc bp el ec ep)))
      @[:code])))

# XXX: backward compatibility
(def j/l/ast j/l/par)

(comment

  (j/l/par "(+ 1 1)")
  # =>
  @[:code @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}
    [:tuple @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}
     [:symbol @{:bl 1 :el 1 :bc 2 :ep 2 :bp 1 :ec 3} "+"]
     [:whitespace @{:bl 1 :el 1 :bc 3 :ep 3 :bp 2 :ec 4} " "]
     [:number @{:bl 1 :el 1 :bc 4 :ep 4 :bp 3 :ec 5} "1"]
     [:whitespace @{:bl 1 :el 1 :bc 5 :ep 5 :bp 4 :ec 6} " "]
     [:number @{:bl 1 :el 1 :bc 6 :ep 6 :bp 5 :ec 7} "1"]]]

  )

(defn j/l/gen*
  [an-ast buf]
  (case (first an-ast)
    :code
    (each elt (drop 2 an-ast)
      (j/l/gen* elt buf))
    #
    :buffer
    (buffer/push-string buf (in an-ast 2))
    :comment
    (buffer/push-string buf (in an-ast 2))
    :constant
    (buffer/push-string buf (in an-ast 2))
    :keyword
    (buffer/push-string buf (in an-ast 2))
    :long-buffer
    (buffer/push-string buf (in an-ast 2))
    :long-string
    (buffer/push-string buf (in an-ast 2))
    :number
    (buffer/push-string buf (in an-ast 2))
    :string
    (buffer/push-string buf (in an-ast 2))
    :symbol
    (buffer/push-string buf (in an-ast 2))
    :whitespace
    (buffer/push-string buf (in an-ast 2))
    #
    :array
    (do
      (buffer/push-string buf "@(")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf))
      (buffer/push-string buf ")"))
    :bracket-array
    (do
      (buffer/push-string buf "@[")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf))
      (buffer/push-string buf "]"))
    :bracket-tuple
    (do
      (buffer/push-string buf "[")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf))
      (buffer/push-string buf "]"))
    :tuple
    (do
      (buffer/push-string buf "(")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf))
      (buffer/push-string buf ")"))
    :struct
    (do
      (buffer/push-string buf "{")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf))
      (buffer/push-string buf "}"))
    :table
    (do
      (buffer/push-string buf "@{")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf))
      (buffer/push-string buf "}"))
    #
    :fn
    (do
      (buffer/push-string buf "|")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf)))
    :quasiquote
    (do
      (buffer/push-string buf "~")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf)))
    :quote
    (do
      (buffer/push-string buf "'")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf)))
    :splice
    (do
      (buffer/push-string buf ";")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf)))
    :unquote
    (do
      (buffer/push-string buf ",")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf)))
    ))

(defn j/l/gen
  [an-ast]
  (let [buf @""]
    (j/l/gen* an-ast buf)
    # XXX: leave as buffer?
    (string buf)))

# XXX: backward compatibility
(def j/l/code j/l/gen)

(comment

  (j/l/gen [:code])
  # =>
  ""

  (j/l/gen [:whitespace @{:bc 1 :bl 1 :bp 0
                      :ec 2 :el 1 :ep 1} " "])
  # =>
  " "

  (j/l/gen [:buffer @{:bc 1 :bl 1 :bp 0
                  :ec 12 :el 1 :ep 11} "@\"a buffer\""])
  # =>
  `@"a buffer"`

  (j/l/gen @[:code @{:bc 1 :bl 1 :bp 0
                 :ec 8 :el 1 :ep 7}
         [:tuple @{:bc 1 :bl 1 :bp 0
                   :ec 8 :el 1 :ep 7}
                 [:symbol @{:bc 2 :bl 1 :bp 1
                            :ec 3 :el 1 :ep 2} "+"]
                 [:whitespace @{:bc 3 :bl 1 :bp 2
                                :ec 4 :el 1 :ep 3} " "]
                 [:number @{:bc 4 :bl 1 :bp 3
                            :ec 5 :el 1 :ep 4} "1"]
                 [:whitespace @{:bc 5 :bl 1 :bp 4
                                :ec 6 :el 1 :ep 5} " "]
                 [:number @{:bc 6 :bl 1 :bp 5
                            :ec 7 :el 1 :ep 6} "1"]]])
  # =>
  "(+ 1 1)"

  )

(comment

  (def src "{:x  :y \n :z  [:a  :b    :c]}")

  (j/l/gen (j/l/par src))
  # =>
  src

  )

(comment

  (comment

    (let [src (slurp (string (os/getenv "HOME")
                             "/src/janet/src/boot/boot.janet"))]
      (= (string src)
         (j/l/gen (j/l/par src))))

    )

  )


(def j/version "2026-03-23_01-50-41")

# exports
(def j/par j/l/par)
(def j/gen j/l/gen)

########################################################################

(defn j/zipper
  ``
  Returns a new zipper consisting of two elements:

  * `a-root` - the passed in root node.
  * `state` - table of info about node's z-location in the tree with keys:
    * `:ls` - left siblings
    * `:pnodes` - path of nodes from root to current z-location
    * `:pstate` - parent node's state
    * `:rs` - right siblings
    * `:changed?` - indicates whether "editing" has occured

  `state` has a prototype table with four functions:

  * :branch? - fn that tests if a node is a branch (has children)
  * :children - fn that returns the child nodes for the given branch.
  * :make-node - fn that takes a node + children and returns a new branch
    node with the same.
  * :make-state - fn for creating a new state
  ``
  [a-root branch?-fn children-fn make-node-fn]
  #
  (defn make-state_
    [&opt ls_ rs_ pnodes_ pstate_ changed?_]
    (table/setproto @{:ls ls_
                      :pnodes pnodes_
                      :pstate pstate_
                      :rs rs_
                      :changed? changed?_}
                    @{:branch? branch?-fn
                      :children children-fn
                      :make-node make-node-fn
                      :make-state make-state_}))
  #
  [a-root (make-state_)])

(comment

  # XXX

  )

(defn j/indexed-zip
  ``
  Returns a zipper for nested indexed data structures (tuples
  or arrays), given a root data structure.
  ``
  [indexed]
  (j/zipper indexed
          indexed?
          j/h/to-entries
          (fn [_p xs] xs)))

(comment

  (def a-node
    [:x [:y :z]])

  (def [the-node the-state]
    (j/indexed-zip a-node))

  the-node
  # =>
  a-node

  # merge is used to "remove" the prototype table of `st`
  (merge {} the-state)
  # =>
  @{}

  )

(defn j/node
  "Returns the node at `zloc`."
  [zloc]
  (get zloc 0))

(comment

  (j/node (j/indexed-zip [:a :b [:x :y]]))
  # =>
  [:a :b [:x :y]]

  )

(defn j/state
  "Returns the state for `zloc`."
  [zloc]
  (get zloc 1))

(comment

  # merge is used to "remove" the prototype table of `st`
  (merge {}
         (-> (j/indexed-zip [:a [:b [:x :y]]])
             j/state))
  # =>
  @{}

  )

(defn j/branch?
  ``
  Returns true if the node at `zloc` is a branch.
  Returns false otherwise.
  ``
  [zloc]
  (((j/state zloc) :branch?) (j/node zloc)))

(comment

  (j/branch? (j/indexed-zip [:a :b [:x :y]]))
  # =>
  true

  )

(defn j/children
  ``
  Returns children for a branch node at `zloc`.
  Otherwise throws an error.
  ``
  [zloc]
  (if (j/branch? zloc)
    (((j/state zloc) :children) (j/node zloc))
    (error "Called `children` on a non-branch zloc")))

(comment

  (j/children (j/indexed-zip [:a :b [:x :y]]))
  # =>
  [:a :b [:x :y]]

  )

(defn j/make-state
  ``
  Convenience function for calling the :make-state function for `zloc`.
  ``
  [zloc &opt ls rs pnodes pstate changed?]
  (((j/state zloc) :make-state) ls rs pnodes pstate changed?))

(comment

  # merge is used to "remove" the prototype table of `st`
  (merge {}
         (j/make-state (j/indexed-zip [:a :b [:x :y]])))
  # =>
  @{}

  )

(defn j/down
  ``
  Moves down the tree, returning the leftmost child z-location of
  `zloc`, or nil if there are no children.
  ``
  [zloc]
  (when (j/branch? zloc)
    (let [[z-node st] zloc
          [k rest-kids kids]
          (j/h/first-rest-maybe-all (j/children zloc))]
      (when kids
        [k
         (j/make-state zloc
                     []
                     rest-kids
                     (if (not (empty? st))
                       (j/h/tuple-push (get st :pnodes) z-node)
                       [z-node])
                     st
                     (get st :changed?))]))))

(comment

  (j/node (j/down (j/indexed-zip [:a :b [:x :y]])))
  # =>
  :a

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/branch?)
  # =>
  false

  (try
    (-> (j/indexed-zip [:a])
        j/down
        j/children)
    ([e] e))
  # =>
  "Called `children` on a non-branch zloc"

  (deep=
    #
    (merge {}
           (-> [:a [:b [:x :y]]]
               j/indexed-zip
               j/down
               j/state))
    #
    '@{:ls ()
       :pnodes ((:a (:b (:x :y))))
       :pstate @{}
       :rs ((:b (:x :y)))})
  # =>
  true

  )

(defn j/right
  ``
  Returns the z-location of the right sibling of the node
  at `zloc`, or nil if there is no such sibling.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st
        [r rest-rs rs_] (j/h/first-rest-maybe-all rs)]
    (when (and (not (empty? st)) rs_)
      [r
       (j/make-state zloc
                   (j/h/tuple-push ls z-node)
                   rest-rs
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))])))

(comment

  (-> (j/indexed-zip [:a :b])
      j/down
      j/right
      j/node)
  # =>
  :b

  (-> (j/indexed-zip [:a])
      j/down
      j/right)
  # =>
  nil

  )

(defn j/make-node
  ``
  Returns a branch node, given `zloc`, `a-node` and `kids`.
  ``
  [zloc a-node kids]
  (((j/state zloc) :make-node) a-node kids))

(comment

  (j/make-node (j/indexed-zip [:a :b [:x :y]])
             [:a :b] [:x :y])
  # =>
  [:x :y]

  )

(defn j/up
  ``
  Moves up the tree, returning the parent z-location of `zloc`,
  or nil if at the root z-location.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls
         :pnodes pnodes
         :pstate pstate
         :rs rs
         :changed? changed?} st]
    (when pnodes
      (let [pnode (last pnodes)]
        (if changed?
          [(j/make-node zloc pnode [;ls z-node ;rs])
           (j/make-state zloc
                       (get pstate :ls)
                       (get pstate :rs)
                       (get pstate :pnodes)
                       (get pstate :pstate)
                       true)]
          [pnode pstate])))))

(comment

  (def m-zip
    (j/indexed-zip [:a :b [:x :y]]))

  (deep=
    (-> m-zip
        j/down
        j/up)
    m-zip)
  # =>
  true

  (deep=
    (-> m-zip
        j/down
        j/right
        j/right
        j/down
        j/up
        j/up)
    m-zip)
  # =>
  true

  )

# XXX: used by `root` and `df-next`
(defn j/end?
  "Returns true if `zloc` represents the end of a depth-first walk."
  [zloc]
  (= :end (j/state zloc)))

(defn j/root
  ``
  Moves all the way up the tree for `zloc` and returns the node at
  the root z-location.
  ``
  [zloc]
  (if (j/end? zloc)
    (j/node zloc)
    (if-let [p (j/up zloc)]
      (j/root p)
      (j/node zloc))))

(comment

  (def a-zip
    (j/indexed-zip [:a :b [:x :y]]))

  (j/node a-zip)
  # =>
  (-> a-zip
      j/down
      j/right
      j/right
      j/down
      j/root)

  )

(defn j/df-next
  ``
  Moves to the next z-location, depth-first.  When the end is
  reached, returns a special z-location detectable via `end?`.
  Does not move if already at the end.
  ``
  [zloc]
  #
  (defn recur
    [a-loc]
    (if (j/up a-loc)
      (or (j/right (j/up a-loc))
          (recur (j/up a-loc)))
      [(j/node a-loc) :end]))
  #
  (if (j/end? zloc)
    zloc
    (or (and (j/branch? zloc) (j/down zloc))
        (j/right zloc)
        (recur zloc))))

(comment

  (def a-zip
    (j/indexed-zip [:a :b [:x]]))

  (j/node (j/df-next a-zip))
  # =>
  :a

  (-> a-zip
      j/df-next
      j/df-next
      j/node)
  # =>
  :b

  (-> a-zip
      j/df-next
      j/df-next
      j/df-next
      j/df-next
      j/df-next
      j/end?)
  # =>
  true

  )

(defn j/replace
  "Replaces existing node at `zloc` with `a-node`, without moving."
  [zloc a-node]
  (let [[_ st] zloc]
    [a-node
     (j/make-state zloc
                 (get st :ls)
                 (get st :rs)
                 (get st :pnodes)
                 (get st :pstate)
                 true)]))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      (j/replace :w)
      j/root)
  # =>
  [:w :b [:x :y]]

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/right
      j/down
      (j/replace :w)
      j/root)
  # =>
  [:a :b [:w :y]]

  )

(defn j/edit
  ``
  Replaces the node at `zloc` with the value of `(f node args)`,
  where `node` is the node associated with `zloc`.
  ``
  [zloc f & args]
  (j/replace zloc
           (apply f (j/node zloc) args)))

(comment

  (-> (j/indexed-zip [1 2 [8 9]])
      j/down
      (j/edit inc)
      j/root)
  # =>
  [2 2 [8 9]]

  (-> (j/indexed-zip [1 2 [8 9]])
      j/down
      (j/edit inc)
      j/right
      (j/edit inc)
      j/right
      j/down
      (j/edit dec)
      j/right
      (j/edit dec)
      j/root)
  # =>
  [2 3 [7 8]]

  )

(defn j/insert-child
  ``
  Inserts `child` as the leftmost child of the node at `zloc`,
  without moving.
  ``
  [zloc child]
  (j/replace zloc
           (j/make-node zloc
                      (j/node zloc)
                      [child ;(j/children zloc)])))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      (j/insert-child :c)
      j/root)
  # =>
  [:c :a :b [:x :y]]

  )

(defn j/append-child
  ``
  Appends `child` as the rightmost child of the node at `zloc`,
  without moving.
  ``
  [zloc child]
  (j/replace zloc
           (j/make-node zloc
                      (j/node zloc)
                      [;(j/children zloc) child])))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      (j/append-child :c)
      j/root)
  # =>
  [:a :b [:x :y] :c]

  )

(defn j/rightmost
  ``
  Returns the z-location of the rightmost sibling of the node at
  `zloc`, or the current node's z-location if there are none to the
  right.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (and (not (empty? st))
             (indexed? rs)
             (not (empty? rs)))
      [(last rs)
       (j/make-state zloc
                   (j/h/tuple-push ls z-node ;(j/h/butlast rs))
                   []
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))]
      zloc)))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/rightmost
      j/node)
  # =>
  [:x :y]

  )

(defn j/remove
  ``
  Removes the node at `zloc`, returning the z-location that would have
  preceded it in a depth-first walk.  Throws an error if called at the
  root z-location.
  ``
  [zloc]
  (let [[_z-node st] zloc
        {:ls ls
         :pnodes pnodes
         :pstate pstate
         :rs rs} st]
    #
    (defn recur
      [a-zloc]
      (if-let [child (and (j/branch? a-zloc) (j/down a-zloc))]
        (recur (j/rightmost child))
        a-zloc))
    #
    (if (not (empty? st))
      (if (pos? (length ls))
        (recur [(last ls)
                (j/make-state zloc
                            (j/h/butlast ls)
                            rs
                            pnodes
                            pstate
                            true)])
        [(j/make-node zloc (last pnodes) rs)
         (j/make-state zloc
                     (get pstate :ls)
                     (get pstate :rs)
                     (get pstate :pnodes)
                     (get pstate :pstate)
                     true)])
      (error "Called `remove` at root"))))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/remove
      j/node)
  # =>
  :a

  (try
    (j/remove (j/indexed-zip [:a :b [:x :y]]))
    ([e] e))
  # =>
  "Called `remove` at root"

  )

(defn j/left
  ``
  Returns the z-location of the left sibling of the node
  at `zloc`, or nil if there is no such sibling.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (when (and (not (empty? st))
               (indexed? ls)
               (not (empty? ls)))
      [(last ls)
       (j/make-state zloc
                   (j/h/butlast ls)
                   [z-node ;rs]
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))])))

(comment

  (-> (j/indexed-zip [:a :b :c])
      j/down
      j/right
      j/right
      j/left
      j/node)
  # =>
  :b

  (-> (j/indexed-zip [:a])
      j/down
      j/left)
  # =>
  nil

  )

(defn j/df-prev
  ``
  Moves to the previous z-location, depth-first.
  If already at the root, returns nil.
  ``
  [zloc]
  #
  (defn recur
    [a-zloc]
    (if-let [child (and (j/branch? a-zloc)
                        (j/down a-zloc))]
      (recur (j/rightmost child))
      a-zloc))
  #
  (if-let [left-loc (j/left zloc)]
    (recur left-loc)
    (j/up zloc)))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/df-prev
      j/node)
  # =>
  :a

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/right
      j/down
      j/df-prev
      j/node)
  # =>
  [:x :y]

  )

(defn j/insert-right
  ``
  Inserts `a-node` as the right sibling of the node at `zloc`,
  without moving.
  ``
  [zloc a-node]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (not (empty? st))
      [z-node
       (j/make-state zloc
                   ls
                   [a-node ;rs]
                   (get st :pnodes)
                   (get st :pstate)
                   true)]
      (error "Called `insert-right` at root"))))

(comment

  (def a-zip
    (j/indexed-zip [:a :b [:x :y]]))

  (-> a-zip
      j/down
      (j/insert-right :z)
      j/root)
  # =>
  [:a :z :b [:x :y]]

  (try
    (j/insert-right a-zip :e)
    ([e] e))
  # =>
  "Called `insert-right` at root"

  )

(defn j/insert-left
  ``
  Inserts `a-node` as the left sibling of the node at `zloc`,
  without moving.
  ``
  [zloc a-node]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (not (empty? st))
      [z-node
       (j/make-state zloc
                   (j/h/tuple-push ls a-node)
                   rs
                   (get st :pnodes)
                   (get st :pstate)
                   true)]
      (error "Called `insert-left` at root"))))

(comment

  (def a-zip
    (j/indexed-zip [:a :b [:x :y]]))

  (-> a-zip
      j/down
      (j/insert-left :z)
      j/root)
  # =>
  [:z :a :b [:x :y]]

  (try
    (j/insert-left a-zip :e)
    ([e] e))
  # =>
  "Called `insert-left` at root"

  )

(defn j/rights
  "Returns siblings to the right of `zloc`."
  [zloc]
  (when-let [st (j/state zloc)]
    (get st :rs)))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/rights)
  # =>
  [:b [:x :y]]

  (-> (j/indexed-zip [:a :b])
      j/down
      j/right
      j/rights)
  # =>
  []

  )

(defn j/lefts
  "Returns siblings to the left of `zloc`."
  [zloc]
  (if-let [st (j/state zloc)
           ls (get st :ls)]
    ls
    []))

(comment

  (-> (j/indexed-zip [:a :b])
      j/down
      j/lefts)
  # =>
  []

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/right
      j/lefts)
  # =>
  [:a :b]

  )

(defn j/leftmost
  ``
  Returns the z-location of the leftmost sibling of the node at `zloc`,
  or the current node's z-location if there are no siblings to the left.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (and (not (empty? st))
             (indexed? ls)
             (not (empty? ls)))
      [(first ls)
       (j/make-state zloc
                   []
                   [;(j/h/rest ls) z-node ;rs]
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))]
      zloc)))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/leftmost
      j/node)
  # =>
  :a

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/rightmost
      j/leftmost
      j/node)
  # =>
  :a

  )

(defn j/path
  "Returns the path of nodes that lead to `zloc` from the root node."
  [zloc]
  (when-let [st (j/state zloc)]
    (get st :pnodes)))

(comment

  (j/path (j/indexed-zip [:a :b [:x :y]]))
  # =>
  nil

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/path)
  # =>
  [[:a :b [:x :y]]]

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/right
      j/down
      j/path)
  # =>
  [[:a :b [:x :y]] [:x :y]]

  )

(defn j/right-until
  ``
  Try to move right from `zloc`, calling `pred` for each
  right sibling.  If the `pred` call has a truthy result,
  return the corresponding right sibling.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when-let [right-sib (j/right zloc)]
    (if (pred right-sib)
      right-sib
      (j/right-until right-sib pred))))

(comment

  (-> [:code
       [:tuple
        [:comment "# hi there"] [:whitespace "\n"]
        [:symbol "+"] [:whitespace " "]
        [:number "1"] [:whitespace " "]
        [:number "2"]]]
      j/indexed-zip
      j/down
      j/right
      j/down
      (j/right-until |(match (j/node $)
                      [:comment]
                      false
                      #
                      [:whitespace]
                      false
                      #
                      true))
      j/node)
  # =>
  [:symbol "+"]

  )

(defn j/right-from-until
  ``
  Call `pred` on zloc, and if it fails, successively on
  each right sibling until a truthy result.

  Return the zloc corresponding to the one which `pred`
  returns a truthy result for, if any.  Otherwise, return
  nil.
  ``
  [zloc pred]
  (defn helper
    [a-zloc]
    (when-let [right-sib (j/right a-zloc)]
      (if (pred right-sib)
        right-sib
        (helper right-sib))))
  #
  (if (pred zloc)
    zloc
    (helper zloc)))

(comment

  (-> [:code
       [:bracket-tuple
        [:number "1"] [:whitespace " "]
        [:number "2"]]]
      j/indexed-zip
      j/down
      j/right
      j/down
      j/right
      (j/right-from-until |(match (j/node $)
                           [:number]
                           true
                           #
                           false))
      j/node)
  # =>
  [:number "1"]

  (-> [:code
       [:tuple
        [:comment "# hi there"] [:whitespace "\n"]
        [:symbol "+"] [:whitespace " "]
        [:number "1"] [:whitespace " "]
        [:number "2"]]]
      j/indexed-zip
      j/down
      j/right
      j/down
      j/right
      (j/right-from-until |(match (j/node $)
                           [:number]
                           true
                           #
                           false))
      j/node)
  # =>
  [:number "1"]

  )

(defn j/left-until
  ``
  Try to move left from `zloc`, calling `pred` for each
  left sibling.  If the `pred` call has a truthy result,
  return the corresponding left sibling.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when-let [left-sib (j/left zloc)]
    (if (pred left-sib)
      left-sib
      (j/left-until left-sib pred))))

(comment

  (-> [:code
       [:tuple
        [:comment "# hi there"] [:whitespace "\n"]
        [:symbol "+"] [:whitespace " "]
        [:number "1"] [:whitespace " "]
        [:number "2"]]]
      j/indexed-zip
      j/down
      j/right
      j/down
      j/rightmost
      (j/left-until |(match (j/node $)
                     [:comment]
                     false
                     #
                     [:whitespace]
                     false
                     #
                     true))
      j/node)
  # =>
  [:number "1"]

  )

(defn j/search-from
  ``
  Successively call `pred` on z-locations starting at `zloc`
  in depth-first order.  If a call to `pred` returns a
  truthy value, return the corresponding z-location.
  Otherwise, return nil.
  ``
  [zloc pred]
  (if (pred zloc)
    zloc
    (when-let [next-zloc (j/df-next zloc)]
      (when (j/end? next-zloc)
        (break nil))
      (j/search-from next-zloc pred))))

(comment

  (-> (j/indexed-zip [:a :b :c])
      j/down
      (j/search-from |(match (j/node $)
                      :b
                      true))
      j/node)
  # =>
  :b

  (-> (j/indexed-zip [:a :b :c])
      j/down
      (j/search-from |(match (j/node $)
                      :d
                      true)))
  # =>
  nil

  (-> (j/indexed-zip [:a :b :c])
      j/down
      (j/search-from |(match (j/node $)
                      :a
                      true))
      j/node)
  # =>
  :a

  )

(defn j/search-after
  ``
  Successively call `pred` on z-locations starting after
  `zloc` in depth-first order.  If a call to `pred` returns a
  truthy value, return the corresponding z-location.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when (j/end? zloc)
    (break nil))
  (when-let [next-zloc (j/df-next zloc)]
    (if (pred next-zloc)
      next-zloc
      (j/search-after next-zloc pred))))

(comment

  (-> (j/indexed-zip [:b :a :b])
      j/down
      (j/search-after |(match (j/node $)
                       :b
                       true))
      j/left
      j/node)
  # =>
  :a

  (-> (j/indexed-zip [:b :a :b])
      j/down
      (j/search-after |(match (j/node $)
                       :d
                       true)))
  # =>
  nil

  (-> (j/indexed-zip [:a [:b :c [2 [3 :smile] 5]]])
      (j/search-after |(match (j/node $)
                       [_ :smile]
                       true))
      j/down
      j/node)
  # =>
  3

  )

(defn j/unwrap
  ``
  If the node at `zloc` is a branch node, "unwrap" its children in
  place.  If `zloc`'s node is not a branch node, do nothing.

  Throws an error if `zloc` corresponds to a top-most container.
  ``
  [zloc]
  (unless (j/branch? zloc)
    (break zloc))
  #
  (when (empty? (j/state zloc))
    (error "Called `unwrap` at root"))
  #
  (def kids (j/children zloc))
  (var i (dec (length kids)))
  (var curr-zloc zloc)
  (while (<= 0 i) # right to left
    (set curr-zloc
         (j/insert-right curr-zloc (get kids i)))
    (-- i))
  # try to end up at a sensible spot
  (set curr-zloc
       (j/remove curr-zloc))
  (if-let [ret-zloc (j/right curr-zloc)]
    ret-zloc
    curr-zloc))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/right
      j/unwrap
      j/root)
  # =>
  [:a :b :x :y]

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/unwrap
      j/root)
  # =>
  [:a :b [:x :y]]

  (-> (j/indexed-zip [[:a]])
      j/down
      j/unwrap
      j/root)
  # =>
  [:a]

  (-> (j/indexed-zip [[:a :b] [:x :y]])
      j/down
      j/down
      j/remove
      j/unwrap
      j/root)
  # =>
  [:b [:x :y]]

  (try
    (-> (j/indexed-zip [:a :b [:x :y]])
        j/unwrap)
    ([e] e))
  # =>
  "Called `unwrap` at root"

  )

(defn j/eq?
  ``
  Compare two zlocs, `a-zloc` and `b-zloc`, for equality.
  ``
  [a-zloc b-zloc]
  (and (= (length (j/lefts a-zloc)) (length (j/lefts b-zloc)))
       (= (j/path a-zloc) (j/path b-zloc))))

(comment

  (def iz (j/indexed-zip [:a :b :c :b]))

  (j/eq? (-> iz j/down j/right)
       (-> iz j/down j/right j/right j/right))
  # =>
  false

  (j/eq? (-> iz j/down j/right)
       (-> iz j/down j/right j/right j/right j/left j/left))
  # =>
  true

  )

(defn j/wrap
  ``
  Replace nodes from `start-zloc` through `end-zloc` with a single
  node of the same type as `wrap-node` containing the nodes from
  `start-zloc` through `end-zloc`.

  If `end-zloc` is not specified, just wrap `start-zloc`.

  The caller is responsible for ensuring the value of `end-zloc`
  is somewhere to the right of `start-zloc`.  Throws an error if
  an inappropriate value is specified for `end-zloc`.
  ``
  [start-zloc wrap-node &opt end-zloc]
  (default end-zloc start-zloc)
  #
  # 1. collect all nodes to wrap
  #
  (def kids @[])
  (var cur-zloc start-zloc)
  (while (and cur-zloc
              (not (j/eq? cur-zloc end-zloc))) # left to right
    (array/push kids (j/node cur-zloc))
    (set cur-zloc (j/right cur-zloc)))
  (when (nil? cur-zloc)
    (error "Called `wrap` with invalid value for `end-zloc`."))
  # also collect the last node
  (array/push kids (j/node end-zloc))
  #
  # 2. replace locations that will be removed with non-container nodes
  #
  (def dummy-node
    (j/make-node start-zloc wrap-node (tuple)))
  (set cur-zloc start-zloc)
  # trying to do this together in step 1 is not straight-forward
  # because the desired exiting condition for the while loop depends
  # on cur-zloc becoming end-zloc -- if `replace` were to be used
  # there, the termination condition never gets fulfilled properly.
  (repeat (dec (length kids)) # left to right again
    (set cur-zloc
         (-> (j/replace cur-zloc dummy-node)
             j/right)))
  (set cur-zloc
       (j/replace cur-zloc dummy-node))
  #
  # 3. remove all relevant locations
  #
  (def new-node
    (j/make-node start-zloc wrap-node (tuple ;kids)))
  (repeat (dec (length kids)) # right to left
    (set cur-zloc
         (j/remove cur-zloc)))
  # 4. put the new container node into place
  (j/replace cur-zloc new-node))

(comment

  (def start-zloc
    (-> (j/indexed-zip [:a [:b] :c :x])
        j/down
        j/right))

  (j/node start-zloc)
  # =>
  [:b]

  (-> (j/wrap start-zloc [])
      j/root)
  # =>
  [:a [[:b]] :c :x]

  (def end-zloc
    (j/right start-zloc))

  (j/node end-zloc)
  # =>
  :c

  (-> (j/wrap start-zloc [] end-zloc)
      j/root)
  # =>
  [:a [[:b] :c] :x]

  (try
    (-> (j/wrap end-zloc [] start-zloc)
        j/root)
    ([e] e))
  # =>
  "Called `wrap` with invalid value for `end-zloc`."

  )

########################################################################

(defn j/has-children?
  ``
  Returns true if `a-node` can have children.
  Returns false if `a-node` cannot have children.
  ``
  [a-node]
  (when-let [[head] a-node]
    (truthy? (get {:code true
                   :fn true
                   :quasiquote true
                   :quote true
                   :splice true
                   :unquote true
                   :array true
                   :tuple true
                   :bracket-array true
                   :bracket-tuple true
                   :table true
                   :struct true}
                  head))))

(comment

  (j/has-children?
    [:tuple @{}
     [:symbol @{} "+"] [:whitespace @{} " "]
     [:number @{} "1"] [:whitespace @{} " "]
     [:number @{} "2"]])
  # =>
  true

  (j/has-children? [:number @{} "8"])
  # =>
  false

  )

(defn j/zip
  ``
  Returns a zipper location (zloc or z-location) for a tree
  representing Janet code.
  ``
  [a-tree]
  (defn branch?_
    [a-node]
    (truthy? (and (indexed? a-node)
                  (not (empty? a-node))
                  (j/has-children? a-node))))
  #
  (defn children_
    [a-node]
    (if (branch?_ a-node)
      (slice a-node 2)
      (error "Called `children` on a non-branch node")))
  #
  (defn make-node_
    [a-node kids]
    [(first a-node) (get a-node 1) ;kids])
  #
  (j/zipper a-tree branch?_ children_ make-node_))

(comment

  (def root-node
    @[:code @{} [:number @{} "8"]])

  (def [the-node the-state]
    (j/zip root-node))

  the-node
  # =>
  root-node

  # merge is used to "remove" the prototype table of `st`
  (merge {} the-state)
  # =>
  @{}

  )

(defn j/attrs
  ``
  Return the attributes table for the node of a z-location.  The
  attributes table contains at least bounds of the node by 1-based line
  and column numbers along with 0-based positions.
  ``
  [zloc]
  (get (j/node zloc) 1))

(comment

  (-> (j/par "(+ 1 3)")
      j/zip
      j/down
      j/attrs)
  # =>
  @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}

  )

(defn j/zip-down
  ``
  Convenience function that returns a zipper which has
  already had `down` called on it.
  ``
  [a-tree]
  (-> (j/zip a-tree)
      j/down))

(comment

  (-> (j/par "(+ 1 3)")
      j/zip-down
      j/node)
  # =>
  [:tuple @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}
   [:symbol @{:bl 1 :el 1 :bc 2 :ep 2 :bp 1 :ec 3} "+"]
   [:whitespace @{:bl 1 :el 1 :bc 3 :ep 3 :bp 2 :ec 4} " "]
   [:number @{:bl 1 :el 1 :bc 4 :ep 4 :bp 3 :ec 5} "1"]
   [:whitespace @{:bl 1 :el 1 :bc 5 :ep 5 :bp 4 :ec 6} " "]
   [:number @{:bl 1 :el 1 :bc 6 :ep 6 :bp 5 :ec 7} "3"]]

  (-> (j/par "(/ 1 8)")
      j/zip-down
      j/root)
  # =>
  @[:code @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}
    [:tuple @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}
     [:symbol @{:bl 1 :el 1 :bc 2 :ep 2 :bp 1 :ec 3} "/"]
     [:whitespace @{:bl 1 :el 1 :bc 3 :ep 3 :bp 2 :ec 4} " "]
     [:number @{:bl 1 :el 1 :bc 4 :ep 4 :bp 3 :ec 5} "1"]
     [:whitespace @{:bl 1 :el 1 :bc 5 :ep 5 :bp 4 :ec 6} " "]
     [:number @{:bl 1 :el 1 :bc 6 :ep 6 :bp 5 :ec 7} "8"]]]

  )

# wsc == whitespace, comment
(defn j/right-skip-wsc
  ``
  Try to move right from `zloc`, skipping over whitespace
  and comment nodes.

  When at least one right move succeeds, return the z-location
  for the last successful right move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (j/right-until zloc
               |(match (j/node $)
                  [:whitespace]
                  false
                  #
                  [:comment]
                  false
                  #
                  true)))

(comment

  (-> (j/par (string "(# hi there\n"
                   "+ 1 2)"))
      j/zip-down
      j/down
      j/right-skip-wsc
      j/node)
  # =>
  [:symbol @{:bl 2 :el 2 :bc 1 :ep 13 :bp 12 :ec 2} "+"]

  (-> (j/par "(:a)")
      j/zip-down
      j/down
      j/right-skip-wsc)
  # =>
  nil

  )

(defn j/left-skip-wsc
  ``
  Try to move left from `zloc`, skipping over whitespace
  and comment nodes.

  When at least one left move succeeds, return the z-location
  for the last successful left move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (j/left-until zloc
              |(match (j/node $)
                 [:whitespace]
                 false
                 #
                 [:comment]
                 false
                 #
                 true)))

(comment

  (-> (j/par (string "(# hi there\n"
                   "+ 1 2)"))
      j/zip-down
      j/down
      j/right-skip-wsc
      j/right-skip-wsc
      j/left-skip-wsc
      j/node)
  # =>
  [:symbol @{:bl 2 :el 2 :bc 1 :ep 13 :bp 12 :ec 2} "+"]

  (-> (j/par "(:a)")
      j/zip-down
      j/down
      j/left-skip-wsc)
  # =>
  nil

  )

(defn j/down-skip-wsc
  ``
  Try to move down from `zloc`, skipping over any whitespace
  and comment nodes that might exist immediately after
  moving down.

  If successful in finding a non-whitespace, non-comment node,
  return the corresponding z-location.  Otherwise, return nil.
  ``
  [zloc]
  (def d-zloc (j/down zloc))
  (when d-zloc
    (if (match (j/node d-zloc)
          [:whitespace]
          true
          #
          [:comment]
          true
          #
          false)
      (j/right-skip-wsc d-zloc)
      d-zloc)))

(comment

  (-> (j/par (string "(# hi there\n"
                   "+ 1 2)"))
      j/zip-down
      j/down-skip-wsc
      j/node)
  # =>
  [:symbol @{:bc 1 :bl 2 :bp 12 :ec 2 :el 2 :ep 13} "+"]

  (-> (j/par (string "()"))
      j/zip-down
      j/down-skip-wsc
      j/node)
  # =>
  nil

  (-> (j/par (string "(# a comment\n"
                   ")"))
      j/zip-down
      j/down-skip-wsc
      j/node)
  # =>
  nil

  (-> (j/par (string "( )"))
      j/zip-down
      j/down-skip-wsc
      j/node)
  # =>
  nil

  (-> (j/par (string "(\n"
                   "# a comment\n"
                   ")"))
      j/zip-down
      j/down-skip-wsc
      j/node)
  # =>
  nil

  (-> (j/par (string "{:a 1}"))
      j/zip-down
      j/down-skip-wsc
      j/node)
  # =>
  [:keyword @{:bc 2 :bl 1 :bp 1 :ec 4 :el 1 :ep 3} ":a"]

  )

# ws == whitespace
(defn j/right-skip-ws
  ``
  Try to move right from `zloc`, skipping over whitespace
  nodes.

  When at least one right move succeeds, return the z-location
  for the last successful right move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (j/right-until zloc
               |(match (j/node $)
                  [:whitespace]
                  false
                  #
                  true)))

(comment

  (-> (j/par (string "( # hi there\n"
                   "+ 1 2)"))
      j/zip-down
      j/down
      j/right-skip-ws
      j/node)
  # =>
  [:comment @{:bl 1 :el 1 :bc 3 :ep 12 :bp 2 :ec 13} "# hi there"]

  (-> (j/par "(:a)")
      j/zip-down
      j/down
      j/right-skip-ws)
  # =>
  nil

  )

(defn j/left-skip-ws
  ``
  Try to move left from `zloc`, skipping over whitespace
  nodes.

  When at least one left move succeeds, return the z-location
  for the last successful left move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (j/left-until zloc
              |(match (j/node $)
                 [:whitespace]
                 false
                 #
                 true)))

(comment

  (-> (j/par (string "(# hi there\n"
                   "+ 1 2)"))
      j/zip-down
      j/down
      j/right
      j/right
      j/left-skip-ws
      j/node)
  # =>
  [:comment @{:bl 1 :el 1 :bc 2 :ep 11 :bp 1 :ec 12} "# hi there"]

  (-> (j/par "(:a)")
      j/zip-down
      j/down
      j/left-skip-ws)
  # =>
  nil

  )



# XXX: not sure if should check whether node for zloc is:
#
#      :table, :struct
(defn jg/get-via-key
  ``
  Move to the location of `key`'s associated value in the Janet
  dictionary associated with `zloc`.  Currently, `key` can be a Janet
  keyword (e.g. `:ant`) or a Janet natural number (e.g. `0` or `2`).

  If no such key exists, return nil.  Otherwise return the determined
  location.
  ``

  [zloc key]
  (assertf (or (keyword? key) (nat? key))
           "expected keyword or natural number, found: %n" key)
  #
  (def first-key-zloc (j/down-skip-wsc zloc))
  (when first-key-zloc
    (def key-str (string (if (keyword? key) ":" "") key))
    (var cur-zloc first-key-zloc)
    (while (def cur-node (j/node cur-zloc))
      (when (match cur-node
              [:keyword _ (@ key-str)]
              true
              #
              [:number _ (@ key-str)]
              true
              #
              false)
        (break))
      (set cur-zloc (-> cur-zloc
                        j/right-skip-wsc
                        j/right-skip-wsc)))
    #
    (when cur-zloc
      (def value-zloc (j/right-skip-wsc cur-zloc))
      (assertf value-zloc "failed to find value for key: %n" key)
      #
      value-zloc)))

(comment

  (-> (j/par "{:a 1}")
      j/zip-down
      (jg/get-via-key :a)
      j/node)
  # =>
  [:number @{:bc 5 :bl 1 :bp 4 :ec 6 :el 1 :ep 5} "1"]

  (-> (j/par `{-1 "minus-one" 0 "zero"}`)
      j/zip-down
      (jg/get-via-key 0)
      j/node)
  # =>
  [:string @{:bc 19 :bl 1 :bp 18 :ec 25 :el 1 :ep 24} `"zero"`]

  (-> (j/par "{}")
      j/zip-down
      (jg/get-via-key :a)
      j/node)
  # =>
  nil

  (-> (j/par "{:a 1 :b 2}")
      j/zip-down
      (jg/get-via-key :c)
      j/node)
  # =>
  nil

  (-> (j/par (string "{# hi there\n"
                     " :ant 1\n"
                     " :bee 2}"))
      j/zip-down
      (jg/get-via-key :bee)
      j/node)
  # =>
  [:number @{:bc 7 :bl 3 :bp 26 :ec 8 :el 3 :ep 27} "2"]

  )

# XXX: not sure if should check whether node for zloc is:
#
#      :array, :bracket-array, :tuple, :bracket-tuple
(defn jg/get-via-index
  ``
  Move to the location corresponding to `index` in the Janet indexed
  data structure associated with `zloc`.  `index` should be a
  Janet natural number (e.g. `0` or `11`).

  If no such index exists, return nil.  Otherwise return the
  determined location.
  ``
  [zloc index]
  (assertf (nat? index) "expected natural number, found: %n" index)
  #
  (def first-index-zloc (j/down-skip-wsc zloc))
  (when first-index-zloc
    (var cur-zloc first-index-zloc)
    (repeat index
      (set cur-zloc (j/right-skip-wsc cur-zloc)))
    #
    cur-zloc))

(comment

  (-> (j/par "[:x :y]")
      j/zip-down
      (jg/get-via-index 1)
      j/node)
  # =>
  [:keyword @{:bc 5 :bl 1 :bp 4 :ec 7 :el 1 :ep 6} ":y"]

  (-> (j/par "[]")
      j/zip-down
      (jg/get-via-index 1)
      j/node)
  # =>
  nil

  (-> (j/par "[:ant]")
      j/zip-down
      (jg/get-via-index 1)
      j/node)
  # =>
  nil

  (-> (j/par (string "[# hi there\n"
                     " :ant :bee :cat]"))
      j/zip-down
      (jg/get-via-index 2)
      j/node)
  # =>
  [:keyword @{:bc 12 :bl 2 :bp 23 :ec 16 :el 2 :ep 27} ":cat"]

  )

(defn jg/get-via
  ``
  Dispatch to `get-via-key` if `zloc` represents a dictionary, or to
  `get-via-index` if it represents an indexed data structure.

  Otherwise, error.
  ``
  [zloc id]
  (def the-type (get (j/node zloc) 0))
  (case the-type
    :struct (jg/get-via-key zloc id)
    :table (jg/get-via-key zloc id)
    :array (jg/get-via-index zloc id)
    :bracket-array (jg/get-via-index zloc id)
    :tuple (jg/get-via-index zloc id)
    :bracket-tuple (jg/get-via-index zloc id)
    (errorf "unexpected type: %n" the-type)))

(comment

  (-> (j/par (string "{# hi there\n"
                     " :ant 1\n"
                     " :bee 2}"))
      j/zip-down
      (jg/get-via :bee)
      j/node)
  # =>
  [:number @{:bc 7 :bl 3 :bp 26 :ec 8 :el 3 :ep 27} "2"]

  (-> (j/par `{-1 "minus-one" 0 "zero"}`)
      j/zip-down
      (jg/get-via 0)
      j/node)
  # =>
  [:string @{:bc 19 :bl 1 :bp 18 :ec 25 :el 1 :ep 24} `"zero"`]

  (-> (j/par "{}")
      j/zip-down
      (jg/get-via :a)
      j/node)
  # =>
  nil

  (-> (j/par (string "[# hi there\n"
                     " :ant :bee :cat]"))
      j/zip-down
      (jg/get-via 2)
      j/node)
  # =>
  [:keyword @{:bc 12 :bl 2 :bp 23 :ec 16 :el 2 :ep 27} ":cat"]

  )


(comment import ./jipper :prefix "")


(defn jt/traverse-src
  [src skip path value value-str]
  # prepare zipper for zipper traversal
  (def zloc (-> src j/par j/zip-down))
  (var cur-zloc zloc)
  # if needed, skip to appropriate top-level item
  (repeat skip
    (set cur-zloc (j/right-skip-wsc cur-zloc)))
  # traverse the path, one step at a time
  (each step path
    (set cur-zloc (jg/get-via cur-zloc step)))
  (when (nil? cur-zloc)
    (errorf "unexpected nil value for current location"))
  #
  # prepare replacement
  (def found-value-str (j/gen (j/node cur-zloc)))
  (assertf (deep= value (parse found-value-str))
           "expected: %n, but found: %n" value (parse found-value-str))
  #
  (def v-zloc (-> value-str j/par j/zip-down))
  # replace
  (def e-zloc (j/replace cur-zloc (j/node v-zloc)))
  # generate new source string
  (j/gen (j/root e-zloc)))


(comment import ./traverse :prefix "")
(comment import ./get :prefix "")
(comment

  (def path-str ":vendored 2 :tag")

  (def value `"7caf81f636bb97104aada6544219733b3c86badf"`)

  # bundle/info.jdn
  (def src
    ``
    {:name "ghost"
     :url "https://github.com/sogaiu/ghost"
     :repo "git+https://github.com/sogaiu/ghost"
     :vendored
     [{:name "some-bundle-bits"
       :url "https://github.com/sogaiu/some-bundle-bits"
       :tag "70409ab34b7c762118e0d31efaa4d2f01d46fecf"
       :paths [["sbb.janet" "bundle/"]]}
      {:name "jell"
       :url "https://github.com/sogaiu/jell"
       :tag "718464b8b94971fd8f9e984ca81d35e2c859546e"
       :paths [["jell" "bin/"]]}
      {:name "niche"
       :url "https://github.com/sogaiu/niche"
       :tag "7caf81f636bb97104aada6544219733b3c86badf"
       :paths [["niche.janet" "bin/"]]}]}
    ``)

  (def ds (parse src))

  (def path (parse-all path-str))
  # =>
  @[:vendored 2 :tag]

  (def step-0 (get ds (get path 0)))
  # =>
  [{:name "some-bundle-bits"
    :paths [["sbb.janet" "bundle/"]]
    :tag "70409ab34b7c762118e0d31efaa4d2f01d46fecf"
    :url "https://github.com/sogaiu/some-bundle-bits"}
   {:name "jell"
    :paths [["jell" "bin/"]]
    :tag "718464b8b94971fd8f9e984ca81d35e2c859546e"
    :url "https://github.com/sogaiu/jell"}
   {:name "niche"
    :paths [["niche.janet" "bin/"]]
    :tag "7caf81f636bb97104aada6544219733b3c86badf"
    :url "https://github.com/sogaiu/niche"}]

  (def step-1 (get step-0 (get path 1)))
  # =>
  {:name "niche"
   :paths [["niche.janet" "bin/"]]
   :tag "7caf81f636bb97104aada6544219733b3c86badf"
   :url "https://github.com/sogaiu/niche"}

  (def step-2 (get step-1 (get path 2)))
  # =>
  "7caf81f636bb97104aada6544219733b3c86badf"

  )

(defn g/get-via-path
  [ds path]
  (var context ds)
  (def new-path @[])
  (each step path
    (cond
      (or (keyword? step) (nat? step))
      (do
        (def new-context (get context step))
        (assertf new-context "failed to take a step: %n in context: %n"
                 step context)
        (array/push new-path step)
        (set context new-context))
      #
      (function? step)
      (do
        (assertf (indexed? context)
                 "expected indexed, found: %n" context)
        (var capture nil)
        (eachp [i elt] context
          (when (step elt)
            (array/push new-path i)
            (set capture elt)
            (break)))
        (when (nil? capture)
          (errorf "failed to find target item in: %n" context))
        #
        (set context capture))
      #
      (errorf "unexpected value: %n" step)))
  #
  [context new-path])

(comment

  (def src
    ``
    {:name "ghost"
     :url "https://github.com/sogaiu/ghost"
     :repo "git+https://github.com/sogaiu/ghost"
     :vendored
     [{:name "some-bundle-bits"
       :url "https://github.com/sogaiu/some-bundle-bits"
       :tag "70409ab34b7c762118e0d31efaa4d2f01d46fecf"
       :paths [["sbb.janet" "bundle/"]]}
      {:name "jell"
       :url "https://github.com/sogaiu/jell"
       :tag "718464b8b94971fd8f9e984ca81d35e2c859546e"
       :paths [["jell" "bin/"]]}
      {:name "niche"
       :url "https://github.com/sogaiu/niche"
       :tag "7caf81f636bb97104aada6544219733b3c86badf"
       :paths [["niche.janet" "bin/"]]}]}
    ``)

  (def ds (parse src))

  (g/get-via-path ds [:vendored 2 :tag])
  # =>
  ["7caf81f636bb97104aada6544219733b3c86badf" @[:vendored 2 :tag]]

  (g/get-via-path ds [:vendored
                    |(= (get $ :name) "niche")
                    :tag])
  # =>
  ["7caf81f636bb97104aada6544219733b3c86badf" @[:vendored 2 :tag]]

  )



(defn t/scan-src
  [src top-level-index path]
  # check source string
  (def [ok? data] (protect (parse-all src)))
  (when (not ok?)
    (errorf "failed to parse content"))
  #
  (var tl-idx nil)
  # determine top-level item to operate on
  (def top-level-item
    (if (nat? top-level-index)
      (get data top-level-index)
      (let [first-step (get path 0)]
        (assertf (function? first-step)
                 "first item of path should be a function: %n"
                 first-step)
        (var capture nil)
        (eachp [i elt] data
          (when (first-step elt)
            (set tl-idx i)
            (set capture elt)
            (break)))
        (when (nil? capture)
          (errorf "failed to find target top-level item: %n"
                  data))
        #
        capture)))
  (assertf top-level-item
           "failed to determine top-level item for: %n" data)
  # adjust path if needed
  (def mod-path (if tl-idx (slice path 1) path))
  # finalize top-level index
  (set tl-idx (cond
                (nat? top-level-index) top-level-index
                (nat? tl-idx) tl-idx
                (errorf "failed to determine top-level index")))
  # non-zipper traversal to learn various things
  [;(g/get-via-path top-level-item mod-path) tl-idx])



(def version "2026-03-25_06-02-35")

(def usage
  `````
  Usage: tweake <file> <path> <value>
         tweake - <path> <value>

         tweake [-h|--help]|[-v|--version]

  Slightly modify [1] and display JDN content [2].

  Parameters:

    <file>                 path to `.jdn` file
    <path>                 describes "path" to target to replace
    <value>                value to replace with
    -                      read JDN content from standard input

  Options:

    -h, --help             show this output
    -v, --version          show version information

  Examples:

    Show content based on `bundle/info.jdn` with new :name:

    $ tweake bundle/info.jdn ':name' '"cooler-name"'

    Show content based on standard input with :name's value
    changed:

    $ cat bundle/info.jdn | tweake - ':name' '"spot"'

    Show content based on `.niche.jdn` which changes included
    paths:

    $ tweake .niche.jdn ':includes 0' '"tweake"'

    Show content based on `bundle/info.jdn` with new tag for a
    vendored dependency:

    $ tweake bundle/info.jdn \
             ':vendored |(= (get $ :name) "niche") :tag' \
             '"shiny-new-tag"'

  ---

  [1] A fair bit of the formatting as well as comments are
      preserved.

  [2] It's only the content which is modified and displayed,
      i.e. files are not modified in-place.

  `````)

########################################################################

(defn tweak
  [src top-level-index path value-str]
  (def [found-value new-path skip]
    (t/scan-src src top-level-index path))
  #
  (jt/traverse-src src skip new-path found-value value-str))

(comment

  (tweak `{:key "value"}` 0 [:key] `"lock"`)
  # =>
  `{:key "lock"}`

  (def src
    ``
    [{:name "alice"
      :value 1}
     {:name "bob"
      :value 2}]
    ``)

  (tweak src 0 [0 :value] `11`)
  # =>
  ``
  [{:name "alice"
    :value 11}
   {:name "bob"
    :value 2}]
  ``

  (tweak src 0 [|(= (get $ :name) "bob") :value] `11`)
  # =>
  ``
  [{:name "alice"
    :value 1}
   {:name "bob"
    :value 11}]
  ``

  (def project-janet-src
    ``
    (declare-project
      :name "janet-peg"
      :url "https://github.com/sogaiu/janet-peg")

    (declare-source
      :prefix "janet-peg"
      :source @["lib"])
    ``)

  (tweak project-janet-src 1 [2] `"janet-pegs"`)
  # =>
  ``
  (declare-project
    :name "janet-peg"
    :url "https://github.com/sogaiu/janet-peg")

  (declare-source
    :prefix "janet-pegs"
    :source @["lib"])
  ``

  (tweak project-janet-src 1 [] `nil`)
  # =>
  ``
  (declare-project
    :name "janet-peg"
    :url "https://github.com/sogaiu/janet-peg")

  nil
  ``

  (tweak project-janet-src
         nil
         [|(= (get $ 0) 'declare-source)]
         `nil`)
  # =>
  ``
  (declare-project
    :name "janet-peg"
    :url "https://github.com/sogaiu/janet-peg")

  nil
  ``

  )

(defn main
  [_ & args]
  (def opts (a/parse-args args))
  #
  (when (get opts :show-help)
    (print usage)
    (os/exit 0))
  #
  (when (get opts :show-version)
    (print version)
    (os/exit 0))
  #
  (def {:input input
        :path path
        :top-level-index top-level-index
        :value-str value-str} opts)
  #
  (def [ok? src] (protect (if (= input stdin)
                            (file/read input :all)
                            (slurp input))))
  (when (not ok?)
    (errorf "failed to read in: %s" input))
  #
  (def new-src (tweak src top-level-index path value-str))
  #
  (print new-src))

