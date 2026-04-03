(defn parse-args
  [args]
  (def the-args (array ;args))
  #
  (def head (get the-args 0))
  #
  (when (not head)
    (break {}))
  #
  (when (or (= head "-h") (= head "--help"))
    (break {:show-help true}))
  #
  (when (or (= head "-v") (= head "--version"))
    (break {:show-version true}))
  #
  (array/remove the-args 0)
  #
  (def [opts cmd]
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
  (merge opts
         {:rest the-args}))

