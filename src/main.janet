(import ./args :as a)
(import ./subs/prep :as p)

(def version "DEVEL")

(def usage
  `````
  Usage: gather

         gather [-h|--help]|[-v|--version]

  Vendor bits according to info.jdn's :vendored section [1].

  Options:

    -h, --help             show this output
    -v, --version          show version information

  ---

  [1] This code is currently mostly a repackaging of `jeep`'s
      `prep` subcommand with the argument `vendor`.
  `````)

########################################################################

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
  (assert (or (= :file (os/stat "bundle/info.jdn" :mode))
              (= :file (os/stat "info.jdn" :mode)))
          "failed to locate info.jdn")
  #
  (p/run @{:cmd "" 
           :err "" 
           :help "" 
           :opts @{} 
           :sub @{:cmd "prep" 
                  :opts @{} 
                  :params @{:profile "vendor"}}}))

