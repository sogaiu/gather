(import ./args :as a)
(import ./subs/prep :as p)

(def version "DEVEL")

(def usage
  `````
  Usage: gather

         gather [-h|--help]|[-v|--version]

  Vendor bits according to `info.jdn`'s `:vendored` section [1].

  Options:

    -h, --help             show this output
    -v, --version          show version information

  An example fragment of `info.jdn` is:

    {:name "..."
     # ...
     :vendored [{:name "spork"
                 :url "https://github.com/janet-lang/spork"
                 :tag "8bfdbb907505225c9a75e0e55c0dec905dffc0b8"
                 :paths [["spork/sh-dsl.janet" "bin/"]
                         ["spork/path.janet" "src/spork/"]]}
                # ...
                ]}

  Description of associated values are:

  * `:url` - repository to fetch from
  * `:tag` - relevant commit hash
  * `:paths` - files to copy into place locally and where to

  The happy path is something like:

  * Find a file named `bundle/info.jdn` or `info.jdn` relative
    to the current directory.

  * Parse the first top-level dictionary from the located
    file, looking for an indexed value associated with a key
    named `:vendored`.

  * For each dictionary value within the located indexed value
    from the previous step, fetch the described content and
    copy into place appropriately.

  For detailed examples of relevant `info.jdn` content, see:

    https://github.com/pyrmont/jeep/
    https://github.com/sogaiu/gather/

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
  (when (not (or (= :file (os/stat "bundle/info.jdn" :mode))
                 (= :file (os/stat "info.jdn" :mode))))
    (eprint (string "failed to find bundle/info.jdn or info.jdn: "
                    "try invoking with -h for help"))
    (os/exit 1))
  #
  (p/run @{:cmd ""
           :err ""
           :help ""
           :opts @{}
           :sub @{:cmd "prep"
                  :opts @{}
                  :params @{:profile "vendor"}}}))

