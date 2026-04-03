(import ./sbb :as s)
(import ../bin/tweake :as t)

(defn install
  [manifest &]
  (s/ddumpf "bundle script: %s hook" "install")
  (def [tos s] (s/get-os-stuff))
  (s/add-binscripts manifest [tos s]))

(defn check
  [&]
  (s/ddumpf "bundle script: %s hook" "check")
  (s/run-tests))

# cwd is project root
(defn prep
  [&]
  (s/ddumpf "bundle script: %s hook" "prep")
  # patch install.janet
  (def target-path "src/install.janet")
  (try
    (do
      (def src (slurp target-path))
      # expect to find in src:
      #
      #   (import ../deps/spork/pm)
      #           ^^^^^^^^^^^^^^^^
      #
      # i.e. the zero-th top-level form of file, 1st argument
      # should be a symbol with name `../deps/spork/pm`
      (def args [src 0 [1]])
      (def [found-value _ _] (t/peek ;args))
      (assertf (= '../deps/spork/pm found-value)
               "unexpected import path in source code, check: %s"
               target-path)
      (def new-src (t/tweak ;args "./spork/pm"))
      (spit target-path new-src))
    ([e]
      (eprintf "bundle script: prep hook error\n  %s" e))))

