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
      (def new-src (t/tweak src 0 [1] "./spork/pm"))
      (spit target-path new-src))
    ([e]
      (eprint "bundle script: prep hook error" e))))

