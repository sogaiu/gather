#! /usr/bin/env janet

(comment import ./args :prefix "")
(defn a/parse-args
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


(comment import ./subs/prep :prefix "")
(comment import ../install :prefix "")
(comment import ./spork/pm :prefix "")
###
### Package management functionality. Augments janet's bundle/* module with git, tar, and
### curl support. Connects to a package registry as well, and provides some dependency management.
###
### Port of parts of jpm/pm.janet
###
### This module is purposefully decoupled from declare-cc.

(comment import ./sh :prefix "")
###
### Shell utilties for Janet.
### sh.janet
###

(comment import ./path :prefix "")
### path.janet
###
### A library for path manipulation.
###
### Copyright 2019 © Calvin Rose
###
### Version without defmacro and friends

#
# Common
#

(def- path/ext-peg
  (peg/compile ~{:back (> -1 (+ (* ($) (set "\\/.")) :back))
                 :main :back}))

(defn path/ext
  "Get the file extension for a path."
  [path]
  (if-let [m (peg/match path/ext-peg path (length path))]
    (let [i (m 0)]
      (if (= (path i) 46)
        (string/slice path (m 0))))))

(defn- path/capture-lead
  [& xs]
  [:lead (xs 0)])

#
# Posix
#

(defn path/posix/abspath?
  "Check if a path is absolute."
  [path]
  (string/has-prefix? "/" path))

(def path/posix/ext path/ext)

(def path/posix/sep "Platform separator" "/")

(def path/posix/delim "Platform delimiter" ":")

(def path/posix/last-sep-peg
  (peg/compile '{:back (> -1 (+ (* "/" ($)) :back))
                 :main (+ :back (constant 0))}))

(defn path/posix/basename
  "Gets the base file name of a path."
  [path]
  (if-let [m (peg/match path/posix/last-sep-peg path (length path))]
    (let [[p] m]
      (string/slice path p))
    path))

(defn path/posix/dirname
  "Gets the directory name of a path."
  [path]
  (if-let [m (peg/match path/posix/last-sep-peg path (length path))]
    (let [[p] m]
      (if (zero? p) "./" (string/slice path 0 p)))
    path))

(defn path/posix/parent
  "Gets the parent directory name of a path."
  [path]
  (if-let [m (peg/match path/posix/last-sep-peg path (length path))]
    (let [[p] m]
      (cond (zero? p) ""
            (and (= p 1) (= (string/slice path 0 1) "/")) "/"
            true (string/slice path 0 (- p 1))))
    path))

(defn path/posix/parts
  "Split a path into its parts."
  [path]
  (string/split "/" path))

(def- path/posix-normalize-peg
  (peg/compile
    ~{:span (some (if-not "/" 1))
      :sep (some "/")
      :main (* (? (* (replace '"/" ,path/capture-lead) (any "/")))
               (? ':span)
               (any (* :sep ':span))
               (? (* :sep (constant ""))))}))

(defn path/posix/normalize
  "Normalize a path. This removes . and .. in the
   path, as well as empty path elements."
  [path]
  (def accum @[])
  (def parts (peg/match path/posix-normalize-peg path))
  (var seen 0)
  (var lead nil)
  (each x parts
    (match x
      [:lead what] (set lead what)
      "." nil
      ".." (if (= 0 seen)
             (array/push accum x)
             (do (-- seen) (array/pop accum)))
      (do (++ seen) (array/push accum x))))
  (def ret (string (or lead "") (string/join accum "/")))
  (if (= "" ret) "." ret))

(defn path/posix/join
  "Join path elements together."
  [& els]
  (path/posix/normalize (string/join els "/")))

(defn path/posix/abspath
  "Coerce a path to be absolute."
  [path]
  (if (path/posix/abspath? path)
    (path/posix/normalize path)
    (path/posix/join (or (dyn :path-cwd) (os/cwd)) path)))

(defn path/posix/relpath
  "Get the relative path between two subpaths."
  [source target]
  (def source-parts
    (filter next (path/posix/parts (path/posix/abspath source))))
  (def target-parts
    (filter next (path/posix/parts (path/posix/abspath target))))
  (def same-parts 
    (length (take-until identity (map not= source-parts target-parts))))
  (def up-walk (array/new-filled (- (length source-parts) same-parts) ".."))
  (def down-walk (tuple/slice target-parts same-parts))
  (path/posix/join ;up-walk ;down-walk))

###########################################################################
#
# Windows
#
###########################################################################

(def- path/win-prefix-peg
  (peg/compile
    ~{:drive (* (range "AZ" "az") `:` (any (choice `\` `/`)) ($))
      :dos-unc (* `\\` (choice "." "?") `\UNC\` 
                  (some (if-not `\` 1)) `\` (some (if-not `\` 1)) (any `\`) ($))
      :dos (* `\\` (choice "." "?") `\` (some (if-not `\` 1)) (any `\`) ($))
      :unc (* `\\` (some (if-not `\` 1)) `\` (some (if-not `\` 1)) (any `\`) ($))
      :main (+ :drive :dos-unc :dos :unc)}))

(defn path/win32/abspath?
  "Check if a path is absolute."
  [path]
  (not (nil? (peg/match path/win-prefix-peg path))))

(defn- path/win32-path-prefix [path]
  (if-let [m (peg/match path/win-prefix-peg path)]
    (let [[p] m]
      p)
    0))

# need to use a peg to allow for mixed `\` and `/` in the
# same Windows path.
(def- path/all-sep-peg
  (peg/compile ~{:main (any (+ (some (* ($) (choice `\` `/`) 1))
                               1))}))

(defn- path/sep-split
  "Split string based on separator peg"
  [path]
  (let [locs (peg/match path/all-sep-peg path)
        parts @[]]
    (var start 0)
    (each l locs
      (array/concat parts (string/slice path start l))
      (set start (inc l)))
    (when (< start (length path))
      (array/concat parts (string/slice path start)))
    (filter |(> (length $) 0) parts)))

(def path/win32/ext path/ext)

(def path/win32/sep "Platform separator" `\`)

(def path/win32/delim "Platform delimiter" ";")

(def path/win32/last-sep-peg
  (peg/compile '{:back (> -1 (+ (* (set `\/`) ($)) :back))
                 :main (+ :back (constant 0))}))

(defn path/win32/basename
  "Gets the base file name of a path."
  [path]
  (if-let [m (peg/match path/win32/last-sep-peg path (length path))]
    (let [[p] m
          prefix-end (path/win32-path-prefix path)]
      (if (zero? prefix-end)
        (string/slice path p)
        (if (> prefix-end p)
          "" # last separator is inside the prefix, so basename is blank
          (string/slice path p))))
    path))

(defn path/win32/dirname
  "Gets the directory name of a path."
  [path]
  (if-let [m (peg/match path/win32/last-sep-peg path (length path))]
    (let [[p] m
          prefix-end (path/win32-path-prefix path)]
      (if (zero? p)
        (if (> prefix-end 0) path `.\`)
        (if (> prefix-end p) path (string/slice path 0 p))))
    path))

(defn path/win32/parent
  "Gets the parent directory name of a path."
  [path]
  (if-let [m (peg/match path/win32/last-sep-peg path (length path))]
    (let [[p] m
          prefix-end (path/win32-path-prefix path)]
      (cond (and (zero? prefix-end) (zero? p)) 
            ""
            (and (zero? prefix-end) (not (zero? p)))
            (string/slice path 0 (dec p))
            (and (not (zero? prefix-end)) (zero? p))
            path
            true
            (if (= prefix-end (length path)) 
              path 
              (string/slice path 0 (if (= p prefix-end) p (dec p))))))
    path))

# if there is a prefix (drive letter or unc location),
# add it to output then split the rest else just split the whole thing
(defn path/win32/parts
  "Split a path into its parts."
  [path]
  (let [start (path/win32-path-prefix path)
        rest-path (string/slice path start)]
    (if (zero? start)
      (path/sep-split path)
      (array/concat @[(string/slice path 0 start)] (path/sep-split rest-path)))))

(def- path/win32-normalize-peg
  (peg/compile
    ~{:span (some (if-not (set `\/`) 1))
      :sep (some (set `\/`))
      :main (* (? (* (replace '(+ (* `\\` (some (if-not `\` 1)) `\`) 
                                  (* (? (* (range "AZ" "az") `:`)) `\`))
                              ,path/capture-lead)
                     (any (set `\/`))))
               (? ':span)
               (any (* :sep ':span))
               (? (* :sep (constant ""))))}))

(defn path/win32/normalize
  "Normalize a path. This removes . and .. in the
   path, as well as empty path elements."
  [path]
  (def accum @[])
  (def parts (peg/match path/win32-normalize-peg path))
  (var seen 0)
  (var lead nil)
  (each x parts
    (match x
      [:lead what] (set lead what)
      "." nil
      ".." (if (= 0 seen)
             (array/push accum x)
             (do (-- seen) (array/pop accum)))
      (do (++ seen) (array/push accum x))))
  (def ret (string (or lead "") (string/join accum `\`)))
  (if (= "" ret) "." ret))

(defn path/win32/join
  "Join path elements together."
  [& els]
  (path/win32/normalize (string/join els `\`)))

(defn path/win32/abspath
  "Coerce a path to be absolute."
  [path]
  (if (path/win32/abspath? path)
    (path/win32/normalize path)
    (path/win32/join (or (dyn :path-cwd) (os/cwd)) path)))

(defn path/win32/relpath
  "Get the relative path between two subpaths."
  [source target]
  (def source-parts
    (filter next (path/win32/parts (path/win32/abspath source))))
  (def target-parts
    (filter next (path/win32/parts (path/win32/abspath target))))
  (def same-parts 
    (length (take-until identity (map not= source-parts target-parts))))
  (def up-walk (array/new-filled (- (length source-parts) same-parts) ".."))
  (def down-walk (tuple/slice target-parts same-parts))
  (path/win32/join ;up-walk ;down-walk))

#
# Specialize for current OS
#

(def- path/pre (if (= :windows (os/which)) "win32" "posix"))

(def path/sep (comptime (if path/pre path/win32/sep path/posix/sep)))

(def path/delim (comptime (if path/pre path/win32/delim path/posix/delim)))

(def path/basename (comptime (if path/pre path/win32/basename path/posix/basename)))

(def path/dirname (comptime (if path/pre path/win32/dirname path/posix/dirname)))

(def path/parent (comptime (if path/pre path/win32/parent path/posix/parent)))

(def path/abspath? (comptime (if path/pre path/win32/abspath? path/posix/abspath?)))

(def path/abspath (comptime (if path/pre path/win32/abspath path/posix/abspath)))

(def path/parts (comptime (if path/pre path/win32/parts path/posix/parts)))

(def path/normalize (comptime (if path/pre path/win32/normalize path/posix/normalize)))

(def path/join (comptime (if path/pre path/win32/join path/posix/join)))

(def path/relpath (comptime (if path/pre path/win32/relpath path/posix/relpath)))



(defn sh/devnull
  "get the /dev/null equivalent of the current platform as an open file"
  []
  (os/open (if (= :windows (os/which)) "NUL" "/dev/null") :rw))

(defn sh/exec
  "Execute command specified by args returning its exit code"
  [& args]
  (os/execute args :p))

(defn sh/exec-fail
  "Execute command specified by args, fails when command exits with non-zero exit code"
  [& args]
  (os/execute args :px))

(defn sh/exec-slurp
  ```
  It executes args with `os/spawn` and throws an error if the process returns with non-zero exit code. If the process
  exits with zero exit code, this function trims standard output of the process and returns it. Before the function
  finishes, the spawned process is closed for resource control.
  ```
  [& args]
  # Close the process pipes. If the process pipes are not closed, janet can run out of file descriptors.
  (with [proc (os/spawn args :xp {:out :pipe})]
    (let [[out] (ev/gather
                  (ev/read (proc :out) :all)
                  (os/proc-wait proc))]
      (if out (string/trimr out) ""))))

(defn sh/exec-slurp-all
  ```
  It executes args with `os/spawn` and returns a struct which has the following keys.

  * `:out` - trimmed standard output of the process
  * `:err` - trimmed standard error of the process
  * `:status` - the exit code of the process

  Before the function finishes, the spawned process is closed for resource control.
  ```
  [& args]
  # Close the process pipes. If the process pipes are not closed, janet can run out of file descriptors.
  (with [proc (os/spawn args :p {:out :pipe :err :pipe})]
    (let [[out err status]
          (ev/gather
            (ev/read (proc :out) :all)
            (ev/read (proc :err) :all)
            (os/proc-wait proc))]
      {:out (if out (string/trimr out) "")
       :err (if err (string/trimr err) "")
       :status status})))

(defn sh/rm
  "Remove a directory and all sub directories recursively."
  [path]
  (case (os/lstat path :mode)
    :directory (do
                 (each subpath (os/dir path)
                   (sh/rm (path/join path subpath)))
                 (os/rmdir path))
    nil nil # do nothing if file does not exist
    # Default, try to remove
    (os/rm path)))

(defn sh/rm-readonly
  "Like sh/rm, but will also remove readonly files and folders on windows"
  [path]
  (def os (os/which))
  (when (or (= os :windows) (= os :mingw))
    (def root
      (if (= :file (os/stat path :mode))
        path
        (path/join path "*.*")))
    (sh/exec "attrib" "-R" "-H" root "/S" "/D"))
  (sh/rm path))

(defn sh/exists?
  "Check if the given file or directory exists. (Follows symlinks)"
  [path]
  (not= nil (os/stat path)))

(defn sh/scan-directory
  "Scan a directory recursively, applying the given function on all files and
  directories in a depth-first manner. This function has no effect if the
  directory does not exist."
  [dir func]
  (each name (try (os/dir dir) ([_] @[]))
    (def fullpath (path/join dir name))
    (case (os/stat fullpath :mode)
      :file (func fullpath)
      :directory (do
                   (sh/scan-directory fullpath func)
                   (func fullpath)))))

(defn sh/list-all-files
  "List the files in the given directory recursively. Return the paths to all
  files found, relative to the current working directory if the given path is a
  relative path, or as an absolute path otherwise."
  [dir &opt into]
  (default into @[])
  (each name (try (os/dir dir) ([_] @[]))
    (def fullpath (path/join dir name))
    (case (os/stat fullpath :mode)
      :file (array/push into fullpath)
      :directory (sh/list-all-files fullpath into)))
  into)

(defn sh/create-dirs
  "Create all directories in path specified as string including itself."
  [dir-path]
  (def dirs @[])
  (each part (path/parts dir-path)
    (array/push dirs part)
    (let [path (path/join ;dirs)]
      (protect (os/mkdir path)))))

(defn sh/create-dirs-to
  "Create all directories in path specified as string not including the final path segment."
  [dir-path]
  (def dirs @[])
  (each part (slice (path/parts dir-path) 0 -2)
    (array/push dirs part)
    (let [path (path/join ;dirs)]
      (protect (os/mkdir path)))))

(defn sh/make-new-file
  "Create and open a file, creating all the directories leading to the file if
  they do not exist, and return it. By default, open as a writable file (mode is `:w`)."
  [file-path &opt mode]
  (default mode :w)
  (let [parent-path (path/dirname file-path)]
    (when (and (not (sh/exists? file-path))
               (not (sh/exists? parent-path)))
      (sh/create-dirs parent-path)))
  (file/open file-path mode))

(defn sh/copy-file
  "Copy a file from source to destination. Creates all directories in the path
  to the destination file if they do not exist."
  [src-path dst-path]
  (def buf-size 4096)
  (def buf (buffer/new buf-size))
  (with [src (file/open src-path :rb)]
    (with [dst (sh/make-new-file dst-path :wb)]
      (while (def bytes (file/read src buf-size buf))
        (file/write dst bytes)
        (buffer/clear buf)))))

(defn sh/copy
  `Copy a file or directory recursively from one location to another.
  Expects input to be unix style paths`
  [src dest]
  (if (= :windows (os/which))
    (let [end (last (path/posix/parts src))
          isdir (= (os/stat src :mode) :directory)]
      (os/shell (string "C:\\Windows\\System32\\xcopy.exe"
                        " "
                        (path/win32/join ;(path/posix/parts src))
                        (path/win32/join ;(if isdir [;(path/posix/parts dest) end] (path/posix/parts dest)))
                        "/y /s /e /i > nul")))
    (os/execute ["cp" "-rf" src dest] :px)))

(def- sh/shlex-grammar (peg/compile ~{:ws (set " \t\r\n")
                                   :escape (* "\\" (capture 1))
                                   :dq-string (accumulate (* "\"" (any (+ :escape (if-not "\"" (capture 1)))) "\""))
                                   :sq-string (accumulate (* "'" (any (if-not "'" (capture 1))) "'"))
                                   :token-char (+ :escape (* (not :ws) (capture 1)))
                                   :token (accumulate (some :token-char))
                                   :value (* (any (+ :ws)) (+ :dq-string :sq-string :token) (any :ws))
                                   :main (any :value)}))

(defn sh/split
  "Split a string into 'sh like' tokens, returns
   nil if unable to parse the string."
  [s]
  (peg/match sh/shlex-grammar s))

(defn- sh/shell-quote
  [arg]
  (string "'" (string/replace-all "'" `'\''` arg) "'"))

(defn sh/escape
  "Output a string with all arguments correctly quoted"
  [& args]
  (string/join (map sh/shell-quote args) " "))

(defn sh/which
  "Search for the full path to a program, like the `which` command on unix or the `where` command on Windows."
  [name &opt paths]
  (def o (os/which))
  (def win (or (= o :windows) (= o :mingw)))
  (default paths
    (if win
      (string/split ";" (os/getenv "Path"))
      (string/split ":" (os/getenv "PATH"))))
  (def pathexts (if win (string/split ";" (os/getenv "PATHEXT" "")) @[]))
  (array/insert pathexts 0 "")
  (prompt :result
    (each p paths
      (when (= (os/stat p :mode) :directory)
        (def fp (path/join p name))
        (each ext pathexts
          (def fp2 (string fp ext))
          (when (= (os/stat fp2 :mode) :file)
            (return :result fp2)))))))

(defn sh/self-exe
  "Get path to the janet executable"
  []
  (def janet (dyn *executable* "janet"))
  (when (path/abspath? janet) (break janet))
  (case (os/which)
    :linux (os/readlink "/proc/self/exe")
    # default
    (sh/which janet)))

(comment import ./path :prefix "")

(comment import ./pm-config :prefix "")
###
### Configuration from environment variables for pm.janet and declare-cc.janet.
###

(def pm-config/default-pkglist
  "The default package listing for resolving short bundle names."
  "https://github.com/janet-lang/pkgs.git")

(defn pm-config/detect-toolchain
  "Auto-detect the current compiler toolchain."
  [env]
  (cond
    (get env :toolchain) (get env :toolchain)
    (os/getenv "MSVC") :msvc
    (os/getenv "GCC") :gcc
    (os/getenv "CLANG") :clang
    (os/getenv "CC") :cc # any posix compatible compiler accessed via `cc`
    (= :windows (os/which)) :msvc
    (os/compiler)))

# Fix for janet 1.35.2
(compwhen (not (dyn 'assertf))
  (defmacro- assertf
    "Convenience macro that combines `assert` and `string/format`."
    [x fmt & args]
    (def v (gensym))
    ~(do
       (def ,v ,x)
       (if ,v
         ,v
         (,errorf ,fmt ,;args)))))

(defn- pm-config/set1
  [env d e &opt xform]
  (default xform identity)
  (when-let [x (os/getenv e)]
    (put env d (xform x))))

(defn- pm-config/tobool
  [x]
  (get
    {"t" true "true" true "1" true "yes" true "on" true}
    (string/ascii-lower (string/trim x)) false))

(defn- pm-config/toposint
  [x]
  (def y (scan-number x))
  (assertf (and (>= y 1) (int? y)) "expected a positive integer for number of workers, got %v" x)
  y)

(defn- pm-config/make-enum
  [name & options]
  (def enum-set (tabseq [o :in options] o o))
  (fn enum
    [x]
    (def y (-> x string/ascii-lower keyword))
    (assertf (in enum-set y) "unknown option %v for %s. Expected one of %s." x name (string/join options ", "))
    y))

(def- pm-config/build-type-xform (pm-config/make-enum "build type" :debug :develop :release))
(def- pm-config/toochain-xform (pm-config/make-enum "toolchain" :gcc :clang :msvc :cc)) # TODO mingw, zig

(defn pm-config/read-env-variables
  "Read and validate environment variables for configuration. These environment variables are
  translated to dynamic bindings and stored in an environment table. By default, store the bindings in the current environment."
  [&opt env]
  (default env (curenv))
  (when (get env :is-configured) (break))
  (pm-config/set1 env :janet-prefix "JANET_PREFIX")
  (pm-config/set1 env :gitpath "JANET_GIT")
  (pm-config/set1 env :curlpath "JANET_CURL")
  (pm-config/set1 env :tarpath "JANET_TAR")
  (pm-config/set1 env :build-type "JANET_BUILD_TYPE" pm-config/build-type-xform)
  (pm-config/set1 env :toolchain "JANET_TOOLCHAIN" pm-config/toochain-xform)
  (pm-config/set1 env :build-root "JANET_BUILD_DIR")
  (pm-config/set1 env :offline "JANET_OFFLINE" pm-config/tobool)
  (pm-config/set1 env :pkglist "JANET_PKGLIST")
  (pm-config/set1 env :workers "WORKERS" pm-config/toposint)
  (pm-config/set1 env :verbose "VERBOSE" pm-config/tobool)
  (put env :is-configured true))

(defn pm-config/print-config
  "Print all current settings"
  [&opt env]
  (default env (curenv))
  (print "build dir:  " (get env :build-root "_build"))
  (print "build type: " (get env :build-type "release"))
  (print "curl:       " (get env :curlpath "curl"))
  (print "git:        " (get env :gitpath "git"))
  (print "offline:    " (if (get env :offline) "true" "false"))
  (print "pkg list:   " (get env :pkglist pm-config/default-pkglist))
  (print "prefix:     " (get env :janet-prefix "<none>"))
  (print "syspath:    " (get env *syspath* "<none>"))
  (print "tar:        " (get env :tarpath "tar"))
  (print "toolchain:  " (pm-config/detect-toolchain env))
  (print "verbose:    " (if (get env :verbose) "true" "false"))
  (print "workers:    " (get env :workers (os/cpu-count))))

(comment import ./cc :prefix "")
###
### cc.janet
###
### Improved version of the C Compiler abstraction from JPM that should be more correct, composable, and
### have less configuration.
###
### Wrapper around the system C compiler for compiling Janet native modules and executables.
### Opinionated and optimized for use with Janet, and does not actually run
### commands unless specified with (dyn *visit*). Also included is package config integration.
### Headers, static libraries, and dynamic libraries can all be used from `(dyn *syspath*)`.
###
### Example usage:
###
### (use spork/cc)
###
### (search-static-libraries "m" "rt" "dl")
### (search-dynamic-libraries "janet")
### (pkg-config "sdl2" "vulkan")
### (with-dyns [*defines* {"GAME_BUILD" "devel-0.0"}
###             *visit* visit-execute-if-stale]
###   (compile-and-link-executable "game" "main.c" "sound.c" "graphics.c"))
###

(comment import ./path :prefix "")

(comment import ./sh :prefix "")

(comment import ./build-rules :prefix "")
###
### spork/build-rules.janet
###
### Run commands that produce files in an incremental manner.
### Use to implement a build system.
###

(defn- build-rules/cancel-all [fibers reason] (each f fibers (ev/cancel f reason) (put fibers f nil)))

(defn- build-rules/wait-for-fibers
  [chan fibers]
  (defer (build-rules/cancel-all fibers "parent canceled")
    (repeat (length fibers)
      (def [sig fiber] (ev/take chan))
      (if (= sig :ok)
        (put fibers fiber nil)
        (do
          (build-rules/cancel-all fibers "sibling canceled")
          (propagate (fiber/last-value fiber) fiber))))))

(defn- build-rules/target-not-found
  "Creates an error message."
  [target]
  (errorf "target %v does not exist and no rule exists to build it" target))

(defn- build-rules/target-already-defined
  "Error when an output already has a rule defined to create it."
  [target]
  (errorf "target %v has multiple rules" target))

(defn- build-rules/stat-mtime
  "Cache modified times to make as few calls to os/stat as possible"
  [path mtime-cache]
  (if-let [check (get mtime-cache path)]
    check
    (set (mtime-cache path)
         (let [m (os/stat path :modified)]
           (if (nil? m) false m)))))

(defn- build-rules/utd
  "Check if a target is up to date."
  [target all-targets utd-cache mtime-cache]
  (def u (get utd-cache target))
  (if (not= nil u) (break u))
  (def rule (get all-targets target))
  (if (= target (get rule :task)) (break (set (utd-cache target) false)))
  (def mtime (build-rules/stat-mtime target mtime-cache))
  (if-not mtime
    (if rule
      (break (set (utd-cache target) false))
      (build-rules/target-not-found target)))
  (var ret true)
  (each i (get rule :inputs [])
    (if-not (build-rules/utd i all-targets utd-cache mtime-cache) (break (set ret false)))
    (def s (build-rules/stat-mtime i mtime-cache))
    (when (or (not s) (< mtime s))
      (set ret false)
      (break)))
  (set (utd-cache target) ret))

(defn- build-rules/run-rules
  "Execute the minimal set of rules needed to build all targets in `targets`."
  [rules targets &opt n-workers]
  (def utd-cache @{})
  (def all-targets @{})
  (def dirty-targets @{})
  (def dependents @{})
  (def dep-counts @{})
  (def mtime-cache @{})
  (var work-count 0)
  (def targets-built @[])
  (def q (ev/chan math/int32-max))

  # Check rules for duplicates
  (each rule (distinct rules)
    (when-let [p (get rule :task)]
      (when (get all-targets p) (build-rules/target-already-defined p))
      (put all-targets p rule))
    (each o (get rule :outputs [])
      (when (get all-targets o) (build-rules/target-already-defined o))
      (put all-targets o rule)))

  # Check for rules that need running
  (defn needs-build? [target]
    (def check (get dirty-targets target))
    (if (not= nil check) (break check))
    (def rule (get all-targets target))
    (def inputs (get rule :inputs []))
    (var needs-build (not (build-rules/utd target all-targets utd-cache mtime-cache)))
    (var dep-count 0)
    (each i inputs
      (put dependents i (put (get dependents i @{}) target true))
      (when (needs-build? i) (++ dep-count) (set needs-build true)))
    (put dep-counts target dep-count)
    (put dirty-targets target needs-build)
    (when needs-build
      (if (= 0 dep-count) (ev/give q target))
      (++ work-count))
    needs-build)
  (each target targets (needs-build? target))
  (default n-workers 1)

  (defn worker
    [_n]
    (while (pos? work-count)
      (def target (ev/take q))
      (if-not target (break))
      (-- work-count)
      (def rule (get all-targets target))
      (def dependent-set (get dependents target ()))
      (def r (assert (get rule :recipe)))
      (edefer
        (do
          (each o (get rule :outputs [])
            (protect (os/rm o)))
          (repeat n-workers (ev/give q nil)))
        (if (indexed? r)
          (each rr r (rr))
          (r)))
      (array/push targets-built target)
      (eachk next-target dependent-set
        (-- (dep-counts next-target))
        (if (= 0 (get dep-counts next-target))
          (ev/give q next-target))))
    (ev/give q nil))

  (def fibers @{})
  (def super (ev/chan))
  (forv i 0 n-workers
    (def fib (ev/go worker i super))
    (put fibers fib fib))
  (build-rules/wait-for-fibers super fibers)

  targets-built)

(defn- build-rules/gettarget [rules target]
  (def item (get rules target))
  (unless item (error (string "no rule for target '" target "'")))
  item)

(defn- build-rules/target-append
  [rules target key v]
  (def item (build-rules/gettarget rules target))
  (def vals (get item key))
  (unless (find |(= v $) vals)
    (array/push vals v))
  item)

(defn- build-rules/rule-impl
  [rules target deps thunk]
  (def phony (keyword? target))
  (assert (table? rules) "rules must be a table")
  (def all-targets
    (cond
      (keyword? target) [(string target)]
      (string? target) [target]
      (indexed? target) target
      (errorf "bad target %v" target)))
  (def target (first all-targets))
  (each d [;deps ;all-targets]
    (assert (string? d) "inputs and outputs must be strings"))
  (unless (get rules target)
    (def new-rule
      @{:inputs @[]
        :outputs @[]
        :recipe @[]})
    (put rules target new-rule))
  (each d deps (build-rules/target-append rules target :inputs d))
  (if phony
    (put (build-rules/gettarget rules target) :task target)
    (each t all-targets (build-rules/target-append rules target :outputs t)))
  (build-rules/target-append rules target :recipe thunk))

(defmacro build-rules/build-rule
  ```
  Add a rule to the rule graph. `rules` should be a table, `target`
  a string or tuple of strings, and `deps` a tuple of strings. `body`
  is code that will be executed to create all of the targets by the rules.
  If target is a keyword, the rule will always be considered out of date.
  ```
  [rules target deps & body]
  ~(,build-rules/rule-impl ,rules ,target ,deps (fn :build-rule [] nil ,;body)))

(defn build-rules/build-thunk
  ```
  Add a rule to the rule graph. `rules` should be a table, `target`
  a string or tuple of strings, and `deps` a tuple of strings. `body`
  is code that will be executed to create all of the targets by the rules.
  If target is a keyword, the rule will always be considered out of date.
  ```
  [rules target deps thunk]
  (build-rules/rule-impl rules target deps thunk))

(defn build-rules/build-run
  "Build a list of targets, as specified by rules. Return an array of all recursively updated targets."
  [rules targets &opt n-workers]
  (assert (table? rules) "rules must be a table")
  (default n-workers (os/cpu-count))
  (def all-targets (if (indexed? targets) targets [targets]))
  (build-rules/run-rules rules all-targets n-workers))

(comment import ./stream :prefix "")
(defn stream/lines
  ```
  Returns a fiber that yields each line from a core/stream value. If separator is not specified, the default separator
  is `\n`. After the fiber yields the last line, it returns `nil`. If the fiber is resumed after the stream is closed or
  after the fiber returns `nil`, an error is thrown.
  ```
  [stream &named separator]
  (default separator "\n")
  (defn yield-lines
    [chunk]
    (when-let [idx (string/find separator chunk)]
      # Yield the first line
      (yield (buffer/slice chunk 0 idx))
      # Eliminate the first line from chunk without creating a new buffer
      (def idx+1 (inc idx))
      (buffer/blit chunk chunk 0 idx+1)
      (yield-lines (buffer/popn chunk idx+1))))
  (defn fetch-lines
    [chunk]
    (if (ev/read stream 1024 chunk)
      (do
        (yield-lines chunk)
        (fetch-lines chunk))
      (do
        (yield-lines chunk)
        (when (not (empty? chunk))
          (yield chunk)))))
  (coro (fetch-lines @"")))


(defdyn *ar* "Archiver, defaults to `ar`.")
(defdyn *build-dir* "If generating intermediate files, store them in this directory")
(defdyn *build-type* "Presets for compiler optimizations, can be :release, :develop, and :debug, defaults to :develop.")
(defdyn *c++* "C++ compiler, defaults to `c++`.")
(defdyn *c++flags* "Extra C++ compiler flags to use during compilation")
(defdyn *cc* "C compiler, defaults to `cc`.")
(defdyn *cflags* "Extra C compiler flags to use during compilation")
(defdyn *defines* "Map of extra defines to use when compiling")
(defdyn *libs* "List of libraries to use when compiling - can be static or dynamic depending on system.")
(defdyn *dynamic-libs* "List of dynamic libraries to use when compiling")
(defdyn *msvc-libs* "List of .lib libraries to use when compiling with msvc")
(defdyn *msvc-vcvars* "Path to vcvarsall.bat to use initialize MSVC environment. If unset, `msvc-find` will try to guess using typical install locations.")
(defdyn *msvc-cpath* "Path to Janet libraries and headers.")
(defdyn *lflags* "Extra linker flags")
(defdyn *static-libs* "List of static libraries to use when compiling")
(defdyn *target-os* "Operating system to assume is being used for target compiler toolchain")
(defdyn *visit* "Optional callback to process each CLI command and its inputs and outputs")
(defdyn *use-rpath* "Optional setting to enable using `(dyn *syspath*)` as the runtime path to load for Shared Objects. Defaults to true")
(defdyn *use-rdynamic*
  ``Optional setting to enable using `-rdynamic` or `-Wl,-export_dynamic` when linking executables.
  This is the preferred way on POSIX systems to let an executable load native modules dynamically at runtime.
  Defaults to true``)
(defdyn *pkg-config-flags* "Extra flags to pass to pkg-config")
(defdyn *smart-libs*
  ``Try to resolve circular or out-of-order dependencies between libraries by using --start-group and --end-group.
  Some linkers support this by default, but not at all. Defaults to true on linux and macos.``)
(defdyn *c-std* "C standard to use as a 2 digit number, defaults to 99 on GCC-like compilers, 11 on msvc.")
(defdyn *c++-std* "C++ standard to use as a 2 digit number, defaults to 11 on GCC-like compilers, 14 on msvc.")
(defdyn *rules* "Rules to use with visit-add-rule")
(defdyn *vcvars-cache* "Where to cache vcvars once we have calculated them")
(defdyn *janet-prefix* "Path prefix used to detect where to find libjanet, janet.h, etc.")

###
### Prefix detection
###

(defn cc/get-unix-prefix
  "Auto-detect what prefix to use for finding libjanet.so, headers, etc. on unix systems"
  []
  (if-let [p (dyn *janet-prefix*)] (break p))
  (var result nil)
  (each test [(os/getenv "JANET_PREFIX")
              (os/getenv "PREFIX")
              (path/join (dyn *syspath*) ".." "..")
              (path/join (dyn *syspath*) "..")
              (try (path/join (sh/self-exe) ".." "..") ([_e] nil))
              (dyn *syspath*)
              "/usr/"
              "/usr/local"
              "/"]
    (when test
      (def headercheck (path/join test "include" "janet.h"))
      (when (= :file (os/stat headercheck :mode))
        (set result test)
        (break))))
  (assert result "no prefix discovered for janet headers!")
  (setdyn *janet-prefix* result)
  result)

(defn cc/get-msvc-prefix
  "Auto-detect install location on windows systems with a default install. This is the directory containing Library, C, docs, bin, etc."
  []
  (if-let [p (dyn *janet-prefix*)] (break p))
  (var result nil)
  (each test [(os/getenv "JANET_PREFIX")
              (os/getenv "PREFIX")
              (path/join (dyn *syspath*) ".." "..")
              (path/join (dyn *syspath*) "..")
              (try (path/join (sh/self-exe) ".." "..") ([_e] nil))
              (dyn *syspath*)]
    (when test
      (def headercheck (path/join test "C" "janet.h"))
      (when (= :file (os/stat headercheck :mode))
        (set result test)
        (break))))
  (assert result "no prefix discovered for janet headers!")
  (setdyn *janet-prefix* result)
  result)

###
### Universal helpers for all toolchains
###

(defn- cc/cflags [] (dyn *cflags* []))
(defn- cc/c++flags [] (dyn *c++flags* []))
(defn- cc/lflags [] (dyn *lflags* []))
(defn- cc/target-os [] (dyn *target-os* (os/which)))
(defn- cc/build-dir [] (dyn *build-dir* "."))
(defn- cc/static-libs [] (dyn *static-libs* []))
(defn- cc/dynamic-libs [] (dyn *dynamic-libs* []))
(defn- cc/default-libs [] (dyn *libs* []))
(defn- cc/vcvars-cache [] (dyn *vcvars-cache* ".vcvars.jdn"))

(defn- cc/build-type []
  (def bt (dyn *build-type* :develop))
  (if-not (in {:develop true :debug true :release true} bt)
    (errorf "invalid build type %v, expected :release, :develop, or :debug" bt))
  bt)

(defn- cc/lib-path []
  "Guess a library path based on the current system path"
  (def prefix (cc/get-unix-prefix))
  (path/join prefix "lib"))

(defn- cc/include-path []
  "Guess a header path based on the current system path"
  (def prefix (cc/get-unix-prefix))
  (path/join prefix "include"))

(defn- cc/msvc-cpath
  "Guess a library and header path for msvc with a defualt Janet windows install."
  []
  (when-let [p (dyn *msvc-cpath*)] (break p))
  (def wp (cc/get-msvc-prefix))
  (path/join wp "C"))

(defn cc/msvc-janet-import-lib
  "Get path to the installed Janet import lib. This import lib is needed when create dlls for natives."
  []
  (path/join (cc/msvc-cpath) "janet.lib"))

(defn- cc/default-exec [&])
(defn- cc/exec
  "Call the (dyn *visit*) function on commands"
  [cmd inputs outputs message]
  ((dyn *visit* cc/default-exec) cmd inputs outputs message) cmd)

(defn- cc/getsetdyn
  [sym]
  (def x (dyn sym))
  (if (= nil x)
    (setdyn sym @[])
    x))

(defn- cc/classify-source
  "Classify a source file as C or C++ (or object files)"
  [path]
  (cond
    (string/has-suffix? ".c" path) :c
    (string/has-suffix? ".cc" path) :c++
    (string/has-suffix? ".cpp" path) :c++
    (string/has-suffix? ".cxx" path) :c++
    # object files
    (string/has-suffix? ".o" path) :o
    (string/has-suffix? ".obj" path) :o
    # else
    (errorf "unknown source file type for %v" path)))

###
### Basic GCC-like Compiler Wrapper
###

# GCC toolchain helpers
(defn- cc/ar [] (dyn *ar* "ar"))
(defn- cc/cc [] (dyn *cc* "cc"))
(defn- cc/c++ [] (dyn *c++* "c++"))
(defn- cc/opt []
  (case (cc/build-type)
    :debug ["-O0" "-g"]
    :develop ["-O2" "-g"]
    :release ["-O2"]
    []))
(defn- cc/defines []
  (def res @[])
  (array/push res (string "-DJANET_BUILD_TYPE=" (cc/build-type)))
  (eachp [k v] (dyn *defines* [])
    (if (= v true)
      (array/push res (string "-D" k))
      (array/push res (string "-D" k "=" v))))
  (sort res) # for deterministic builds
  res)
(defn- cc/extra-paths []
  (def sp (dyn *syspath* "."))
  (def ip (cc/include-path))
  [(string "-I" sp)
   # ;(if (dyn :verbose) ["-v"] []) # err, too verbose
   ;(if (and ip (not= ip sp)) [(string "-I" ip)] [])])
(defn- cc/extra-link-paths []
  (def sp (dyn *syspath* "."))
  (def lp (cc/lib-path))
  [(string "-L" sp)
   ;(if (and lp (not= lp sp)) [(string "-L" lp)] [])])
(defn- cc/rpath
  []
  (if (dyn *use-rpath* true)
    [(string "-Wl,-rpath," (cc/lib-path))
     (string "-Wl,-rpath," (dyn *syspath* "."))]
    []))
(defn- cc/smart-libs []
  (def dflt (index-of (cc/target-os) [:linux]))
  (dyn *smart-libs* dflt))
(defn- cc/libs [static]
  (def dl (if (= (cc/target-os) :macos) ["-undefined" "dynamic_lookup"] []))
  (def sg (if (cc/smart-libs) ["-Wl,--start-group"] []))
  (def eg (if (cc/smart-libs) ["-Wl,--end-group"] []))
  (def bs (if (not= (cc/target-os) :macos) ["-Wl,-Bstatic"] []))
  (def bd (if (not= (cc/target-os) :macos) ["-Wl,-Bdynamic"] []))
  [;sg
   ;(cc/lflags)
   ;(if static ["-static"] [])
   ;dl
   ;(cc/default-libs)
   ;bs
   ;(cc/static-libs)
   ;bd
   ;(cc/dynamic-libs)
   ;(if static bs []) # put back to static linking so the -static flag works.
   ;eg
   ;(cc/rpath)])
(defn- cc/rdynamic
  "Some systems like -rdynamic, some like -Wl,-export_dynamic"
  []
  (if (dyn *use-rdynamic* true)
    [(if (= (cc/target-os) :macos) "-Wl,-export_dynamic" "-rdynamic")]
    []))
(defn- cc/ccstd []
  (def std (dyn *c-std* 99))
  (if (and (bytes? std) (string/has-prefix? "-" std))
    std
    (string "-std=c" std)))
(defn- cc/c++std []
  (def std (dyn *c++-std* 11))
  (if (and (bytes? std) (string/has-prefix? "-" std))
    std
    (string "-std=c++" std)))

(defn cc/compile-c
  "Compile a C source file to an object file. Return the command arguments."
  [from to]
  (cc/exec [(cc/cc) (cc/ccstd) ;(cc/opt) ;(cc/cflags) ;(cc/extra-paths) "-fPIC" ;(cc/defines) "-c" from "-o" to "-pthread"]
        [from] [to] (string "compiling " from "...")))

(defn cc/compile-c++
  "Compile a C++ source file to an object file. Return the command arguments."
  [from to]
  (cc/exec [(cc/c++) (cc/c++std) ;(cc/opt) ;(cc/c++flags) ;(cc/extra-paths) "-fPIC" ;(cc/defines) "-c" from "-o" to "-pthread"]
        [from] [to] (string "compiling " from "...")))

(defn cc/link-shared-c
  "Link a C program to make a shared library. Return the command arguments."
  [objects to]
  (cc/exec [(cc/cc) (cc/ccstd) ;(cc/opt) ;(cc/cflags) ;(cc/extra-link-paths) "-o" to ;objects "-pthread" ;(cc/libs false) ;(cc/dynamic-libs) "-shared"]
        objects [to] (string "linking " to "...")))

(defn cc/link-shared-c++
  "Link a C++ program to make a shared library. Return the command arguments."
  [objects to]
  (cc/exec [(cc/c++) (cc/c++std) ;(cc/opt) ;(cc/c++flags) ;(cc/extra-link-paths) "-o" to ;objects "-pthread" ;(cc/libs false) ;(cc/dynamic-libs) "-shared"]
        objects [to] (string "linking " to "...")))

(defn cc/link-executable-c
  "Link a C program to make an executable. Return the command arguments."
  [objects to &opt make-static]
  (cc/exec [(cc/cc) (cc/ccstd) ;(cc/opt) ;(cc/cflags) ;(cc/extra-link-paths) "-o" to ;objects ;(cc/rdynamic) "-pthread" ;(cc/libs make-static)]
        objects [to] (string "linking " to "...")))

(defn cc/link-executable-c++
  "Link a C++ program to make an executable. Return the command arguments."
  [objects to &opt make-static]
  (cc/exec [(cc/c++) (cc/c++std) ;(cc/opt) ;(cc/c++flags) ;(cc/extra-link-paths) "-o" to ;objects ;(cc/rdynamic) "-pthread" ;(cc/libs make-static)]
        objects [to] (string "linking " to "...")))

(defn cc/make-archive
  "Make an archive file. Return the command arguments."
  [objects to]
  (cc/exec [(cc/ar) "rcs" to ;objects] objects [to] (string "archiving " to "...")))

# Compound commands

(defn cc/out-path
  "Take a source file path and convert it to an output path with no intermediate directories."
  [path to-ext &opt sep]
  (default sep "/")
  (def flatpath
    (->> path
         (string/replace-all "\\" "___")
         (string/replace-all "/" "___")))
  (string (cc/build-dir) sep flatpath to-ext))

(defn- cc/compile-many
  "Compile a number of source files, and return the
  generated objects files, as well as a boolean if any cpp
  source files were found."
  [sources cmds-into ext]
  (def objects @[])
  (var has-cpp false)
  (each source sources
    (def o (cc/out-path source ext))
    (def source-type (cc/classify-source source))
    (case source-type
      :o
      (array/push objects source)
      :c
      (do
        (array/push cmds-into (cc/compile-c source o))
        (array/push objects o))
      :c++
      (do
        (set has-cpp true)
        (array/push cmds-into (cc/compile-c++ source o))
        (array/push objects o))
      # else
      (errorf "unknown source file type for %v" source)))
  [has-cpp objects])

(defn cc/compile-and-link-shared
  "Compile and link a shared C/C++ library. Return an array of commands."
  [to & sources]
  (def res @[])
  (def [has-cpp objects] (cc/compile-many sources res ".shared.o"))
  (array/push
    res
    (if has-cpp
      (cc/link-shared-c++ objects to)
      (cc/link-shared-c objects to))))

(defn cc/compile-and-link-executable
  "Compile and link an executable C/C++ program. Return an array of commands."
  [to & sources]
  (def res @[])
  (def [has-cpp objects] (cc/compile-many sources res ".executable.o"))
  (array/push
    res
    (if has-cpp
      (cc/link-executable-c++ objects to)
      (cc/link-executable-c objects to))))

(defn cc/compile-and-make-archive
  "Compile and create a static archive. Return an array of commands."
  [to & sources]
  (def res @[])
  (def [_ objects] (cc/compile-many sources res ".static.o"))
  (array/push res (cc/make-archive objects to)))

###
### MSVC Compiler Wrapper (msvc 2017 and later)
###

### TODO:
### - more testing
### - libraries

(def- cc/tag "AAAAAAAAAAAAAAAAAAAAAAAAAAA")
(def- cc/vcvars-grammar
  (peg/compile
    ~{:main (* (thru ,cc/tag) (any :line))
      :line (group (* :key "=" :value :s*))
      :key '(to "=")
      :value '(to "\n")}))

(defn cc/msvc-setup?
  "Check if MSVC environment is already setup."
  []
  (and
    (os/getenv "INCLUDE")
    (os/getenv "LIB")
    (os/getenv "LIBPATH")))

(defn- cc/dumb-escape
  [x]
  (->> x
       (string/replace-all " " "^ ")
       (string/replace-all "(" "^(")
       (string/replace-all ")" "^)")))

(defn cc/msvc-find
  ``Find vcvarsall.bat and run it to setup the current environment for building.
  Uses `(dyn *msvc-vcvars*)` to find the location of the setup script, then will check
  for the presence of a file `(dyn *vcvars-cache* ".vcvars.jdn")`, and then otherwise defaults
  to checking typical install locations for Visual Studio.
  Supports VS 2017, 2019, and 2022, any edition.
  Will set environment variables such that invocations of cl.exe, link.exe, etc.
  will work as expected.``
  []
  (when (cc/msvc-setup?) (break))
  # Cache the vcvars locally instead of calling vcvarsall.bat over and over again
  (var found false)
  (when-with [f (file/open (cc/vcvars-cache))]
    (def data (-> f (:read :all) parse))
    (eachp [k v] data
      (os/setenv k v))
    (set found true))
  (if found (break))
  (def arch (string (os/arch)))
  (defn loc [pf y e]
    (string `C:\` pf `\Microsoft Visual Studio\` y `\` e `\VC\Auxiliary\Build\vcvarsall.bat`))
  (var found-path nil)
  (if-let [vcv (dyn *msvc-vcvars*)]
    (set found-path vcv)
    (do
      (loop [pf :in ["Program Files" "Program Files (x86)"]
             y :in [2022 2019 2017]
             e :in ["Enterprise" "Professional" "Community" "BuildTools"]]
        (def path (loc pf y e))
        (when (os/stat path :mode)
          (set found-path path)
          (break)))))
  (unless found-path (error "Could not find vcvarsall.bat"))
  (when (dyn :verbose)
    (print "found " found-path))
  (def arg (string (cc/dumb-escape found-path) ` ` arch ` && echo ` cc/tag ` && set`))
  (def output (sh/exec-slurp "cmd" "/s" "/c" arg))
  (def kvpairs (peg/match cc/vcvars-grammar output))
  (assert kvpairs)
  (def cache @{})
  (each [k v] kvpairs
    (def kk (string/trim k))
    (def vv (string/trim v))
    (put cache kk vv)
    (os/setenv kk vv))
  (spit (cc/vcvars-cache) (string/format "%j" cache))
  nil)

(defn- cc/msvc-opt
  []
  (case (cc/build-type)
    :debug ["/Od" "/DDEBUG" "/Z7" "/MDd"]
    :develop ["/O2" "/DDEBUG" "/Z7" "/MDd"]
    ["/O2" "/MD"]))
(defn- cc/msvc-defines []
  (def res @[])
  (array/push res (string "/DJANET_BUILD_TYPE=" (cc/build-type)))
  (eachp [k v] (dyn *defines* [])
    (array/push res (string "/D" k "=" v)))
  (sort res) # for deterministic builds
  res)
(defn- cc/msvc-cstd
  []
  (def std (dyn *c-std* 11))
  (if (and (bytes? std) (string/has-prefix? "/" std))
    std
    (string "/std:c" std)))
(defn- cc/msvc-c++std
  []
  (def std (dyn *c++-std* 14))
  (if (and (bytes? std) (string/has-prefix? "/" std))
    std
    (string "/std:c++" std)))
(defn- cc/msvc-compile-paths
  []
  (def cpath (cc/msvc-cpath))
  (def sp (dyn *syspath* "."))
  (if (= sp cpath)
    [(string "/I" cpath)]
    [(string "/I" cpath) (string "/I" sp)]))
(defn- cc/msvc-link-paths
  []
  (def cpath (cc/msvc-cpath))
  (def sp (dyn *syspath* "."))
  (if (= sp cpath)
    [(string "/LIBPATH:" cpath)]
    [(string "/LIBPATH:" cpath) (string "/LIBPATH:" sp)]))
(defn- cc/msvc-libs []
  (seq [l :in (dyn *msvc-libs* [])]
    (if (string/has-suffix? ".lib" l)
      l
      (string l ".lib"))))
(defn- cc/cl.exe [] "cl.exe")
(defn- cc/link.exe [] "link.exe")
(defn- cc/lib.exe [] "lib.exe")

(defn cc/msvc-compile-c
  "Compile a C source file with MSVC to an object file. Return the command arguments."
  [from to]
  (cc/exec [(cc/cl.exe) "/c" (cc/msvc-cstd) "/utf-8" "/nologo" ;(cc/cflags) ;(cc/msvc-compile-paths) ;(cc/msvc-opt) ;(cc/msvc-defines)
         from (string "/Fo" to)]
        [from] [to] (string "compiling " from "...")))

(defn cc/msvc-compile-c++
  "Compile a C++ source file with MSVC to an object file. Return the command arguments."
  [from to]
  (cc/exec [(cc/cl.exe) "/c" (cc/msvc-c++std) "/utf-8" "/nologo" "/EHsc" ;(cc/c++flags) ;(cc/msvc-compile-paths) ;(cc/msvc-opt) ;(cc/msvc-defines)
         from (string "/Fo" to)]
        [from] [to] (string "compiling " from "...")))

(defn cc/msvc-link-shared
  "Link a C/C++ program with MSVC to make a shared library. Return the command arguments."
  [objects to]
  (cc/exec [(cc/link.exe) "/nologo" "/DLL" (string "/OUT:" to) ;objects ;(cc/msvc-link-paths) ;(cc/msvc-libs) ;(cc/lflags)]
        objects [to] (string "linking " to "...")))

(defn cc/msvc-link-executable
  "Link a C/C++ program with MSVC to make an executable. Return the command arguments."
  [objects to &opt _make-static]
  (cc/exec [(cc/link.exe) "/nologo" (string "/OUT:" to) ;objects ;(cc/msvc-link-paths) ;(cc/msvc-libs) ;(cc/lflags)]
        objects [to] (string "linking " to "...")))

(defn cc/msvc-make-archive
  "Make an archive file with MSVC. Return the command arguments."
  [objects to]
  (cc/exec [(cc/lib.exe) "/nologo" (string "/OUT:" to) ;objects]
        objects [to] (string "archiving " to "...")))

# Compound commands

(defn- cc/msvc-compile-many
  "Compile a number of source files, and return the
  generated objects files, as well as a boolean if any cpp
  source files were found."
  [sources cmds-into ext]
  (def objects @[])
  (each source sources
    (def o (cc/out-path source ext "\\"))
    (def source-type (cc/classify-source source))
    (case source-type
      :o
      (array/push objects source)
      :c
      (do
        (array/push cmds-into (cc/msvc-compile-c source o))
        (array/push objects o))
      :c++
      (do
        (array/push cmds-into (cc/msvc-compile-c++ source o))
        (array/push objects o))
      # else
      (errorf "unknown source file type for %v" source)))
  objects)

(defn cc/msvc-compile-and-link-shared
  "Compile and link a shared C/C++ library. Return an array of commands."
  [to & sources]
  (def res @[])
  (def objects (cc/msvc-compile-many sources res ".shared.o"))
  (array/push
    res
    (cc/msvc-link-shared objects to)))

(defn cc/msvc-compile-and-link-executable
  "Compile and link an executable C/C++ program. Return an array of commands."
  [to & sources]
  (def res @[])
  (def objects (cc/msvc-compile-many sources res ".executable.o"))
  (array/push
    res
    (cc/msvc-link-executable objects to)))

(defn cc/msvc-compile-and-make-archive
  "Compile and create a static archive. Return an array of commands."
  [to & sources]
  (def res @[])
  (def objects (cc/msvc-compile-many sources res ".static.o"))
  (array/push res (cc/msvc-make-archive objects to)))

###
### *visit* functions
###

(defn cc/visit-do-nothing
  "A visiting function that has no side effects and therefor does nothing."
  [&])

(defn cc/visit-clean
  "A visiting function that will remove all outputs."
  [_cmd _inputs outputs _message]
  (print "cleaing " (string/join outputs " ") "...")
  (each output outputs
    (sh/rm output)))

(defn cc/visit-generate-makefile
  "A function that can be provided as `(dyn *visit*)` that will generate Makefile targets."
  [cmd inputs outputs message]
  (assert (one? (length outputs)) "only single outputs are supported for Makefile generation")
  (print ".PHONY: _all")
  (print "_all: " (string/join outputs " "))
  (print (first outputs) ": " (string/join inputs " "))
  (print "\t@echo " (describe message))
  (print "\t@'" (string/join cmd "' '") "'\n"))

(def cc/ver (tuple ;(map scan-number (string/split "." janet/version))))

(defn- cc/exec-linebuffered
  "Line buffer compiler output so we can run commands in parallel"
  [args]
  (when (< cc/ver [1 36 0]) # os/pipe with flags is a new feature
    (break (eprint (sh/exec-slurp ;args))))
  (def [r w] (os/pipe :W))
  (def proc (os/spawn args :p {:out w :err w}))
  (var exit nil)
  (ev/gather
    (each line (stream/lines r)
      (eprint line))
    (do
      (set exit (os/proc-wait proc))
      (ev/close w)))
  (if (not= 0 exit) (error "non-zero exit code"))
  exit)

(defn cc/visit-execute
  "A function that can be provided as `(dyn *visit*)` that will execute commands."
  [cmd _inputs _outputs message]
  (if (dyn :verbose)
    (do
      (eprint (string/join cmd " "))
      (flush)
      (cc/exec-linebuffered cmd))
    (do
      (print message)
      (with [proc (os/spawn cmd :p {:out :pipe :err :pipe})]
        (def [out err exit] (ev/gather
                              (ev/read (proc :out) :all)
                              (ev/read (proc :err) :all)
                              (os/proc-wait proc)))
        (unless (zero? exit) # only print output on failure
          (if out (eprint (string/trimr out)))
          (if err (eprint (string/trimr err)))
          (error "non-zero exit code"))))))

(defn cc/visit-execute-if-stale
  "A function that can be provided as `(dyn *visit*)` that will execute a command
  if inputs are newer than outputs, providing a simple, single-threaded, incremental build tool.
  This is not optimal for parallel builds, but is simple and works well for small projects."
  [cmd inputs outputs message]
  (defn otime [file] (or (os/stat file :modified) math/-inf))
  (defn itime [file] (or (os/stat file :modified) (errorf "%v: input file %v does not exist!" message file)))
  (def im (max ;(map itime inputs)))
  (def om (min ;(map otime outputs)))
  (if (>= om im) (break))
  (cc/visit-execute cmd inputs outputs message))

(defn cc/visit-execute-quiet
  "A function that can be provided as `(dyn *visit*)` that will execute commands quietly."
  [cmd _inputs _outputs _message]
  (with [devnull (sh/devnull)]
    (os/execute cmd :px {:out devnull :err devnull})))

(defn cc/visit-add-rule
  "Used in conjuction with spork/build-rules. Adds rules to the (dyn *rules* (curenv))"
  [cmd inputs outputs message]
  (def rules (dyn *rules* (curenv)))
  (build-rules/build-rule
    rules outputs @[;inputs]
    (cc/visit-execute cmd inputs outputs message)))

###
### Library discovery and self check
###

(defn cc/check-library-exists
  "Check if a library exists on the current POSIX system. Will run a test compilation
  and return true if the compilation succeeds. Libname is passed directly to the compiler/linker, such as `-lm` on GNU/Linux."
  [libname &opt binding test-source-code]
  (default binding *libs*)
  (default test-source-code "int main() { return 0; }")
  (def temp (string "_temp" (gensym)))
  (def src (string temp "/" (gensym) ".c"))
  (def executable (string temp "/" (gensym)))
  (defer (sh/rm temp)
    (os/mkdir temp)
    (spit src test-source-code)
    (try
      (with-dyns [*visit* cc/visit-execute-quiet
                  *build-dir* temp
                  *static-libs* []
                  *dynamic-libs* []
                  *libs* []]
        (setdyn binding [libname])
        (cc/compile-and-link-executable executable src)
        (with [devnull (sh/devnull)]
          (os/execute [executable] :x {:out devnull :err devnull}))
        true)
      ([_e]
        false))))

(defn- cc/search-libs-impl
  [dynb cc/libs]
  (def ls (cc/getsetdyn dynb))
  (def notfound @[])
  (each lib cc/libs
    (def llib (if (string/has-prefix? "-l" lib) lib (string "-l" lib)))
    (if (cc/check-library-exists llib dynb)
      (array/push ls llib)
      (array/push notfound lib)))
  notfound)

(defn cc/search-libraries
  "Search for libraries on the current POSIX system and configure `(dyn *libs*)`.
  This is done by checking for the existence of libraries with
  `check-library-exists`. Returns an array of libraries that were not found."
  [& cc/libs]
  (cc/search-libs-impl *libs* cc/libs))

(defn cc/search-static-libraries
  "Search for static libraries on the current POSIX system and configure `(dyn *static-libs*)`.
  This is done by checking for the existence of libraries with
  `check-library-exists`. Returns an array of libraries that were not found."
  [& cc/libs]
  (cc/search-libs-impl *static-libs* cc/libs))

(defn cc/search-dynamic-libraries
  "Search for dynamic libraries on the current POSIX system and configure `(dyn *dynamic-libraries*)`.
  This is done by checking for the existence of libraries with
  `check-library-exists`. Returns an array of libraries that were not found."
  [& cc/libs]
  (cc/search-libs-impl *dynamic-libs* cc/libs))

###
### Package Config wrapper to find libraries and set flags
###

(defn- cc/pkg-config-impl
  [& cmd]
  (def pkg-config-path (or (cc/lib-path) (dyn *syspath* ".")))
  # Janet may be installed in a non-standard location, so we need to tell pkg-config where to look
  # by appending PKG_CONFIG_PATH environment variable.
  (def pkp (path/join pkg-config-path "pkgconfig"))
  (def s (if (= (cc/target-os) :windows) ";" ":"))
  (def pcp (string (if-let [exist (os/getenv "PKG_CONFIG_PATH")] (string exist s) "") pkg-config-path s pkp))
  (def extra (dyn *pkg-config-flags* []))
  (def output
    (with [proc (os/spawn ["pkg-config" ;extra ;cmd] :xpe {:out :pipe "PKG_CONFIG_PATH" pcp})]
      (let [[out] (ev/gather
                    (ev/read (proc :out) :all)
                    (os/proc-wait proc))]
        (if out (string/trimr out) ""))))

  (filter next (string/split " " (string/trim output))))

(defn cc/pkg-config
  "Setup defines, cflags, and library flags from pkg-config."
  [& pkg-config-libraries]
  (def cc/cflags (cc/pkg-config-impl "--cflags" ;pkg-config-libraries))
  (def cc/lflags (cc/pkg-config-impl "--libs-only-L" "--libs-only-other" ;pkg-config-libraries))
  (def cc/libs (cc/pkg-config-impl "--libs-only-l" ;pkg-config-libraries))
  (def leftovers (cc/search-libraries ;cc/libs))
  (unless (empty? leftovers)
    (errorf "could not find libraries %j" leftovers))
  (setdyn *cflags* (array/concat @[] (cc/getsetdyn *cflags*) cc/cflags))
  (setdyn *lflags* (array/concat @[] (cc/getsetdyn *lflags*) cc/lflags))
  nil)

###
### Save and Load configuration
###

# Get all dynamic bindings in this module and make them saveable.
(def- cc/save-map
  (tabseq
    [k :keys (curenv) :when (symbol? k) :when (string/has-prefix? "*" k)]
    (keyword (slice k 1 -2)) true))

(defn cc/save-settings
  "Get a snapshot of the current settings for various compiler flags, libraries, defines, etc. that can be loaded later."
  []
  (freeze
    (tabseq [k :keys cc/save-map]
      k (dyn k))))

(defn cc/load-settings
  "Load settings from a snapshot of settings saved with `save-settings`."
  [settings]
  (eachp [k v] (thaw settings)
    (setdyn k v)))


(defdyn *gitpath* "What git command to use to fetch dependencies")
(defdyn *tarpath* "What tar command to use to fetch dependencies")
(defdyn *curlpath* "What curl command to use to fetch dependencies")
(defdyn *pkglist* "Override the default package listing if a `pkgs` bundle is not currently installed.")

(def- pm/filepath-replacer
  "Convert url with potential bad characters into a file path element."
  (peg/compile ~(% (any (+ (/ '(set "<>:\"/\\|?*") "_") '1)))))

(defn- pm/filepath-replace
  "Remove special characters from a string or path
  to make it into a path segment."
  [repo]
  (get (peg/match pm/filepath-replacer repo) 0))

(defn pm/git
  "Make a call to git."
  [& args]
  (sh/exec (dyn *gitpath* "git") ;args))

(defn pm/tar
  "Make a call to tar."
  [& args]
  (sh/exec (dyn *tarpath* "tar") ;args))

(defn pm/curl
  "Make a call to curl"
  [& args]
  (sh/exec (dyn *curlpath* "curl") ;args))

(defn- pm/getpkglist []
  (dyn *pkglist* pm-config/default-pkglist))

(var- pm/bundle-install-recursive nil)

(defn- pm/resolve-bundle-name
  "Convert short bundle names to full tables."
  [bname]
  (if (string/find ":" bname) (break bname))
  (let [pkgs (try
               (require "pkgs")
               ([_err]
                 (pm/bundle-install-recursive (pm/getpkglist))
                 (require "pkgs")))
        url (get-in pkgs ['packages :value (symbol bname)])]
    (unless url
      (error (string "bundle " bname " not found.")))
    url))

(defn pm/resolve-bundle
  ```
  Convert any bundle string/table to the normalized table form. `bundle` can be any of the following forms:

  * A short name that indicates a package from the package listing.
  * A URL or path to a git repository
  * A URL or path to a .tar.gz archive
  * A string of 2 parts separated by "::" - {type}::{path-or-url}
  * A string of 3 parts separated by "::" - {type}::{path-or-url}::{tag}
  * A table or struct with the following keys:

  * `:url` or `:repo` - the URL or path of the git repository or of the .tar.gz file. Required.
  * `:tag`, `:sha`, `:commit`, or `:ref` - The revision to checkout from version control. Optional.
  * `:type` - The dependency type, either `:git`, `:tar`, or `:file`. The default is `:git`. Optional.
  ```
  [bundle]
  (var repo nil)
  (var tag nil)
  (var btype :git)
  (if (dictionary? bundle)
    (do
      (set repo (or (get bundle :url) (get bundle :repo)))
      (set tag (or (get bundle :tag) (get bundle :sha) (get bundle :commit) (get bundle :ref)))
      (set btype (get bundle :type :git)))
    (let [parts (string/split "::" bundle)]
      (case (length parts)
        1 (set repo (get parts 0))
        2 (do (set repo (get parts 1)) (set btype (keyword (get parts 0))))
        3 (do
            (set btype (keyword (get parts 0)))
            (set repo (get parts 1))
            (set tag (get parts 2)))
        (errorf "unable to parse bundle string %v" bundle))))
  (set repo (if (= btype :file) (os/realpath repo) (pm/resolve-bundle-name repo)))
  (when (string/has-prefix? "git+" repo)
    (set repo (string/slice repo 4 -1))
    (assert (= :git btype)))
  {:url repo :tag tag :type btype})

(defn pm/update-git-bundle
  "Fetch latest tag version from remote repository"
  [bundle-dir tag]
  # Tag can be a hash, e.g. in lockfile. Some Git servers don't allow
  # fetching arbitrary objects by hash. First fetch ensures that we have
  # all objects locally.
  (pm/git "-C" bundle-dir "fetch" "--tags" "origin")
  (pm/git "-C" bundle-dir "fetch" "--depth" "1" "origin" (or tag "HEAD"))
  (pm/git "-C" bundle-dir "reset" "--hard" "FETCH_HEAD"))

(defn pm/download-git-bundle
  "Download a git bundle from a remote respository."
  [bundle-dir url tag]
  (var fresh false)
  (if (dyn :offline)
    (if (not= :directory (os/stat bundle-dir :mode))
      (error (string "did not find cached repository for dependency " url))
      (set fresh true))
    (when (os/mkdir bundle-dir)
      (set fresh true)
      (pm/git "-c" "init.defaultBranch=master" "-C" bundle-dir "init")
      (pm/git "-C" bundle-dir "remote" "add" "origin" url)
      (pm/update-git-bundle bundle-dir tag)))
  (unless (or (dyn :offline) fresh)
    (pm/update-git-bundle bundle-dir tag))
  (unless (dyn :offline)
    (pm/git "-C" bundle-dir "submodule" "update" "--init" "--recursive")))

(defn pm/download-tar-bundle
  "Download a dependency from a tape archive. The archive should have exactly one
  top level directory that contains the contents of the project."
  [bundle-dir url]
  (def has-gz (string/has-suffix? "gz" url))
  (def is-remote (string/find ":" url))
  (def dest-archive (if is-remote (string bundle-dir "/bundle-archive." (if has-gz "tar.gz" "tar")) url))
  (os/mkdir bundle-dir)
  (when is-remote
    (pm/curl "-sL" url "--output" dest-archive))
  (spit (string bundle-dir "/.bundle-tar-url") url)
  (def tar-flags (if has-gz "-xzf" "-xf"))
  (pm/tar tar-flags dest-archive "--strip-components=1" "-C" bundle-dir))

(defn- pm/get-cachedir
  [url bundle-type tag]
  (def url (if (= tag :file) (os/realpath url) url)) # use absolute paths for file caches
  (def cache (path/join (dyn *syspath*) ".cache"))
  (os/mkdir cache)
  (def id (pm/filepath-replace (string bundle-type "_" tag "_" url)))
  (path/join cache id))

(defn pm/download-bundle
  "Download the package source (using git, curl+tar, or a file copy) to the local cache. Return the
  path to the downloaded or cached soure code."
  [url bundle-type &opt tag]
  (var bundle-dir (pm/get-cachedir url bundle-type tag))
  (case bundle-type
    :git (pm/download-git-bundle bundle-dir url tag)
    :tar (pm/download-tar-bundle bundle-dir url)
    :file (set bundle-dir url)
    (errorf "unknown bundle type %v" bundle-type))
  bundle-dir)

(defn- pm/slurp-maybe
  [path]
  (when-with [f (file/open path)]
    (def data (file/read f :all))
    data))

(defn pm/load-project-meta
  "Load the metadata from a project.janet file without doing a full evaluation
  of the project.janet file. Returns a struct with the project metadata. Raises
  an error if no metadata found."
  [dir]
  # Check bundle paths first
  (def infopath (path/join dir "bundle" "info.jdn"))
  (def infopath2 (path/join dir "info.jdn"))
  (when-let [d (pm/slurp-maybe infopath)] (break (parse d)))
  (when-let [d (pm/slurp-maybe infopath2)] (break (parse d)))
  # Then check project.janet for declare-project
  (def path (path/join dir "project.janet"))
  (def src (slurp path))
  (def p (parser/new))
  (parser/consume p src)
  (parser/eof p)
  (var ret nil)
  (while (parser/has-more p)
    (if ret (break))
    (def item (parser/produce p))
    (match item
      ['declare-project & rest] (set ret (table ;rest))))
  (unless ret
    (errorf "no metadata found in %s" path))
  # Fix the issue of :dependencies having a different meaning in the metadata
  (def deps (seq [d :in (get ret :dependencies @[])] d))
  (put ret :jpm-dependencies deps)
  (put ret :dependencies @["spork"])
  ret)

(def- pm/shimcode
  ````
(if (dyn :install-time-syspath)
  (use @install-time-syspath/spork/declare-cc)
  (use spork/declare-cc))
(dofile "project.janet" :env (jpm-shim-env))
````)

(defn- pm/manifest-pm-extract
  "Extract the package manager source of a manifest. Needs to handle both pm and default janet installs."
  [m]
  (or
    (get m :pm) # installed by pm, has extra info like git repo, etc.
    (table/to-struct
      (merge-into @{:type :file :url (get m :local-source)} (get m :info {}))))) # just a path on disk, native janet support

(defn- pm/bundle-name-to-bundle
  "Convert an installed bundle name to a pm bundle. Also handles bundles not installed with pm for debugging purposes."
  [bundle-name]
  (pm/manifest-pm-extract (bundle/manifest bundle-name)))

(defn- pm/name-lookup
  "Find the bundle name of a bundle address"
  [bundle-addr]
  (def {:url url
        :tag tag
        :type bundle-type} bundle-addr)
  (def key [url tag bundle-type])
  (var result nil)
  (each d (bundle/list)
    (def m (bundle/manifest d))
    (when m
      (def pm (pm/manifest-pm-extract m))
      (def check [(get pm :url) (get pm :tag) (get pm :type)])
      (when (= check key)
        (set result (get m :name))
        (break))))
  result)

(defn pm/jpm-dep-to-bundle-dep
  "Convert a remote dependency identifier to a bundle dependency name. `dep-name` is any value that can be passed to `pm-install`.
  Will return a string than can be passed to `bundle/reinstall`, `bundle/uninstall`, etc."
  [dep-name]
  (def bundle-name (pm/name-lookup (pm/resolve-bundle dep-name)))
  (unless bundle-name
    (eprintf "unable to resolve jpm style dependency %q to a local bundle" dep-name))
  bundle-name)

(defn- pm/project-janet-shim
  ``If not already present, add a bundle/ directory to a legacy jpm project directory to allow installation with janet --install. Adds "spork"
  as a dependency. Return true if a default bundle/ directory was generated, false otherwise.``
  [dir]
  (def project (path/join dir "project.janet"))
  (def bundle-hook-dir (path/join dir "bundle"))
  (def bundle-janet-path (path/join dir "bundle.janet"))
  (def bundle-init (path/join dir "bundle" "init.janet"))
  (def bundle-info (path/join dir "bundle" "info.jdn"))
  (if (os/stat bundle-hook-dir :mode) (break false))
  (if (os/stat bundle-janet-path :mode) (break false))
  (assert (os/stat project :mode) "did not find bundle directory, bundle.janet or project.janet")
  (printf "generating %s" bundle-hook-dir)
  (def meta (pm/load-project-meta dir))
  (os/mkdir bundle-hook-dir)
  (spit bundle-init pm/shimcode)
  (spit bundle-info (string/format "%j" meta))
  true)

(defn- pm/dyn-env
  []
  (def e (make-env))
  (defn- add1
    [x]
    (eachp [k v] x
      (if (keyword? k)
        (put e k v)))
    (if-let [p (getproto x)]
      (add1 p)))
  (add1 (curenv))
  e)

(defn pm/pm-install
  "Install a bundle given a url, short name, or full 'bundle code'. The bundle source code will be fetched from
  git or a url, then installed with `bundle/install`."
  [bundle-code &named no-deps force-update no-install auto-remove]
  (def bundle (pm/resolve-bundle bundle-code))
  (def name (pm/name-lookup bundle))
  (if (and name (not force-update)) (break))
  (def {:url url :type bundle-type :tag tag} bundle)
  (def bdir (pm/download-bundle url bundle-type tag))
  (def did-shim (pm/project-janet-shim bdir))
  (def info (pm/load-project-meta bdir))
  (def infoname (get info :name))
  (when (and (not name) (bundle/installed? infoname))
    (def existing (pm/bundle-name-to-bundle infoname))
    (eprintf "a conflicting bundle %v is already installed, keeping that one." infoname)
    (eprintf "  existing bundle: %.99M" existing)
    (eprintf "  skipped bundle   %.99M" bundle)
    (break))
  (def jpm-deps (get info :jpm-dependencies @[]))
  (unless no-deps
    (each dep jpm-deps
      (pm/pm-install dep :force-update force-update :auto-remove true)))
  (when did-shim
    # patch deps after installing all jpm dependencies. This allows the bundle/* module to track dependencies, and
    # prevent things like uninstalling a dependency, breaking another installed package.
    (def deps (seq [d :in jpm-deps] (pm/jpm-dep-to-bundle-dep d)))
    (def deps (filter identity deps))
    (unless (index-of "spork" deps)
      # if spork is not installed, we are installing to a different tree.
      (when (bundle/installed? "spork") (array/push deps "spork")))
    (put info :dependencies deps)
    (spit (path/join bdir "bundle" "info.jdn") (string/format "%.99m\n" info)))
  (def config @{:pm bundle :installed-with "spork/pm" :auto-remove auto-remove})
  (unless no-install
    (with-env (pm/dyn-env) # work around bundle/* quirk with accidentally injecting hooks.
      (if (and name (bundle/installed? name))
        (bundle/reinstall name :config config ;(kvs config))
        (bundle/install bdir :config config ;(kvs config))))))

(defn pm/local-hook
  "Run a bundle hook on the local project."
  [hook & args]
  (pm/project-janet-shim ".")
  (def [fullpath _] (module/find "/bundle"))
  (unless fullpath (break))
  (def module (require "/bundle"))
  (def hookf (module/value module (symbol hook)))
  (unless hookf (break))
  (hookf ;args))

###
### Lock files
###

(defn pm/save-lockfile
  "Create a lockfile that can be used to reinstall all currently installed bundles at a later date."
  [lock-dest]
  (def lock @[])
  (each b (bundle/topolist)
    (def manifest (bundle/manifest b))
    (def config (get manifest :config))
    (def pm (get manifest :pm (get config :pm {:type :file :url (get manifest :local-source)})))
    (def name (get manifest :name))
    (array/push lock {:name name :pm pm :config config}))
  (def buf @"[\n")
  (each d lock
    (string/format "%j" d) # check JDN correctness
    (buffer/format buf "%.99m\n" d))
  (buffer/push buf "]\n")
  (spit lock-dest buf)
  lock)

(defn pm/load-lockfile
  "Install all saved dependencies in a lockfile."
  [lock-src]
  (def lock (-> lock-src slurp parse))
  (each d lock
    (def {:pm pm :name name} d)
    (pm/pm-install pm :force-update true :no-deps true)
    (assert (bundle/installed? name))))

(set pm/bundle-install-recursive pm/pm-install)

###
### Configuration via environment variables
###

###
### Project scaffolding
###
### Generate new projects quickly, ported from jpm
###

(def- pm/template-peg :flycheck
  "Extract string pieces to generate a templating function"
  (peg/compile
    ~{:sub (group
             (+ (* "${" '(to "}") "}")
                (* "$" '(some (range "az" "AZ" "09" "__" "--")))))
      :main (any (* '(to (+ "$$" -1 :sub)) (+ (/ '"$$" "$") :sub 0)))}))

(defn- pm/make-template
  "Make a simple string template as defined by Python PEP292 (shell-like $ substitution).
  Also allows dashes in indentifiers."
  [source]
  (def frags (peg/match pm/template-peg source))
  (def partitions (partition-by type frags))
  (def string-args @[])
  (each chunk partitions
    (case (type (get chunk 0))
      :string (array/push string-args (string ;chunk))
      :array (each sym chunk
               (array/push string-args ~(,get opts ,(keyword (first sym)))))))
  ~(fn [opts] opts (,string ,;string-args)))

(defmacro pm/deftemplate
  ```
  Define a inline template as defined by Python PEP292 (shell-like $ substitution),
  and also allows dashes in indentifiers.

  It defines new function `template-name` that takes a dictionary `opts` containing
  substitutions as an argument. Keys should be keywords with the same name (sans :)
  as the substitution keys.

  Template is parsed from the last element of `body` which should be string and can contain substitutions.
  ```
  [template-name & body]
  ~(def ,template-name ,;(slice body 0 -2) ,(pm/make-template (last body))))

(defn pm/opt-ask
  ```
  Ask user for the value of the `key`. `input-options` should be a table with default values.
  If the default value for the `key` is not `nil`, it will return that.
  ```
  [key input-options]
  (def dflt (get input-options key))
  (if (nil? dflt)
    (string/trim (getline (string key "? ")))
    dflt))

(pm/deftemplate project-template
  :private
  ````
  (declare-project
    :name "$name"
    :description ```$description ```
    :author ```$author ```
    :dependencies @["spork"]
    :version "0.0.0")

  (declare-source
    :source ["$name"])
  ````)

(pm/deftemplate native-project-template
  :private
  ````
  (declare-project
    :name "$name"
    :description ```$description ```
    :author ```$author ```
    :dependencies @["spork"]
    :version "0.0.0")

  (declare-source
    :source ["$name"])

  (declare-native
    :name "${name}-native"
    :source @["c/module.c"])
  ````)

(pm/deftemplate module-c-template
  :private
  ```
  #include <janet.h>

  /***************/
  /* C Functions */
  /***************/

  JANET_FN(cfun_hello_native,
           "($name/hello-native)",
           "Evaluate to \"Hello!\". but implemented in C.") {
      janet_fixarity(argc, 0);
      (void) argv;
      return janet_cstringv("Hello!");
  }

  /****************/
  /* Module Entry */
  /****************/

  JANET_MODULE_ENTRY(JanetTable *env) {
      JanetRegExt cfuns[] = {
          JANET_REG("hello-native", cfun_hello_native),
          JANET_REG_END
      };
      janet_cfuns_ext(env, "$name", cfuns);
  }
  ```)

(pm/deftemplate exe-project-template
  :private
  ````
  (declare-project
    :name "$name"
    :description ```$description ```
    :author ```$author ```
    :dependencies @["spork"]
    :version "0.0.0")

  (declare-executable
    :name "$name"
    :entry "$name/init.janet")
  ````)

(pm/deftemplate readme-template
  :private
  ```
  # ${name}

  Add project description here.
  ```)

(pm/deftemplate changelog-template
  :private
  ```
  # Changelog
  All notable changes to this project will be documented in this file.
  Format for entries is <version-string> - release date.

  ## 0.0.0 - $date
  - Created this project.
  ```)

(pm/deftemplate license-template
  :private
  ```
  Copyright (c) $year $author and contributors

  Permission is hereby granted, free of charge, to any person obtaining a copy of
  this software and associated documentation files (the "Software"), to deal in
  the Software without restriction, including without limitation the rights to
  use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
  of the Software, and to permit persons to whom the Software is furnished to do
  so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
  ```)

(pm/deftemplate init-template
  :private
  ```
  (defn hello
    `Evaluates to "Hello!"`
    []
    "Hello!")

  (defn main
    [& args]
    (print (hello)))
  ```)

(pm/deftemplate test-template
  :private
  ```
  (use ../$name/init)

  (assert (= (hello) "Hello!"))
  ```)

(pm/deftemplate native-test-template
  :private
  ```
  (use ${name}-native)

  (assert (= (hello-native) "Hello!"))
  ```)

(pm/deftemplate bundle-init-template
  :private
  ````
  (defn install
    [manifest &]
    (bundle/add manifest "$name"))

  (defn build
    [&]
    (print "Nothing to build!"))

  (defn clean
    [&]
    (print "Nothing to clean!"))

  (defn check
    [&]
    (var pass-count 0)
    (var total-count 0)
    (def failing @[])
    (each dir (sorted (os/dir "test"))
      (def path (string "test/" dir))
      (when (string/has-suffix? ".janet" path)
        (def pass (zero? (os/execute [(dyn *executable* "janet") "--" path] :p)))
        (++ total-count)
        (unless pass (array/push failing path))
        (when pass (++ pass-count))))
    (if (= pass-count total-count)
      (print "All tests passed!")
      (do
        (printf "%d of %d passed." pass-count total-count)
        (print "failing scripts:")
        (each f failing
          (print "  " f))
        (os/exit 1))))
  ````)

(pm/deftemplate bundle-info-template
  :private
  ````
  {:name "$name"
   :description ```$description ```
   :author ```$author ```
   :dependencies @[]
   :version "0.0.0"}
  ````)

(defn- pm/format-date
  []
  (def x (os/date))
  (string/format "%d-%.2d-%.2d" (x :year) (inc (x :month)) (inc (x :month-day))))

(defn pm/scaffold-project
  "Generate a standardized project scaffold."
  [name &opt options]
  (default options {})
  (def year (get (os/date) :year))
  (def author (pm/opt-ask :author options))
  (def description (pm/opt-ask :description options))
  (def date (pm/format-date))
  (def scaffold-native (get options :c))
  (def scaffold-exe (get options :exe))
  (def scaffold-spork-free (get options :no-spork))
  (def template-opts (merge-into @{:name name :year year :author author :date date :description description} options))
  (print "creating project directory for " name)
  (os/mkdir name)
  (os/mkdir (string name "/test"))
  (os/mkdir (string name "/" name))
  (os/mkdir (string name "/bin"))
  (spit (string name "/" name "/init.janet") (init-template template-opts))
  (spit (string name "/test/basic.janet") (test-template template-opts))
  (spit (string name "/README.md") (readme-template template-opts))
  (spit (string name "/LICENSE") (license-template template-opts))
  (spit (string name "/CHANGELOG.md") (changelog-template template-opts))
  (cond
    scaffold-spork-free
    (do
      (os/mkdir (string name "/bundle"))
      (spit (string name "/bundle/info.jdn") (bundle-info-template template-opts))
      (spit (string name "/bundle/init.janet") (bundle-init-template template-opts)))
    scaffold-native
    (do
      (os/mkdir (string name "/c"))
      (spit (string name "/c/module.c") (module-c-template template-opts))
      (spit (string name "/test/native.janet") (native-test-template template-opts))
      (spit (string name "/project.janet") (native-project-template template-opts)))
    scaffold-exe
    (do
      (spit (string name "/project.janet") (exe-project-template template-opts)))
    (do
      (spit (string name "/project.janet") (project-template template-opts)))))

(pm/deftemplate enter-shell-template
  :private
    ````
    # . bin/activate
    if [ -n "$${_OLD_JANET_PATH+set}" ]; then
      echo 'An environment is already active, please run `deactivate` first.';
    else
      _OLD_JANET_PATH="$$JANET_PATH";
      _OLD_JANET_PATH_SET="$${JANET_PATH+set}";
      _OLD_PATH="$$PATH";
      _OLD_PS1="$$PS1";
      JANET_PATH="$abspath";
      PATH="$$JANET_PATH"/bin:"$$PATH";
      PS1="("$name") $${PS1:-}";
      export _OLD_JANET_PATH;
      export _OLD_PATH;
      export _OLD_PS1;
      export JANET_PATH;
      export PATH;
      export PS1;
      deactivate() {
        PATH="$$_OLD_PATH";
        if [ -n "$$_OLD_JANET_PATH_SET" ]; then
          JANET_PATH="$$_OLD_JANET_PATH";
        else
          unset JANET_PATH;
        fi
        PS1="$$_OLD_PS1";
        export JANET_PATH;
        export PATH;
        export PS1;
        unset _OLD_JANET_PATH;
        unset _OLD_JANET_PATH;
        unset _OLD_PATH;
        unset _OLD_PS1;
        unset -f deactivate;
        export _OLD_JANET_PATH;
        export _OLD_PATH;
        export _OLD_PS1;
        hash -r 2> /dev/null;
      }
    fi
    hash -r 2> /dev/null;
    ````)

(pm/deftemplate enter-ps-template
  :private
  ````
  # . bin/activate.ps1
  $$global:_OLD_JANET_PATH=$$env:JANET_PATH
  $$global:_OLD_PATH=$$env:PATH
  $$env:JANET_PATH="$abspath"
  $$env:PATH=$$env:JANET_PATH + "\bin;" + $$env:PATH
  $$function:old_prompt = $$function:prompt
  function global:prompt {
    Write-Host "($name) " -NoNewline
    & $$function:old_prompt
  }
  function deactivate {
    $$env:PATH=$$global:_OLD_PATH
    $$env:JANET_PATH=$$global:_OLD_JANET_PATH
    Remove-Item function:\deactivate
    $$function:prompt = $$function:old_prompt
    Remove-Item function:\old_prompt
  }
  ````)

(pm/deftemplate enter-cmd-template
  :private
  ````
  @rem bin\activate.bat
  @set _OLD_JANET_PATH=%JANET_PATH%
  @set _OLD_PATH=%PATH%
  @set _OLD_PROMPT=%PROMPT%
  @set JANET_PATH=$abspath
  @set PATH=%JANET_PATH%\bin;%PATH%
  @set PROMPT=($path) %PROMPT%
  ````)

(pm/deftemplate exit-cmd-template
  :private
  ````
  @rem bin\deactivate.bat
  @set JANET_PATH=%_OLD_JANET_PATH%
  @set PATH=%_OLD_PATH%
  @set PROMPT=%_OLD_PROMPT%
  @set _OLD_JANET_PATH=%PATH%
  @set _OLD_PATH=%PATH%
  @set _OLD_PROMPT=%PROMPT%
  ````)

(defn pm/scaffold-pm-shell
  "Generate a pm shell with configuration already setup. If `copy-janet` is truthy, the Janet executable file
   will be bundled in the new environment"
  [path]
  (os/mkdir path)
  (os/mkdir (path/join path "bin"))
  (os/mkdir (path/join path "man"))
  (os/mkdir (path/join path "include"))
  (os/mkdir (path/join path "lib"))
  (os/mkdir (path/join path "lib" "pkgconfig"))
  (def opts {:path path :abspath (path/abspath path) :name (path/basename path)})
  (spit (path/join path "bin" "activate") (enter-shell-template opts))
  (spit (path/join path "bin" "activate.ps1") (enter-ps-template opts))
  (spit (path/join path "bin" "activate.bat") (enter-cmd-template opts))
  (spit (path/join path "bin" "deactivate.bat") (exit-cmd-template opts))
  (print "created project shell environment at " path)
  (print "(PowerShell) run `. " path "/bin/activate.ps1` to enter the new environment, then `deactivate` to exit.")
  (print "(CMD)        run `" path "\\bin\\activate` to enter the new environment, then `deactivate` to exit.")
  (print "(Unix sh)    run `. " path "/bin/activate` to enter the new environment, then `deactivate` to exit."))

(defn- pm/try-copy
    [src dest]
    (unless (sh/exists? src) (break false))
    (sh/copy src dest)
    true)

(defn pm/vendor-binaries-pm-shell
  ```
  Copy the Janet interpreter, shared libraries, and other installation files directly into the new shell environment. This allows
  updates and changes to the system configuration without breaking the shell.
  ```
  [path]
  (def is-win (= :windows (os/which)))
  (def exec-name (dyn *executable* "janet"))
  (def executable (sh/which exec-name))
  (assert (sh/exists? executable) "unable to resolve location of the janet binary. Is it on your path?")
  (def exe-ext (if is-win ".exe" ""))
  (def dest (string (path/join path "bin" "janet") exe-ext))
  (sh/copy executable dest)

  # Copy shared objects, DLLs, static archives, and janet.h into path
  (def [has-prefix prefix] (protect (cc/get-unix-prefix)))
  (when has-prefix
    (def lib (string prefix "/lib"))
    (def include (string prefix "/include"))
    (def parts (string/split "." janet/version))
    (def majorminor (string (in parts 0) "." (in parts 1)))

    # Copy libjanet.so (with correct symlinks and versions)
    (def libjanet-so-with-version (string "libjanet.so." majorminor))
    (def libjanet-so-with-full-version (string "libjanet.so." janet/version))
    (when (pm/try-copy (path/join lib libjanet-so-with-full-version) (path/join path "lib" libjanet-so-with-full-version))
      (os/link libjanet-so-with-full-version (path/join path "lib" libjanet-so-with-version) true)
      (os/link libjanet-so-with-full-version (path/join path "lib" "libjanet.so") true))

    # Copy libjanet.dylib (with correct symlinks and versions)
    (def libjanet-dylib-with-version (string "libjanet.dylib." majorminor))
    (def libjanet-dylib-with-full-version (string "libjanet.dylib." janet/version))
    (when (pm/try-copy (path/join lib libjanet-dylib-with-full-version) (path/join path "lib" libjanet-dylib-with-full-version))
      (os/link libjanet-dylib-with-full-version (path/join path "lib" libjanet-dylib-with-version) true)
      (os/link libjanet-dylib-with-full-version (path/join path "lib" "libjanet.dylib") true))

    # Copy libjanet.a (try versioned file first)
    (def libjanet-static-full (string "libjanet.a." janet/version))
    (if
      (pm/try-copy (path/join lib libjanet-static-full) (path/join path "lib" libjanet-static-full))
      (os/link (path/join path libjanet-static-full) (path/join path "lib" "libjanet.a") true)
      (pm/try-copy (path/join lib "libjanet.a") (path/join path "lib" "libjanet.a")))

    # Copy janet.h
    (os/mkdir (path/join path "include" "janet"))
    (pm/try-copy (path/join include "janet" "janet.h") (path/join path "include" "janet" "janet.h"))
    (pm/try-copy (path/join include "janet.h") (path/join path "include" "janet.h"))

    # pkgconfig
    (def [has-pkgconfig pkgconfig-in] (protect (slurp (path/join prefix "lib" "pkgconfig" "janet.pc"))))
    (when has-pkgconfig # we need to rewrite the pkgconfig file from the default install with new paths
      (def new-values
        {"prefix" (path/abspath path)
         "includedir" (path/abspath (path/join path "include" "janet"))
         "libdir" (path/abspath (path/join path "lib"))})
      (defn do-assignment [key old-value]
        (def new-value (get new-values key old-value))
        (string key "=" new-value))
      (def peg
        ~{:assignment (* '(some (range "az" "AZ" "09" "__")) "=" '(to (+ -1 "\n")))
          :remap (/ :assignment ,do-assignment)
          :main (accumulate (any (+ :remap '1)))})
      (def result (peg/match peg pkgconfig-in))
      (when result
        (spit (path/join path "lib" "pkgconfig" "janet.pc") (first result))))

    # End unix prefix code
    nil)

  # Copy shared objects, DLLs, static archives, and janet.h into path
  (def [has-winprefix win-prefix] (protect (cc/get-msvc-prefix)))
  (when has-winprefix
    (os/mkdir (path/join path "C"))
    (each name ["janet.lib" "janet.h" "janet.exp" "janet.c" "libjanet.lib"]
      (pm/try-copy (path/win32/join win-prefix "C" name) (path/win32/join path "C" name))))

  (print "Copied janet binaries and shared libraries into " path)
  nil)

(comment import ./util :prefix "")
(defdyn *gitpath* "What git command to use to fetch dependencies")
(defdyn *tarpath* "What tar command to use to fetch dependencies")
(defdyn *curlpath* "What curl command to use to fetch dependencies")

(def util/colours {:green "\e[32m" :red "\e[31m"})

(def util/psep "/")
(def util/wsep "\\")
(def util/sep (get {:windows util/wsep :cygwin util/wsep :mingw util/wsep} (os/which) util/psep))
(def util/dir-suffix "/.")

(def util/windows? (= util/sep util/wsep))

(def util/pathg ~{:main    (* (+ :abspath :relpath) (? :sep) -1)
             :abspath (* :root (any :relpath))
             :relpath (* :part (any (* :sep :part)))
             :root    (+ (* ,util/sep (constant ""))
                         (* '(* :a ":") ,util/wsep))
             :sep     (some ,util/sep)
             :part    '(some (* (! :sep) 1))})

# used for splitting POSIX paths
(def- util/posix-pathg ~{:main     (* (+ :abspath :relpath) (? :sep) -1)
                    :abspath  (* :root (any :relpath))
                    :relpath  (* :part (any (* :sep :part)))
                    :root     (* ,util/psep (constant ""))
                    :sep      (some ,util/psep)
                    :part     '(some (* (! :sep) 1))})

# Path

(def- util/this-file (os/realpath (dyn :current-file)))

# Independent functions

(defn util/abspath?
  [path]
  (if (= :windows (os/which))
    (not (nil? (peg/match ~(* (? (* :a ":")) ,util/wsep) path)))
    (string/has-prefix? util/psep path)))

(defn util/apart
  [path &opt posix?]
  (if (empty? path)
    []
    (or (peg/match (if posix? util/posix-pathg util/pathg) path)
        (error "invalid path"))))

(defn util/colour
  [c text &opt force?]
  (default force? false)
  (if (or (os/isatty) force?)
    (string (get util/colours c "\e[0m") text "\e[0m")
    text))

(defn util/devnull
  []
  (os/open (if (= :windows (os/which)) "NUL" "/dev/null") :rw))

(defn util/exec
  [cmd stdio & args]
  (def {:out out :err err} (if (nil? stdio) {:out nil :err nil} stdio))
  (def dn (if (or (nil? out) (nil? err)) (util/devnull)))
  (default out dn)
  (default err dn)
  (os/execute [(dyn (keyword cmd "path") (string cmd)) ;args] :px {:out out :err err}))

(defn util/fexists?
  [p]
  (= :file (os/stat p :mode)))

(defn util/legacy-bundles
  []
  (var res @[])
  (def mpath (string (dyn :syspath) util/sep ".manifests"))
  (unless (= :directory (os/stat mpath :mode))
    (break res))
  (each entry (os/dir mpath)
    (when (string/has-suffix? ".jdn" entry)
      (array/push res (string/slice entry 0 -5))))
  res)

(defn util/rmrf
  [path &opt ignore-check?]
  (case (os/lstat path :mode)
    # recursive delete directories
    :directory
    (do
      (def msg "cannot delete directory while current working directory is inside it")
      (assert (or ignore-check? (not (string/has-prefix? path (os/cwd)))) msg)
      (each subpath (os/dir path)
        (util/rmrf (string path util/sep subpath) true))
      (os/rmdir path))
     # do nothing if file does not exist
    nil
    nil
    # default
    (os/rm path)))

(defn util/slurp-maybe
  [path]
  (when-with [f (file/open path)]
    (file/read f :all)))

(defn util/spit-maybe
  [path s]
  (when-with [f (file/open path :wb)]
    (file/write f s)))

(defn util/tmp-dir
  []
  (unless (nil? (dyn :jeep-tmpdir))
    (break (dyn :jeep-tmpdir)))
  (def rng (math/rng))
  (loop [:repeat 5]
    (def total 8)
    (def b (buffer/new total))
    (loop [:repeat total]
      (buffer/push b (+ 65 (math/rng-int rng 25))))
    (def d (string "tmp_" b))
    (when (os/mkdir d)
      (setdyn :jeep-tmpdir (os/realpath d))
      (break)))
  (assert (dyn :jeep-tmpdir) "cannot create temporary directory")
  (dyn :jeep-tmpdir))

(defn util/url?
  [s]
  (def res (peg/match
             ~{:main (* :prot :domain :path :qs -1)
               :prot (? (* :w+ "://"))
               :domain (* :-w+ (some (* "." :-w+)))
               :-w+ (some (+ "-" :w))
               :path (? (* "/" (any (+ :w (set "./-_")))))
               :qs (? (* "?" (any (+ :w (set "./-_=")))))}
             s))
  (not (nil? res)))

# Directory functions

(defn util/abspath
  [path]
  (if (util/abspath? path)
    path
    (string (os/cwd) util/sep path)))

(defn util/mkdir
  [path &opt posix?]
  (def parts (util/apart path posix?))
  (cond
    # absolute path
    (= "" (first parts))
    (put parts 0 (if posix? util/psep util/sep))
    # Windows path beginning with drive letter
    (string/has-suffix? ":" (first parts))
    (put parts 0 (string (first parts) util/wsep)))
  (var res false)
  (def cwd (os/cwd))
  (each part parts
    (set res (os/mkdir part))
    (os/cd part))
  (os/cd cwd)
  res)

(defn util/parent
  [path &opt level posix?]
  (default level 1)
  (def parts (util/apart path posix?))
  (when (empty? parts)
    (break parts))
  (def s (if posix? util/psep util/sep))
  (def joined (string/join (array/slice parts 0 (- -1 level)) s))
  (if (= "" joined)
    util/sep
    joined))

(defn util/win-path
  [s]
  (def trailing (if (string/has-suffix? util/psep s) util/sep ""))
  (-> (util/apart s true) (string/join util/sep) (string trailing)))

# Other functions

(defn util/change-syspath
  [path]
  (def ap (util/abspath path))
  (unless (= :directory (os/stat ap :mode))
    (util/mkdir ap))
  (setdyn *syspath* ap))

(defn util/cleanup
  [cwd]
  (os/cd cwd)
  (when (def d (dyn :jeep-tmpdir))
    (util/rmrf d)
    (setdyn :jeep-tmpdir nil)))

(defn util/copy
  [src dest]
  (if (= :windows (os/which))
    (do
      (def copy-contents? (string/has-suffix? "\\." src))
      (def xcopy-src (if copy-contents?
                       (string (string/slice src 0 -2) "*")
                       src))
      (def express? (string/has-suffix? util/sep dest))
      (def xcopy-dest
        (cond
          express?
          dest
          copy-contents?
          (string dest util/sep)
          # default
          (do
            (def dir (util/parent dest))
            (def res (string dir util/sep (gensym)))
            # this is not cleaned up if there's an error
            (os/mkdir res)
            (string res util/sep))))
      (os/shell (string "C:\\Windows\\System32\\xcopy.exe "
                        xcopy-src
                        " "
                        xcopy-dest
                        " /e /h /i /k /o /r /x /y >NUL"))
      # Only move if not express and not copying contents
      (unless (or express? copy-contents?)
        (os/shell (string "C:\\Windows\\System32\\cmd.exe /c move "
                          (string/slice xcopy-dest 0 -2)
                          " "
                          dest
                          " >NUL"))))
    (os/execute ["cp" "-a" src dest] :px)))

(defn util/fetch-git
  [&named url tag dir]
  (assert url "function requires :url argument")
  (assert dir "function requires :dir argument")
  (default tag "HEAD")
  (def sha? (peg/match '(between 7 40 :h) tag))
  (if (= "HEAD" tag)
    (util/exec :git nil "clone" "--depth" "1" url dir)
    (if (not sha?)
      (util/exec :git nil "clone" "--branch" tag "--depth" "1" url dir)
      (do
        (util/exec :git nil "clone" "--filter" "blob:none" "--no-checkout" url dir)
        (util/exec :git nil "-C" dir "fetch" "origin" tag)
        (util/exec :git nil "-C" dir "checkout" tag))))
  dir)

(defn util/fetch-dep
  [dep &opt parent-dir]
  (def {:url url
        :tag tag
        :prefix prefix
        :paths files} dep)
  (default files
    (do
      (print "warning: use of :files is deprecated in vendored dependencies")
      (get dep :files)))
  (assert url (error "fetched bundles need a :url key"))
  (def tmp (util/tmp-dir))
  (def cwd (os/cwd))
  (defer (do
           (os/cd cwd)
           (util/rmrf tmp))
    (def local? (string/has-prefix? "file::" url))
    (def origin (if local? (string/slice url 6) url))
    (def src-dir (if local? origin (util/fetch-git :url url :tag tag :dir tmp)))
    (def dest-dir (if parent-dir
                    # use POSIX path separator to match info file
                    (string parent-dir (when prefix (string util/psep prefix)))
                    (or prefix ".")))
    (print "vendoring " (if local? (util/win-path origin) origin))
    (util/mkdir dest-dir true)
    (def to-plat (if (= util/wsep util/sep) util/win-path identity))
    (each f files
      (def [src dest] (if (indexed? f) f [f f]))
      (def full-src (string src-dir util/psep src))
      # use POSIX path separators to match info file
      (def posix-to (string dest-dir util/psep dest))
      (if (string/has-suffix? util/psep posix-to)
        (util/mkdir posix-to true)
        (util/mkdir (util/parent posix-to 1 true) true))
      (def posix-from
        (if (and (not (string/has-suffix? util/dir-suffix full-src))
                 (= :directory (os/stat full-src :mode)))
          (string full-src util/dir-suffix)
          full-src))
      (def from (to-plat posix-from))
      (def to (to-plat posix-to))
      (print "  copying " from " to " to)
      (util/copy from to))))

(defn util/load-info
  [&opt dir]
  (default dir ".")
  (def info-path1 (string/join [dir "bundle" "info.jdn"] util/sep))
  (def info-path2 (string/join [dir "info.jdn"] util/sep))
  (or (util/slurp-maybe info-path1) (util/slurp-maybe info-path2)))

(defn util/load-meta
  [&opt dir]
  (default dir ".")
  (when-let [info (util/load-info dir)]
    (parse info)))

(defn util/local-hook
  [name & args]
  (def [ok? module] (protect (require "/bundle" :fresh true)))
  (assert ok? "failed to load bundle script")
  (when-let [hookf (module/value module (symbol name))]
    (apply hookf args)
    true))

(defn util/save-info
  [jdn &opt dir]
  (default dir ".")
  (def info-path1 (string/join [dir "bundle" "info.jdn"] util/sep))
  (def info-path2 (string/join [dir "info.jdn"] util/sep))
  (or (util/spit-maybe info-path1 jdn) (util/spit-maybe info-path2 jdn)))

(defn util/version
  []
  (if (string/has-prefix? (os/realpath (dyn :syspath)) util/this-file)
    (get (bundle/manifest "jeep") :version)
    (do
      (def [r w] (os/pipe))
      (def bundle-root (-> util/this-file util/parent util/parent))
      (def ver "local")
      (os/cd bundle-root)
      (def [ok? res] (protect (util/exec :git {:out w} "describe" "--always" "--dirty")))
      (:close w)
      (if ok?
        (string ver "-" (string/trim (ev/read r :all)))
        ver))))


(defn- install/manifest-pm-extract
  [m]
  (or
    (get m :pm) # installed by pm, has extra info like git repo, etc.
    (table/to-struct
      (merge-into @{:type :file :url (get m :local-source)} (get m :info {}))))) # just a path on disk, native janet support

(defn- install/installed-lookup
  [bundle]
  (def {:url url
        :tag tag
        :type bundle-type} bundle)
  (def key [url tag bundle-type])
  (var result nil)
  (each d (bundle/list)
    (def m (bundle/manifest d))
    (when m
      (def pm (install/manifest-pm-extract m))
      (def check [(get pm :url) (get pm :tag) (get pm :type)])
      (when (= check key)
        (set result (get m :name))
        (break))))
  result)

(defn- install/bundle-name-to-bundle
  [bundle-name]
  (install/manifest-pm-extract (bundle/manifest bundle-name)))

(defn install/install
  # modified version of Spork's pm/pm-install function
  [id &named auto-remove force-update no-install replace?]
  (def bundle (pm/resolve-bundle id))
  (def installed-name (install/installed-lookup bundle))
  (when (and installed-name (not replace?) (not force-update))
    (eprintf "bundle %s is already installed, skipping" installed-name)
    (break))
  (def {:url url :type bundle-type :tag tag} bundle)
  (def bdir (pm/download-bundle url bundle-type tag))
  (def info (util/load-meta bdir))
  (when (nil? info)
    (errorf "bundle at %s does not include info.jdn file" url))
  (def info-name (get info :name))
  (def conflict? (bundle/installed? info-name))
  (when (and (not replace?) (not installed-name) conflict?)
    (def existing (install/bundle-name-to-bundle info-name))
    (eprintf "a conflicting bundle %v is already installed, skipping" info-name)
    (eprintf "  existing bundle: %.99M" existing)
    (eprintf "  skipped bundle:  %.99M" bundle)
    (break))
  (def deps (get info :dependencies []))
  (each d deps
    (install/install d :replace? replace? :force-update force-update :auto-remove true))
  (def config @{:pm bundle :installed-with "jeep" :auto-remove auto-remove})
  (unless no-install
    (if (and replace? conflict?)
      (bundle/replace info-name bdir :config config ;(kvs config))
      (bundle/install bdir :config config ;(kvs config)))))

(defn install/install-to
  [id dest &named force-update no-install auto-remove]
  (if (string? id)
    (error "id must be struct/table"))
  (util/mkdir dest)
  (def tmp (util/tmp-dir))
  (def oldpath (dyn *syspath*))
  (def syspath (util/change-syspath tmp))
  (def binpath (string syspath util/sep "bin"))
  (def manpath (string syspath util/sep "man"))
  (defn copy-dep [name]
    (def man (bundle/manifest name))
    (each f (get man :files)
      (unless (or (string/has-prefix? binpath f)
                  (string/has-prefix? manpath f))
        (def d (string dest util/sep (string/replace (dyn *syspath*) "" f)))
        (util/copy f d)))
    (each d (get man :dependencies)
      (copy-dep (get man :name))))
  (defer (do
           (util/change-syspath oldpath)
           (util/rmrf tmp))
    (install/install id :force-update force-update :no-install no-install :auto-remove auto-remove)
    (copy-dep (get id :name))))

(comment import ../util :prefix "")


(def- p/helps
  {:profile
   `The profile to use. Valid choices are 'system', 'build' and 'vendor'.`
   :force-deps
   `Force installation of dependencies.`
   :no-deps
   `Skip installation of dependencies.`
   :no-hook
   `Skip running the prep hook.`
   :about
   `Prepares the bundle for a given profile by installing dependencies and
   running the optional prep hook. For more information, see jeep-prep(1).`
   :help
   `Prepare dependencies for a given profile for the current bundle.`})

(def p/config
  {:rules [:profile       {:default "system"
                           :help (p/helps :profile)}
           "--force-deps" {:kind  :flag
                           :short "f"
                           :help (p/helps :force-deps)}
           "--no-deps"    {:kind :flag
                           :short "D"
                           :help (p/helps :no-deps)}
           "--no-hook"    {:kind :flag
                           :short "H"
                           :help (p/helps :no-hook)}
           "----"]
   :info {:about (get p/helps :about)}
   :help (get p/helps :help)})

(def- p/bundle-dir "bundle")
(def- p/this-file (os/realpath (dyn :current-file)))

(defn- p/vendor-deps-legacy
  [dirs-deps &named force-deps?]
  (def msg (string "warning: use of %ss with :vendored is deprecated, "
                   "refer to the man page for more information"))
  (printf msg (string (type dirs-deps)))
  (each [dir deps] (pairs dirs-deps)
    (each d deps
      (if (has-key? d :files)
        (util/fetch-dep d dir)
        (install/install-to d dir :force-update force-deps?)))))

(defn- p/vendor-deps
  [deps &named force-deps?]
  (each d deps
    (if (or (has-key? d :paths)
            (has-key? d :files))
      (util/fetch-dep d)
      (do
        (def dir (get d :prefix "."))
        (install/install-to d dir :force-update force-deps?)))))

(defn- p/install-build
  [info &named force-deps?]
  (def essentials
    ["build-rules.janet"
     "cc.janet"
     "cjanet.janet"
     "declare-cc.janet"
     "path.janet"
     "pm-config.janet"
     "sh.janet"
     "stream.janet"])
  (def spork-dir
    (string
      (if (string/has-prefix? (dyn :syspath) p/this-file)
        (string (dyn :syspath) util/sep "jeep")
        (string/slice p/this-file 0 -21))
      util/sep "deps" util/sep "spork"))
  (os/mkdir p/bundle-dir)
  (os/mkdir (string p/bundle-dir util/sep "spork"))
  (print "vendoring essential build files into " p/bundle-dir)
  (def from-licence (string spork-dir util/sep "LICENSE"))
  (def to-licence (string p/bundle-dir util/sep "spork" util/sep "LICENSE"))
  (print "  copying LICENSE to " p/bundle-dir util/sep "spork" util/sep "LICENSE")
  (util/copy from-licence to-licence)
  (each f essentials
    (def from (string spork-dir util/sep f))
    (def to (string p/bundle-dir util/sep "spork" util/sep f))
    (print "  copying " f " to " to)
    (util/copy from to)))

(defn- p/install-system
  [info &named force-deps?]
  (def system-deps (get info :dependencies []))
  (each d system-deps
    (install/install d :force-update force-deps?)))

(defn- p/install-vendor
  [info &named force-deps?]
  (def vendored (get info :vendored))
  (assert (and vendored (not (empty? vendored)))
          "no vendored dependencies in info.jdn")
  (def vendor-f (if (dictionary? vendored) p/vendor-deps-legacy p/vendor-deps))
  (vendor-f vendored :force-deps? force-deps?))

(defn p/run
  [args &opt jeep-config]
  (def info (util/load-meta "."))
  (def profile (get-in args [:sub :params :profile]))
  (def opts (get-in args [:sub :opts]))
  (def no-deps? (get opts "no-deps"))
  (def no-hook? (get opts "no-hook"))
  (def force-deps? (get opts "force-deps"))
  # install deps
  (unless no-deps?
    (case profile
      "system"
      (p/install-system info :force-deps? force-deps?)
      "build"
      (p/install-build info :force-deps? force-deps?)
      "vendor"
      (p/install-vendor info :force-deps? force-deps?)))
  # run hook
  (unless no-hook?
    (def man @{:info info})
    (try
      (util/local-hook :prep man profile)
      ([e f]
       (def rider "; use --no-hook to skip loading")
       (def msg (if (= "failed to load bundle script" e) (string e rider) e))
       (propagate msg f))))
  (print "Preparations completed."))


(def version "2026-04-04_10-31-29")

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

