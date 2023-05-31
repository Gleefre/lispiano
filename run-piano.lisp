(asdf:load-asd (merge-pathnames "lispiano.asd" (uiop:getcwd)))

(ql:quickload :lispiano)

(lispiano:start-toplevel)

(quit)
