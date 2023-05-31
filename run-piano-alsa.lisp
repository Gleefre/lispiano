(asdf:load-asd (merge-pathnames "lispiano.asd" (uiop:getcwd)))

(ql:quickload :lispiano)

(ql:quickload :cl-mixed-alsa)

(org.shirakumo.fraf.harmony:maybe-start-simple-server :drain 'org.shirakumo.fraf.mixed.alsa:drain)

(lispiano:start-toplevel)

(quit)
