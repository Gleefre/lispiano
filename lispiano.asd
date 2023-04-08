(defsystem "lispiano"
  :description "Piano for your keyboard"
  :version "1.0.0"
  :author "Grolter <varedif.a.s@gmail.com>"
  :license "Apache 2.0"
  :depends-on ("sketch" "sdl2-mixer" "sketch-fit")
  :components ((:file "piano")))
