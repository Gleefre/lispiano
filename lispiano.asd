(defsystem "lispiano"
  :description "Piano for your keyboard"
  :version "1.0.0"
  :author "Grolter <varedif.a.s@gmail.com>"
  :license "Apache 2.0"
  :depends-on ("sketch" "sketch-fit" "harmony" "cl-mixed-wav")
  :components ((:file "piano")))
