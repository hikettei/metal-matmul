(asdf:defsystem "metal-matmul"
  :description "A Playground for benchmarking High Performance FP32 Matmul on Metal"
  :author      "hikettei <ichndm@gmail.com>"
  :license     "MIT"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi" "cl-pack" "flexi-streams" "float-features" "babel")
  :serial t
  :pathname "source"
  :components ((:cffi-wrapper-file "helpers/callback" :soname "callback_helper")
               (:file "libmetal")))
