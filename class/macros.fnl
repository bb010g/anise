;; anise/class/macros.fnl
;; Class system macros for Fennel.

;; Copyright 2018 bb010g <bb010g@gmail.com>
;; This code is licensed under either of the MIT (/LICENSE-MIT) or Apache v2.0
;; (/LICENSE-APACHE) licenses, at your option.

(local mac {})
(local unpack (or unpack table.unpack))

(require-macros :anise.macros)
(require* anise)

(local anise-mac :__anise_class_module)

(define (mac.anise-class-mac-init)
  (` (local (!` (sym anise-mac)) (require :anise.class))))

(define (mac.class name ...)
  (local body [...])
  (unpack body))

mac
