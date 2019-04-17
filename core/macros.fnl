;; anise/core/macros.fnl
;; Utility macros for Fennel.

;; Copyright 2018 bb010g <bb010g@gmail.com>
;; This code is licensed under either of the MIT (/LICENSE-MIT) or Apache v2.0
;; (/LICENSE-APACHE) licenses, at your option.

(local mac {})
(local unpack (or unpack table.unpack))

(fn mac.and-or [test t f]
  (list (sym :or) (list (sym :and) test t) f))

(fn mac.require* [...]
  (local names (list))
  (local requires (list (sym :values)))
  (each [_ spec (pairs [...])]
    (if ; path, import as tail
        (sym? spec)
        (let [path (. spec 1)
              path-parts (or (multi-sym? path) [path])
              tail (. path-parts (# path-parts))]
          (table.insert names (sym tail))
          (table.insert requires (list (sym :require) path)))
        ; typed spec
        (list? spec)
        (let [ty (. spec 1)
              len (# spec)]
          (assert (sym? ty) "require*: spec type must be a symbol")
          (local ty (. ty 1))
          (if ; rename, import as second arg
              (= ty :rename)
              (do
              (assert (= (% len 2) 1)
                      "require*: rename needs pairs of paths and names")
              (for [i 2 len 2]
                (let [path (. spec i)
                      name (. spec (+ 1 i))]
                  (assert (sym? path)
                          "require*: rename's paths must be symbols")
                  (assert (sym? name)
                          "require*: rename's names must be symbols")
                  (table.insert names name)
                  (table.insert requires (list (sym :require) (. path 1))))))
              ; macros, require and import like require-macros
              (= ty :macros)
              (do
              (error "require*: macros not implemented yet"))
              ; unknown typed spec type
              (error "require*: unknown spec type")))
        ; unknown require spec
        (error "require*: unknown spec")))
  (list (sym :local) names requires))

(fn ` [datum]
  (if (list? datum)
      (let [head (. datum 1)
            head (and head (sym? head) (. head 1))]
        (if (= head :!`)
            [(. datum 2)]
            (= head :!@)
            (. datum 2)
            ; else
            (let [new-list (list (sym :anise.applycat) (sym :list))]
              (for [i 1 (# datum)]
                (table.insert new-list (` (. datum i))))
              [new-list])))
      (table? datum)
      (let [datum-len (# datum)]
        (if (> datum-len 0)
          (let [new-arr (list (sym :anise.concat))]
            (for [i 1 datum-len]
              (table.insert new-arr (` (. datum i))))
            [new-arr])
          (let [new-table {}]
            (each [k v (pairs datum)]
              (tset new-table k (` v)))
            [new-table])))
      (sym? datum)
      [(list (sym :sym) (. datum 1))]
      ; else
      [datum]))
 
(fn mac.` [datum] (. (` datum) 1))

mac
