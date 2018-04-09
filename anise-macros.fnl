;; anise-macros.fnl
;; Utility macros for Fennel.

;; Copyright 2018 bb010g <bb010g@gmail.com>
;; This code is licensed under either of the MIT (/LICENSE-MIT) or Apache v2.0
;; (/LICENSE-APACHE) licenses, at your option.

(local mac {})

; string macros

(defn mac.f-str [input ...]
  (when (~= (type input) :string)
    (error "f-str: must be called with a string"))
  (local cat (list (sym "..")))
  (local args [...])
  (var state :string)
  (var bytes [])
  (for [i 1 (# input)]
    (local b (string.byte input i))
    (if (= b 123) ; left curly
        (if (= state :string)
            (set state :expr-first)
            (= state :expr-first) ; escaped curly
            (do
              (set state :string)
              (table.insert bytes b))
            (= state :expr)
            (error "f-str: can't have curly braces in expressions")
            (= state :escape-right)
            (error "f-str: unmatched right curly must be followed by another")
            ; else (unreachable)
            (error "f-str: improper left curly brace"))
        (= b 125) ; right curly
        (if (= state :expr)
            (do
              (set state :string)
              (table.insert cat (sym (string.char (table.unpack bytes))))
              (set bytes []))
            (= state :string)
            (set state :escape-right)
            (= state :escape-right)
            (do
              (set state :string)
              (table.insert bytes b))
            (= state :expr-first)
            (do
              (assert (> (# args) 0) "f-str: Missing argument")
              (set state :string)
              (table.insert cat (string.char (table.unpack bytes)))
              (set bytes [])
              (table.insert cat (table.remove args 1)))
            ; else (unreachable)
            (error "f-str: improper right curly brace"))
        ; else
        (if (or (= state :string) (= state :expr))
            (table.insert bytes b)
            (= state :expr-first) ; starting expression
            (do
              (set state :expr)
              (table.insert cat (string.char (table.unpack bytes)))
              (set bytes [b]))
            (= state :escape-right)
            (error "f-str: unmatched right curly must be followed by another")
            ; else (unreachable)
            (error (.. "f-str: unknown state " state)))))
  (if (= state :string)
      (when (> (# input) 0)
        (table.insert cat (string.char (table.unpack bytes))))
      (or (= state :expr) (= state :expr-first))
      (error "f-str: unmatched right curly")
      (= state :escape-right)
      (error "f-str: unmatched left curly")
      ; else (unreachable)
      (error (.. "f-str: unknown state " state)))
  cat)

; nil-aware macros

(defn mac.set? [lhs rhs]
  (local check (list (sym :and)))
  (if (sym? lhs)
      (table.insert check (list (sym :=) (sym :nil) lhs))
      (list? lhs)
      (each [_ symb (ipairs lhs)]
        (assert (sym? symb) "set?: destructuring not supported")
        (table.insert check (list (sym :=) (sym :nil) symb)))
      ; else
      (error "set?: destructuring not supported"))
  (list (sym :when) check (list (sym :tset) lhs rhs)))

; not safe against side-effects, needs gensym or once
(defn mac.tset? [table key val]
  (list (sym :when) (list (sym :=) (sym :nil) (list (sym :.) table key))
    (list (sym :tset) table key val)))

; iterator comprehensions

(defn gen-iter-comp [i c]
  (fn [...]
    (local (c-wrap data body) (c.wrap [...]))
    (local (i-wrap body) (i body))
    (c-wrap (i-wrap (c.body data body)))))

(defn mk-iter-comp [mac iter i comp c]
  (local name (.. i :/ c))
  (tset mac name (gen-iter-comp (. iter i) (. comp c))))
(defn mk-iter-comps [mac iter is comp cs]
  (each [_ c (ipairs cs)]
    (each [_ i (ipairs is)]
      (mk-iter-comp mac iter i comp c))))

(local iter {
  :each (fn [body]
    (local [bindings & body] body)
    (defn wrap [...]
      (list (sym :each) bindings ...))
    (values wrap body))
  :for (fn [body]
    (local [bindings & body] body)
    (defn wrap [...]
      (list (sym :for) bindings ...))
    (values wrap body))
  :while (fn [body]
    (local [condition & body] body)
    (defn wrap [...]
      (list (sym :while) condition ...))
    (values wrap body))
})

; not completely safe, needs gensym
(local comp {})

(set comp.array {
  :wrap (fn [body]
    (local coll (sym :__comp_array_coll))
    (local i (sym :__comp_array_i))
    (defn wrap [...]
      (local body [...])
      (table.insert body (list (sym :values) coll i))
      (list (sym :let) [coll []]
        (list (sym :var) i 0)
        (table.unpack body)))
    (values wrap [coll i] body))
  :body (fn [data body]
    (local [coll i] data)
    (local body-len (# body))
    (tset body body-len
      (list (sym :tset) coll i (. body body-len)))
    (values
      (list (sym :set) i (list (sym :+) 1 i))
      (table.unpack body)))
})

(set comp.table {
  :wrap (fn [body]
    (local coll (sym :__comp_table_coll))
    (defn wrap [...]
      (local body [...])
      (table.insert body coll)
      (list (sym :let) [coll {}]
        (table.unpack body)))
    (values wrap coll body))
  :body (fn [coll body]
    (local body-len (# body))
    (local val (. body body-len))
    (assert (and (table? val) (= (# val) 2))
            "last body expression must be [k v]")
    (tset body body-len
      (list (sym :tset) coll (. val 1) (. val 2)))
    (table.unpack body))
})

(set comp.and {
  :wrap (fn [body]
    (local exp (sym :__comp_and_exp))
    (local tmp (sym :__comp_and_tmp))
    (defn wrap [...]
      (local body [...])
      (table.insert body exp)
      (list (sym :do)
        (list (sym :var) exp true)
        (table.unpack body)))
    (values wrap [exp tmp] body))
  :body (fn [data body]
    (local [exp tmp] data)
    (local (exp_str tmp_str) (values (. exp 1) (. tmp 1)))
    (local body-len (# body))
    (tset body body-len
      (list (sym :local) tmp (. body body-len)))
    (tset body (+ 1 body-len) (list (sym :luastatement)
      (.. "if not " tmp_str " then " exp_str " = " tmp_str "; break; end")))
    (table.unpack body))
})

(set comp.or {
  :wrap (fn [body]
    (local exp (sym :__comp_or_exp))
    (local tmp (sym :__comp_or_tmp))
    (defn wrap [...]
      (local body [...])
      (table.insert body exp)
      (list (sym :do)
        (list (sym :var) exp false)
        (table.unpack body)))
    (values wrap [exp tmp] body))
  :body (fn [data body]
    (local [exp tmp] data)
    (local (exp_str tmp_str) (values (. exp 1) (. tmp 1)))
    (local body-len (# body))
    (tset body body-len
      (list (sym :local) tmp (. body body-len)))
    (tset body (+ 1 body-len) (list (sym :luastatement)
      (.. "if " tmp_str " then " exp_str " = " tmp_str "; break; end")))
    (table.unpack body))
})

(set comp.sum {
  :wrap (fn [body]
    (local exp (sym :__comp_sum_exp))
    (defn wrap [...]
      (local body [...])
      (table.insert body exp)
      (list (sym :do)
        (list (sym :var) exp 0)
        (table.unpack body)))
    (values wrap exp body))
  :body (fn [exp body]
    (local body-len (# body))
    (tset body body-len
      (list (sym :set) exp (list (sym :+) exp (. body body-len))))
    (table.unpack body))
})

(set comp.product {
  :wrap (fn [body]
    (local exp (sym :__comp_product_exp))
    (defn wrap [...]
      (local body [...])
      (table.insert body exp)
      (list (sym :do)
        (list (sym :var) exp 1)
        (table.unpack body)))
    (values wrap exp body))
  :body (fn [exp body]
    (local body-len (# body))
    (tset body body-len
      (list (sym :set) exp (list (sym :*) exp (. body body-len))))
    (table.unpack body))
})

(set comp.first {
  :wrap (fn [body]
    (local res (sym :__comp_first_res))
    (defn wrap [...]
      (local body [...])
      (table.insert body res)
      (list (sym :do)
        (list (sym :var) res (sym :nil))
        (table.unpack body)))
    (values wrap res body))
  :body (fn [res body]
    (local res_str (. res 1))
    (local body-len (# body))
    (tset body body-len
      (list (sym :set) res (. body body-len)))
    (tset body (+ 1 body-len) (list (sym :luastatement) :break))
    (table.unpack body))
})

(set comp.last {
  :wrap (fn [body]
    (local res (sym :__comp_last_res))
    (defn wrap [...]
      (local body [...])
      (table.insert body res)
      (list (sym :do)
        (list (sym :var) res (sym :nil))
        (table.unpack body)))
    (values wrap res body))
  :body (fn [res body]
    (local res_str (. res 1))
    (local body-len (# body))
    (tset body body-len
      (list (sym :set) res (. body body-len)))
    (table.unpack body))
})

(mk-iter-comps mac
  iter [:each :for :while]
  comp [:array :table
        :and :or
        :sum :product
        :first :last])

; misc

(defn mac.and-or [test t f]
  (list (sym :or) (list (sym :and) test t) f))

(defn mac.require* [...]
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
        (and (list? spec))
        (let [ty (. spec 1)
              len (# spec)]
          (assert (sym? ty) "require*: spec type must be a symbol")
          (if ; rename, import as second arg
              (= (. ty 1) :rename)
              (do
              (assert (= (% len 2) 1) "require*: rename needs pairs of paths and names")
              (for [i 2 len 2]
                (let [path (. spec i)
                      name (. spec (+ 1 i))]
                  (assert (sym? path) "require*: rename's paths must be symbols")
                  (assert (sym? name) "require*: rename's names must be symbols")
                  (table.insert names name)
                  (table.insert requires (list (sym :require) (. path 1))))))
              ; unknown typed spec type
              (error "require*: unknown spec type")))
        ; unknown require spec
        (error "require*: unknown spec")))
  (list (sym :local) names requires))

mac
