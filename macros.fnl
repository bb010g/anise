;; anise/macros.fnl
;; Utility macros for Fennel.

;; Copyright 2018 bb010g <bb010g@gmail.com>
;; This code is licensed under either of the MIT (/LICENSE-MIT) or Apache v2.0
;; (/LICENSE-APACHE) licenses, at your option.

(require-macros :anise.core.macros)
(require* (rename anise.core anise))

(local mac {})
(local unpack (or unpack table.unpack))

(anise.merge_into mac (. fennel.macroLoaded :anise.core.macros))

; string macros

(fn mac.f-str [input ...]
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
              (table.insert cat (sym (string.char (unpack bytes))))
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
              (table.insert cat (string.char (unpack bytes)))
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
              (table.insert cat (string.char (unpack bytes)))
              (set bytes [b]))
            (= state :escape-right)
            (error "f-str: unmatched right curly must be followed by another")
            ; else (unreachable)
            (error (.. "f-str: unknown state " state)))))
  (if (= state :string)
      (when (> (# input) 0)
        (table.insert cat (string.char (unpack bytes))))
      (or (= state :expr) (= state :expr-first))
      (error "f-str: unmatched right curly")
      (= state :escape-right)
      (error "f-str: unmatched left curly")
      ; else (unreachable)
      (error (.. "f-str: unknown state " state)))
  cat)

; nil-aware macros

(fn mac.set? [lhs rhs]
  (local checks [])
  (if (sym? lhs)
      (table.insert checks (` (= nil (!` lhs))))
      (list? lhs)
      (each [_ symb (ipairs lhs)]
        (assert (sym? symb) "set?: destructuring not supported")
        (table.insert checks (` (= nil (!` symb)))))
      ; else
      (error "set?: destructuring not supported"))
  (` (when (and (!@ checks))
    (set (!` lhs) (!` rhs)))))

; not safe against side-effects, needs gensym or once
(fn mac.tset? [table key val]
  (` (when (= nil (. (!` table) (!` key)))
    (tset (!` table) (!` key) (!` val)))))

; iterator comprehensions

(fn gen-iter-comp [i c]
  (fn [...]
    (local (c-wrap data body) (c.wrap [...]))
    (local (i-wrap body) (i body))
    (c-wrap (i-wrap (c.body data body)))))

(fn mk-iter-comp [mac iter i comp c]
  (local name (.. i :/ c))
  (tset mac name (gen-iter-comp (. iter i) (. comp c))))
(fn mk-iter-comps [mac iter is comp cs]
  (each [_ c (ipairs cs)]
    (each [_ i (ipairs is)]
      (mk-iter-comp mac iter i comp c))))

(local iter {
  :each (fn [body]
    (local [bindings & body] body)
    (fn wrap [...]
      (list (sym :each) bindings ...))
    (values wrap body))
  :for (fn [body]
    (local [bindings & body] body)
    (fn wrap [...]
      (list (sym :for) bindings ...))
    (values wrap body))
  :while (fn [body]
    (local [condition & body] body)
    (fn wrap [...]
      (list (sym :while) condition ...))
    (values wrap body))
})

; not completely safe, needs gensym
(local comp {})

(set comp.array {
  :wrap (fn [body]
    (local coll (sym :__comp_array_coll))
    (local i (sym :__comp_array_i))
    (fn wrap [...]
      (local body [...])
      (` (let [(!` coll) []]
        (var (!` i) 0)
        (!@ body)
        (values (!` coll) (!` i)))))
    (values wrap [coll i] body))
  :body (fn [data body]
    (local [coll i] data)
    (local body-len (# body))
    (tset body body-len
      (list (sym :tset) coll i (. body body-len)))
    (values
      (` (set (!` i) (+ 1 (!` i))))
      (unpack body)))
})

(set comp.table {
  :wrap (fn [body]
    (local coll (sym :__comp_table_coll))
    (fn wrap [...]
      (local body [...])
      (` (let [(!` coll) {}]
        (!@ body)
        (!` coll))))
    (values wrap coll body))
  :body (fn [coll body]
    (local body-len (# body))
    (local val (. body body-len))
    (assert (and (table? val) (= (# val) 2))
            "last body expression must be [k v]")
    (tset body body-len
      (list (sym :tset) coll (. val 1) (. val 2)))
    (unpack body))
})

(set comp.and {
  :wrap (fn [body]
    (local exp (sym :__comp_and_exp))
    (local tmp (sym :__comp_and_tmp))
    (fn wrap [...]
      (local body [...])
      (` (do
        (var (!` exp) true)
        (!@ body)
        (!` exp))))
    (values wrap [exp tmp] body))
  :body (fn [data body]
    (local [exp tmp] data)
    (local (exp_str tmp_str) (values (. exp 1) (. tmp 1)))
    (local body-len (# body))
    (tset body body-len
      (list (sym :local) tmp (. body body-len)))
    (tset body (+ 1 body-len) (list (sym :luastatement)
      (.. "if not " tmp_str " then " exp_str " = " tmp_str "; break; end")))
    (unpack body))
})

(set comp.or {
  :wrap (fn [body]
    (local exp (sym :__comp_or_exp))
    (local tmp (sym :__comp_or_tmp))
    (fn wrap [...]
      (local body [...])
      (` (do
        (var (!` exp) false)
        (!@ body)
        (!` exp))))
    (values wrap [exp tmp] body))
  :body (fn [data body]
    (local [exp tmp] data)
    (local (exp_str tmp_str) (values (. exp 1) (. tmp 1)))
    (local body-len (# body))
    (tset body body-len
      (list (sym :local) tmp (. body body-len)))
    (tset body (+ 1 body-len) (list (sym :luastatement)
      (.. "if " tmp_str " then " exp_str " = " tmp_str "; break; end")))
    (unpack body))
})

(set comp.sum {
  :wrap (fn [body]
    (local exp (sym :__comp_sum_exp))
    (fn wrap [...]
      (local body [...])
      (` (do
        (var (!` exp) 0)
        (!@ body)
        (!` exp))))
    (values wrap exp body))
  :body (fn [exp body]
    (local body-len (# body))
    (tset body body-len
      (list (sym :set) exp (list (sym :+) exp (. body body-len))))
    (unpack body))
})

(set comp.product {
  :wrap (fn [body]
    (local exp (sym :__comp_product_exp))
    (fn wrap [...]
      (local body [...])
      (` (do
        (var (!` exp) 1)
        (!@ body)
        (!` exp))))
    (values wrap exp body))
  :body (fn [exp body]
    (local body-len (# body))
    (tset body body-len
      (list (sym :set) exp (list (sym :*) exp (. body body-len))))
    (unpack body))
})

(set comp.first {
  :wrap (fn [body]
    (local res (sym :__comp_first_res))
    (fn wrap [...]
      (local body [...])
      (` (do
        (var (!` res) nil)
        (!@ body)
        (!` res))))
    (values wrap res body))
  :body (fn [res body]
    (local res_str (. res 1))
    (local body-len (# body))
    (tset body body-len
      (list (sym :set) res (. body body-len)))
    (tset body (+ 1 body-len) (list (sym :luastatement) :break))
    (unpack body))
})

(set comp.last {
  :wrap (fn [body]
    (local res (sym :__comp_last_res))
    (fn wrap [...]
      (local body [...])
      (` (do
        (var (!` res) nil)
        (!@ body)
        (!` res))))
    (values wrap res body))
  :body (fn [res body]
    (local res_str (. res 1))
    (local body-len (# body))
    (tset body body-len
      (list (sym :set) res (. body body-len)))
    (unpack body))
})

(mk-iter-comps mac
  iter [:each :for :while]
  comp [:array :table
        :and :or
        :sum :product
        :first :last])

; match

;(fn match-pat

;(fn mac.match [val-expr ...]
  ;(local clauses [...])


; functions

(fn define-args [args ...]
  (local body [...])
  (var optionals false)
  (fn set-default [arg]
    (local [a val] arg)
    (list (sym :set-forcably!) a (list (sym :or) a val)))
  (each [i arg (ipairs args)]
    (if optionals
      (do
        (assert (table? arg) "define: no required arguments after optional")
        (set optionals (+ 1 optionals))
        (tset args i (. arg 1))
        (table.insert body optionals (set-default arg)))
      (when (table? arg)
        (set optionals 1)
        (tset args i (. arg 1))
        (table.insert body optionals (set-default arg)))))
  (values args (unpack body)))

(fn define-fn [head body]
  (if ; done
      (sym? head)
      (values head (fn [...]
        (local b (body ...))
        (table.insert b 2 head)
        b))
      ; lambda
      (list? head)
      (let [[new-head & args] head]
        (define-fn
          new-head
          (fn [...] (body (list (sym :fn) (define-args args ...))))))))

(fn mac.define [id ...]
  (if ; variable
      (sym? id)
      (list (sym (or (and (multi-sym? (. id 1)) :set) :local)) id ...)
      ; function
      (list? id)
      (let [(_head body) (define-fn id (fn [...] ...))]
        (body ...))
      ; other
      (error "define: id must be symbol or list")))

; misc

; core.macros.and-or

; core.macros.require*

mac
