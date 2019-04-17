;; anise/init.fnl
;; Utility functions for Fennel.

;; Copyright 2018 bb010g <bb010g@gmail.com>
;; This code is licensed under either of the MIT (/LICENSE-MIT) or Apache v2.0
;; (/LICENSE-APACHE) licenses, at your option.

(require-macros :anise.macros)
(require* anise.core)

(local anise {})
(core.merge_into anise core)

;; arrays

; (core.push arr ...)

; differs from table.pack by dropping nils
; (core.pack ...)

; drops nils in the tables being concatenated
; (core.pushcat arr ...)

; drops nils in the tables being concatenated
; (core.concat ...)

(define (anise_move a1 [:mut f] e [:mut t] [a2 a1])
  (local a2_len (# a2))
  (when (<= t (+ a2_len 1))
    (local a1_len (# a1))
    (local tmp [])
    (local tmp_len (math.min e a1_len))
    (for [i 1 tmp_len]
      (rawset tmp i (. a1 f))
      (set f (+ 1 f)))
    (for [i 1 tmp_len]
      (tset a2 i (rawget tmp t))
      (set t (+ 1 t))))
  a2)
(define anise.move (or table.move anise_move))

;; dictionary tables

; (core.clone t)

(define (anise.keys t)
  (each/array [k _ (pairs t)] k))

(define (anise.values t)
  (each/array [_ v (pairs t)] v))

(define (anise.dict_to_arr t)
  (each/array [k v (pairs t)] [k v]))

(define (anise.arr_to_dict assocs)
  (each/table [_ a (ipairs assocs)] [(. a 1) (. a 2)]))

(define (anise.dict_len t)
  (each/sum [_ _ (pairs t)] 1))

; (core.merge_into t ...)

; (core.merge ...)

;; iterators

(define (anise.collect_keys ...)
  (each/array [k _ ...] k))

(define (anise.collect_assocs ...)
  (each/array [k v ...] [k v]))

(define (anise.collect_table ...)
  (each/table [k v ...] [k v]))

(define (anise.collect_vals ...)
  (each/array [_ v ...] v))

(define (anise.filter_keys pred ...)
  (each/array [k _ ...]
    (if (not (pred k)) (luaexpr "continue"))
    k))

(define (anise.filter_assocs pred ...)
  (each/array [k v ...]
    (if (not (pred k v)) (luaexpr "continue"))
    [k v]))

(define (anise.filter_table pred ...)
  (each/table [k v ...]
    (if (not (pred k v)) (luaexpr "continue"))
    [k v]))

(define (anise.filter_vals pred ...)
  (each/array [_ v ...]
    (if (not (pred v)) (luaexpr "continue"))
    v))

;; math

(define (anise.clamp x min max)
  (math.max min (math.min x max)))

(define (anise.divmod x y)
  (local q (math.floor (/ x y)))
  (values q (- x (* y q))))

;; modules

; implementation based on lume.hotswap, licensed under MIT
; https://github.com/rxi/lume
(define (anise.hotswap modname)
  (local oldglobal (anise.clone _G))
  (local updated {})
  (define (update old new)
    (if (rawget updated old)
      (values)
      (let [oldmt (getmetatable old)
            newmt (getmetatable new)]
        (rawset updated old true)
        (when (and oldmt newmt)
          (update oldmt newmt))
        (each [k v (pairs new)]
          (if (= (type v) :table)
            (update (. old k) v)
            (tset old k v))))))
  (var err nil)
  (define (onerror e)
    (each [k (pairs _G)]
      (tset _G k (. oldglobal k)))
    (set err (anise.trim e)))
  (var (ok oldmod) (pcall require modname))
  (set oldmod (and-or ok oldmod nil))
  (xpcall
    (fn []
      (tset package.loaded modname nil)
      (local newmod (require modname))
      (when (= (type oldmod) :table)
        (update oldmod newmod))
      (each [k v (pairs oldglobal)]
        (when (and (~= v (. _G k)) (= (type v) :table))
          (update v (. _G k))
          (tset _G k v))))
    onerror)
  (tset package.loaded modname oldmod)
  (if err (values nil err) oldmod))

;; strings

(define (anise.gfind str pattern init plain)
  (define (iter s i)
    (local (start end) (string.find s pattern (+ i 1) plain))
    (values end start))
  (values iter str 0))

(define (anise.pretty_float x)
  (if (= (% x 1) 0)
    (tostring (math.floor x))
    (tostring x)))

(define (anise.split_str_iter s pat plain)
  (local pat (or pat (and-or plain "%s+" " ")))
  (var last_end 1)
  (define (iter s _)
    (local (start end) (string.find s pat last_end plain))
    (if (= last_end nil)
        nil
        (= end nil)
        (let [part_start last_end
              part_end (string.len s)]
          (set last_end nil)
          (values (string.sub s part_start part_end) part_start part_end))
        ; else
        (let [part_start last_end
              part_end (- start 1)]
          (set last_end (+ end 1))
          (values (string.sub s part_start part_end) part_start part_end))))
  (values iter s nil))

(define (anise.split_str s pat plain)
  (each/array [p (anise.split_str_iter s pat plain)] p))

(define (anise.trim s [pat "%s*"])
  (string.match s (f-str "^{pat}(.-){pat}$")))

(define (anise.trim_left s [pat "%s*"])
  (string.match s (f-str "^{pat}(.*)$")))

;; custom data structures

; data table

(let [dtm {}]
  (define (dtm.__index self key)
    (local data (rawget self :_data))
    (local val (. data key))
    (if data._parent
      (and val (setmetatable { :_data val :_key (rawget self :_key) }
                             (getmetatable self)))
      (and val (. val (rawget self :_key)))))

  (define (dtm.__newindex self key value)
    (local data (rawget self :_data))
    (if data._parent
      (error (f-str "Can't set non-terminal key {} in a data_table"
                    (tostring key)))
      (do
        (var t (. data key))
        (when (= t nil)
          (set t {})
          (tset data key t))
        (tset t (rawget self :_key) value)
        (values))))

  (define (dtm.__pairs self)
    (local data (rawget self :_data))
    (local iter
      (if data._parent
        (let [selfmeta (getmetatable self)]
          (fn [table index]
            (local (new_index val) (next table index))
            (if new_index
              (values new_index
                      (setmetatable { :_data val :_key (rawget self :_key) }
                                    selfmeta))
              nil)))
        (fn [table index]
          (local (new_index val) (next table index))
          (if new_index
            (values new_index (. val (rawget self :_key)))
            nil))))
    (values iter data nil))

  (define (dtm.__ipairs self)
    (local data (rawget self :_data))
    (local iter
      (if data._parent
        (let [selfmeta (getmetatable self)]
          (fn [table i]
            (local i (+ 1 i))
            (local val (. table i))
            (if val
              (values i (setmetatable { :_data val :_key (rawget self :_key) }
                                      selfmeta))
              nil)))
        (fn [table i]
          (local i (+ 1 i))
          (local val (. table i))
          (if val
            (values i (. val (rawget self :_key)))
            nil))))
    (values iter data 0))

  (set anise.data_table_meta dtm))
(define (anise.data_table data key)
  (setmetatable { :_data data :_key key } anise.data_table_meta))

; sets

; Fennel sets are wrappers around normal Lua tables. By default, this encoding
; is accomplished using `true` for values, giving you commutativity. If you
; wish, varying keys can be specified, but this weakens various properties
; (noted below). (If all sets involved in an operation have equal values, the
; sets are commutative, so using `true` isn't required but is easier.)
;
; `raw` variants are provided for all applicable methods that use `rawget` and
; `rawset` with the backing table.
;
; Sets with `union` and an identity of the empty set form an associative,
; unital magma (monoid) that's also commutative provided identical values.
;
; If you know all possible keys in your set, then the inverse of a set x is
; the complement of the set of all keys and x, and you now have a group
; (strengthened to an Abellian group provided identical values). Note that
; treating `complement` as subtraction of two sets works here with addition as
; `union`, i.e. x - y = x + (-y).
;
; Sets with `intersection` form an associative magma (semigroup) that's also
; commutative provided identical values.
;
; If you know all possible keys in your set, then the monoidal identity is the
; set of all keys.
;
; If you know all possible keys, then sets form a ring with addition as
; `union` and multiplication as `intersection` (strengthened to a commutative
; ring provided identical values).
;
; Handy definitions on nLab:
; - https://ncatlab.org/nlab/show/magma
; - https://ncatlab.org/nlab/show/semigroup
; - https://ncatlab.org/nlab/show/group

(let [sm {}]
  (define (sm.get self key)
    (. (rawget self :data) key))

  (define (sm.rawget self key)
    (rawget (rawget self :data) key))

  ; Adds a key to the set and returns the old value (default `true`).
  ; (Given a value, any old stored value will be overridden.)
  (define (sm.add self key [:value true])
    (local data (rawget self :data))
    (local old-value (. data key))
    (when (= old-value nil)
      (rawset self :count (+ (rawget self :count) 1)))
    (tset data k value)
    old-value)

  (define (sm.rawadd self key [:value true])
    (local data (rawget self :data))
    (local old-value (rawget data key))
    (when (= old-value nil)
      (rawset self :count (+ (rawget self :count) 1)))
    (rawset data k value)
    old-value)

  ; Removes a key from the set and returns the old value (default `true`).
  (define (sm.remove self key)
    (local data (rawget self :data))
    (local old-value (. data key))
    (when (~= old-value nil)
      (rawset self :count (- (rawget self :count) 1))
      (tset data key nil))
    old-value)

  (define (sm.rawremove self key)
    (local data (rawget self :data))
    (local old-value (rawget data key))
    (when (~= old-value nil)
      (rawset self :count (- (rawget self :count) 1))
      (rawset data key nil))
    old-value)

  (define (sm.empty? self)
    (= (rawget self :count) 0))
  (set sm.is_empty sm.empty?)

  (define (sm.__len self)
    (rawget self :count))

  (define (sm.__pairs self)
    (local data (rawget self :data))
    (local metamethod data.__pairs)
    (if (~= metamethod nil)
      (metamethod data)
      (values next data nil)))

  (define (sm.clone self)
    (setmetatable {
      :data (anise.clone (rawget self :data))
      :count (rawget self :count)
    } sm))

  (define (sm.union_into self ...)
    (local count (rawget self :count))
    (luaexpr "for a = 1, select('#', ...) do\
      local arg = select(a, ...)\
      if type(arg) == 'table' then\
        count = count + rawget(arg, 'count')\
        for k, v in pairs(arg) do\
          self[k] = v\
        end\
      end\
    end")
    (rawset self :count count)
    self)

  (define (sm.rawunion_into self ...)
    (local count (rawget self :count))
    (luaexpr "for a = 1, select('#', ...) do\
      local arg = select(a, ...)\
      if type(arg) == 'table' then\
        count = count + rawget(arg, 'count')\
        for k, v in pairs(arg) do\
          rawset(self, k, v)\
        end\
      end\
    end")
    (rawset self :count count)
    self)

  ; On differing values, the rightmost set's value for the key is used.
  (define (sm.union ...)
    (sm.rawunion_into (setmetatable { :data {} :count 0 } sm) ...))
  (set sm.__add sm.union)

  (define (sm.intersection_with self ...)
    (local count (rawget self :count))
    (local (prev curr) (values true false))
    (local keys (setmetatable (each/table [k _ (pairs self)] prev)
                              { :__mode :k }))
    (luaexpr "for a = 1, select('#', ...) do\
      local arg = select(a, ...)\
      if type(arg) == 'table' then\
        for k, v in pairs(arg) do\
          if rawget(keys, k) == prev then\
            rawset(keys, k, curr)\
            self[k] = v\
          end\
        end\
        for k, v in pairs(keys) then\
          if v == prev then\
            rawset(keys, k, nil)\
            self[k] = nil\
            count = count - 1\
          end\
        end\
        prev, curr = curr, prev\
      end\
    end")
    (rawset self :count count)
    self)

  (define (sm.rawintersection_with self ...)
    (local count (rawget self :count))
    (local (prev curr) (values true false))
    (local keys (setmetatable (each/table [k _ (pairs self)] prev)
                              { :__mode :k }))
    (luaexpr "for a = 1, select('#', ...) do\
      local arg = select(a, ...)\
      if type(arg) == 'table' then\
        for k, v in pairs(arg) do\
          if rawget(keys, k) == prev then\
            rawset(keys, k, curr)\
            rawset(self, k, v)\
          end\
        end\
        for k, v in pairs(keys) then\
          if v == prev then\
            rawset(keys, k, nil)\
            rawset(self, k, nil)\
            count = count - 1\
          end\
        end\
        prev, curr = curr, prev\
      end\
    end")
    (rawset self :count count)
    self)

  ; On differing values, the rightmost set's value for the key is used.
  (define (sm.intersection x ...)
    (sm.rawintersection_with (sm.clone x) ...))
  (set sm.__mul sm.intersection)

  (define (sm.complement_from self ...)
    (local count (rawget self :count))
    (luaexpr "for a = 1, select('#', ...) do\
      local arg = select(a, ...)\
      if type(arg) == 'table' then\
        for k, _ in pairs(arg) do\
          if self[k] ~= nil then\
            count = count - 1\
            self[k] = nil\
          end\
        end\
      end\
    end")
    (rawset self :count count)
    self)

  (define (sm.rawcomplement_from self ...)
    (local count (rawget self :count))
    (luaexpr "for a = 1, select('#', ...) do\
      local arg = select(a, ...)\
      if type(arg) == 'table' then\
        for k, _ in pairs(arg) do\
          if rawget(self, k) ~= nil then\
            self[k] = nil\
            count = count - 1\
          end\
        end\
      end\
    end")
    (rawset self :count count)
    self)

  (define (sm.complement x ...)
    (sm.rawcomplement_from (sm.clone x) ...))
  (define sm.__sub sm.complement)

  (set sm.__index sm)
  (set anise.set_meta sm))
(define (anise.set [:data {}])
  (setmetatable {
    :data data
    :count (anise.dict_len data)
  } anise.set_meta))

;; standard table library functions with return values

(define (anise.sort t f) (table.sort t f) t)
(define (anise.insert t a b) (table.insert t a b) t)
(define (anise.remove t i) (table.remove t i) t)

;; end

anise
