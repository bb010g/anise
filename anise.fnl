;; anise.fnl
;; Utility functions for Fennel.

;; Copyright 2018 bb010g <bb010g@gmail.com>
;; This code is licensed under either of the MIT (/LICENSE-MIT) or Apache v2.0
;; (/LICENSE-APACHE) licenses, at your option.

(require-macros :anise-macros)

(local anise {})

;; arrays

(defn anise.push [arr ...]
  (var i (# arr))
  (for [a 1 (select :# ...)]
    (local arg (select a ...))
    ;(when (~= arg nil) (set i (+ 1 i)) (tset arr i arg)))
    (luastatement "if arg ~= nil then i = 1 + i; arr[i] = arg end"))
  (values arr i))

; differs from table.pack by dropping nils
(defn anise.pack [...]
  (anise.push [] ...))

(set anise.pushcat (luaexpr "function (arr, ...)\
  local i = #arr\
  for a = 1, select('#', ...) do\
    local arg = select(a, ...)\
    if type(arg) == 'table' then\
      for j = 1, #arg do\
        i = 1 + i\
        arr[i] = arg[j]\
      end\
    end\
  end\
  return arr, i\
end"))

(defn anise.concat [...]
  (anise.pushcat [] ...))

;; dictionary tables

(defn anise.keys [t]
  (each/array [k _ (pairs t)] k))

(defn anise.values [t]
  (each/array [_ v (pairs t)] v))

(defn anise.dict_to_arr [t]
  (each/array [k v (pairs t)] [k v]))

(defn anise.arr_to_dict [assocs]
  (each/table [_ a (ipairs assocs)] [(. a 1) (. a 2)]))

(defn anise.dict_len [t]
  (each/sum [_ _ (pairs t)] 1))

;; math

(defn anise.divmod [x y]
  (local q (math.floor (/ x y)))
  (values q (- x (* y q))))

;; strings

(defn anise.pretty_float [x]
  (if (= (% x 1) 0)
    (tostring (math.floor x))
    (tostring x)))

(defn anise.gfind [str pattern init plain]
  (defn iter [s i]
    (local (start end) (string.find s pattern i plain))
    (values end start))
  (values iter str 1))

(defn anise.split_str [s pat plain]
  (local pat (or pat (and-or plain "%s+" " ")))
  (var last_end 1)
  (local (arr i)
    (each/array [end start (anise.gfind s pat plain)]
      (set last_end end)
      (string.sub s last_end start)))
  (tset arr (+ 1 i) (string.sub last_end -1))
  arr)

; (defn anise.trim [s pat plain]
  ; nil)

;; custom data structures

; data table

(let [dtm {}]
  (defn dtm.__index [self key]
    (local data (rawget self :_data))
    (local val (. data key))
    (if data._parent
      (and val (setmetatable { :_data val :_key (rawget self :_key) } (getmetatable self)))
      (and val (. val (rawget self :_key)))))

  (defn dtm.__newindex [self key value]
    (local data (rawget self :_data))
    (if data._parent
      (error (f-str "Can't set non-terminal key {} in a data_table" (tostring key)))
      (do
        (var t (. data key))
        (when (= t nil)
          (set t {})
          (tset data key t))
        (tset t (rawget self :_key) value)
        (values))))

  (defn dtm.__pairs [self]
    (local data (rawget self :_data))
    (local iter
      (if data._parent
        (let [selfmeta (getmetatable self)]
          (fn [table index]
            (local (new_index val) (next table index))
            (if new_index
              (values new_index (setmetatable { :_data val :_key (rawget self :_key) } selfmeta))
              nil)))
        (fn [table index]
          (local (new_index val) (next table index))
          (if new_index
            (values new_index (. val (rawget self :_key)))
            nil))))
    (values iter data nil))

  (defn dtm.__ipairs [self]
    (local data (rawget self :_data))
    (local iter
      (if data._parent
        (let [selfmeta (getmetatable self)]
          (fn [table i]
            (local i (+ 1 i))
            (local val (. table i))
            (if val
              (values i (setmetatable { :_data val :_key (rawget self :_key) } selfmeta))
              nil)))
        (fn [table i]
          (local i (+ 1 i))
          (local val (. table i))
          (if val
            (values i (. val (rawget self :_key)))
            nil))))
    (values iter data 0))

  (set anise.data_table_meta dtm))
(defn anise.data_table [data key]
  (setmetatable { :_data data :_key key } anise.data_table_meta))

; sets

(let [sm {}]
  (defn sm.ref [self k]
    (. (rawget self :data) k))

  (defn sm.add [self k]
    (local data (rawget self :data))
    (when (not (. data k))
      (rawset self :count (+ (rawget self :count) 1))
      (tset data k true)))

  (defn sm.remove [self k]
    (local data (rawget self :data))
    (when (. data k)
      (rawset self :count (- (rawget self :count) 1))
      (tset data k nil)))

  (defn sm.empty [self]
    (= (rawget self :count) 0))

  (defn sm.__len [self]
    (rawget self :count))

  (defn sm.__pairs [self]
    (local data (rawget self :data))
    (local metamethod data.__pairs)
    (if metamethod
      (metamethod data)
      (values next data nil)))

  (set sm.__index sm)
  (set anise.set_meta sm))
(defn anise.set [data]
  (setmetatable {
    :data (or data {})
    :count (and-or data (anise.dict_len data) 0)
  } anise.set_meta))

;; end
 
anise
