;; anise/core/init.fnl
;; Utility functions for Fennel.

;; Copyright 2018 bb010g <bb010g@gmail.com>
;; This code is licensed under either of the MIT (/LICENSE-MIT) or Apache v2.0
;; (/LICENSE-APACHE) licenses, at your option.

(require-macros :anise.core.macros)

(local core {})
(local unpack (or unpack table.unpack))

;; arrays

(fn core.push [arr ...]
  (var i (# arr))
  (for [a 1 (select :# ...)]
    (local arg (select a ...))
    ;(when (~= arg nil) (set i (+ 1 i)) (tset arr i arg)))
    (luastatement "if arg ~= nil then i = 1 + i; arr[i] = arg end"))
  (values arr i))

; differs from table.pack by dropping nils
(fn core.pack [...]
  (core.push [] ...))

; drops nils in the tables being concatenated
(set core.pushcat (luaexpr "function (arr, ...)\
  local i = #arr\
  for a = 1, select('#', ...) do\
    local arg = select(a, ...)\
    if type(arg) == 'table' then\
      for j = 1, #arg do\
        local v = arg[j]\
        if v ~= nil then i = 1 + i; arr[i] = v end\
      end\
    end\
  end\
  return arr, i\
end"))

(set core.map (luaexpr "function (arr, f)\
  local out = {}\
  for i = 0, #arr do\
    out[i] = f(arr[i])\
  end\
  return out\
end"))

; drops nils in the tables being concatenated
(fn core.concat [...]
  (core.pushcat [] ...))

(fn core.applycat [f ...]
  (f (unpack (luaexpr "(core.concat(...))"))))

;; dictionary tables

(fn core.clone [t]
  (local u {})
  (each [k v (pairs t)] (tset u k v))
  u)

(set core.merge_into (luaexpr "function (t, ...)\
  for a = 1, select('#', ...) do\
    local arg = select(a, ...)\
    if type(arg) == 'table' then\
      for k, v in pairs(arg) do\
        t[k] = v\
      end\
    end\
  end\
  return t\
end"))

(fn core.merge [...]
  (core.merge_into {} ...))

core
