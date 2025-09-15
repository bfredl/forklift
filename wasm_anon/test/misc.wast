;; basic tests

(module
  (func $modadder (import "spectest" "modadder") (param i32) (param i32) (result i32))
  (func (export "add") (param $x i32) (param $y i32) (result i32) (i32.add (local.get $x) (local.get $y)))

  (func (export "elser") (param $x i32) (param $y i32) (result i32)
    (local $z i32)
    local.get $x
    if
      local.get $y
      local.set $z
    else
      i32.const 7
      local.set $z
    end
    local.get $z
  )

  (func (export "caller") (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    call $modadder
   )
)

(assert_return (invoke "add" (i32.const 1) (i32.const 1)) (i32.const 2))
(assert_return (invoke "elser" (i32.const 1) (i32.const 10)) (i32.const 10))
(assert_return (invoke "elser" (i32.const 0) (i32.const 10)) (i32.const 7))
(assert_return (invoke "caller" (i32.const 10) (i32.const 3)) (i32.const 4))
