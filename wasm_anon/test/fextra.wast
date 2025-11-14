(module
  (func (export "trunc") (param $x f64) (result f64) (f64.trunc (local.get $x)))
  (func (export "nearest") (param $x f64) (result f64) (f64.nearest (local.get $x)))
)

(assert_return (invoke "nearest" (f64.const 0x3p-2)) (f64.const 0x1p+0))
(assert_return (invoke "trunc" (f64.const 0x3p-2)) (f64.const 0x0p+0))
