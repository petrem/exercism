(module
  (func (export "eggCount") (param $number i32) (result i32)
    (i32.popcnt (local.get $number)))
)
