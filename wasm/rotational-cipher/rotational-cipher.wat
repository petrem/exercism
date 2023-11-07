(module
  (memory (export "mem") 1)
  ;; input buffer at 64-319

  (func $rotChar (param $char i32) (param $key i32)
        (i32.sub (local.get $char)
        

  (func (export "rotate")
        (param $offset i32) (param $length i32) (param $key i32)
        (result i32 i32)

        (local $ptr i32)
        (local $last i32)
        (local.tee $ptr (local.get $offset))
        (local.set $last (i32.add $length))

        (block $end
               (loop $loop
                     (br_if $end (i32.eq (local.get $ptr) (local.get $last)))
                     
                     (i32.load8_u (local.get $ptr))
                     
                     
                     (local.set $ptr (i32.add (local.get $ptr) 4))
                     (br $loop)))
        (return (local.get $offset) (local.get $length)))
)
