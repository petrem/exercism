(module
  (memory (export "mem") 1)
  (data (i32.const 512) "\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\41\41\41\41\41\41\41\41\41\41\41\41\41\41\41\41\41\41\41\41\41\41\41\41\41\41\00\00\00\00\00\00\61\61\61\61\61\61\61\61\61\61\61\61\61\61\61\61\61\61\61\61\61\61\61\61\61\61\00\00\00\00\00")

  (func (export "rotate")
        (param $offset i32) (param $length i32) (param $key i32)
        (result i32 i32)

        (local $ptr i32) (local $last i32) (local $char i32) (local $code i32)
        
        (local.tee $ptr (local.get $offset))
        (local.set $last (i32.add (local.get $length)))

        (block $end
          (loop $loop
           (br_if $end (i32.eq (local.get $ptr) (local.get $last)))
           (block $next
             (local.tee $char (i32.load8_u (local.get $ptr)))
             ;; check if ascii (0-127)
             (br_if $next (i32.gt_u (i32.const 127)))
             ;; is it alpha, and if so, get lower/upper offset (called "$code" here)
             (local.tee $code (i32.load8_u (i32.add (i32.const 512) (local.get $char))))
             (br_if $next (i32.eqz))
             (local.get $ptr) ;; address for store
             (i32.sub (local.get $char) (local.get $code))
             (i32.add (local.get $key))
             (i32.rem_u (i32.const 26))
             (i32.add (local.get $code))
             (i32.store8))
           (local.set $ptr (i32.add (local.get $ptr) (i32.const 1)))
           (br $loop)))
        (return (local.get $offset) (local.get $length)))
)
