(module
  (memory (export "mem") 1)

  ;;
  ;; Reverse a string
  ;;
  ;; @param {i32} offset - The offset of the input string in linear memory
  ;; @param {i32} length - The length of the input string in linear memory
  ;;
  ;; @returns {(i32,i32)} - The offset and length of the reversed string in linear memory
  ;;
  (func (export "reverseString") (param $offset i32) (param $length i32) (result i32 i32)
        (local $fwdPtr i32)
        (local $backPtr i32)
        (local.set $fwdPtr (local.get $offset))
        (local.set $backPtr
                   (i32.sub (i32.add (local.get $offset) (local.get $length))
                            (i32.const 1)))        
        (loop $loop
         ;; swap
         (local.get $backPtr)
         (i32.load8_u (local.get $fwdPtr))
         (local.get $fwdPtr)
         (i32.load8_u (local.get $backPtr))
         (i32.store8)
         (i32.store8)
         ;; advance
         (local.tee $fwdPtr (i32.add (local.get $fwdPtr) (i32.const 1)))
         (local.tee $backPtr (i32.sub (local.get $backPtr) (i32.const 1)))
         (br_if $loop (i32.le_u)))
        (return (local.get $offset) (local.get $length))
  )
)
