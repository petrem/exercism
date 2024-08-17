(module
  ;;(import "console" "log" (func $log (param i32)))
  (memory (export "mem") 1)

  ;; Result strings
  (data (i32.const 0) "PlingPlangPlongPlingPlong")

  ;; Result pair offsets:
  ;; 0x00 - No conversion
  ;; 0x02 - Pling 0 5 - 3
  ;; 0x04 - PlingPlang 0 10 - 3 * 5
  ;; 0x06 - PlingPlangPlong 0 15 - 3 * 5 * 7
  ;; 0x08 - Plang 5 5 - 5
  ;; 0x0a - PlangPlong 5 10 - 5 * 7
  ;; 0x0c - Plong 10 5 - 7
  ;; 0x0e - PlingPlong 15 10 - 3 * 7
  (data (i32.const 25) "\ff\ff\00\05\00\0a\00\0f\05\05\05\0a\0a\05\0f\0a")

  ;; All results up to 3*5*7 = 105
  ;; generated with ```print("".join(next((code for k, code in [(3*5*7, r"\06"), (3*5, r"\04"), (3*7, r"\0e"), (5*7, r"\0a"), (3, r"\02"), (5, r"\08"), (7, r"\0c")] if x % k == 0), r"\00") for x in range(0, 106))```
  (data (i32.const 41) "\06\00\00\02\00\08\02\0c\00\02\08\00\02\00\0c\04\00\00\02\00\08\0e\00\00\02\08\00\02\0c\00\04\00\00\02\00\0a\02\00\00\02\08\00\0e\00\00\04\00\00\02\0c\08\02\00\00\02\08\0c\02\00\00\04\00\00\0e\00\08\02\00\00\02\0a\00\02\00\00\04\00\0c\02\00\08\02\00\00\0e\08\00\02\00\00\04\0c\00\02\00\08\02\00\0c\02\08\00\02\00\00\06")
  ;; next free offset: 147

  
  ;;
  ;; Reverse a string
  ;;
  ;; @param {i32} offset - The offset of the input string in linear memory
  ;; @param {i32} length - The length of the input string in linear memory
  ;;
  (func $reverseString (param $offset i32) (param $length i32)
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
  )


  ;; Convert a number int a string
  ;;
  ;; @param {i32} input - The number to convert
  ;;
  ;; @returns {i32} - Length of the conversion (the offset is fixed)
  (func $numberToString (param $input i32) (result i32)
        (local $pos i32)
        (local $cur i32)
        (local $len i32)

        (local.set $pos (i32.const 147))
        (local.set $cur (local.get $input))

        (loop $loop
         (i32.store8 (local.get $pos)
                     (i32.add (i32.const 48) ;; ord(0)
                              (i32.rem_u (local.get $cur) (i32.const 10))))
         (local.set $pos (i32.add (local.get $pos) (i32.const 1)))
         (local.tee $cur (i32.div_u (local.get $cur) (i32.const 10)))
         (br_if $loop (i32.ne (i32.const 0))))
        i32.const 147
        (local.tee $len (i32.sub (local.get $pos) (i32.const 147)))
        call $reverseString
        local.get $len
        return
  )

  ;;
  ;; Convert a number into a string of raindrop sounds
  ;;
  ;; @param {i32} input - The number to convert
  ;;
  ;; @returns {(i32,i32)} - Offset and length of raindrop sounds string
  ;;                        in linear memory.
  ;;
  (func (export "convert") (param $input i32) (result i32 i32)
        (local $resultPairIndex i32)
        (local $resultOffset i32)
        (local $resultLen i32)

        (local.tee $resultPairIndex
                   (i32.load8_u (i32.add (i32.const 41)
                                         (i32.rem_u (local.get $input) (i32.const 105)))))
        (if (then ;; can be converted
             (local.set $resultOffset
                        (i32.load8_u (i32.add (i32.const 25) (local.get $resultPairIndex))))
             (local.set $resultLen
                        (i32.load8_u (i32.add (i32.const 26) (local.get $resultPairIndex)))))
            (else
             (local.set $resultOffset (i32.const 147))
             (local.set $resultLen
                        (call $numberToString (local.get $input)))))
        (return (local.get $resultOffset) (local.get $resultLen))
  )
)
