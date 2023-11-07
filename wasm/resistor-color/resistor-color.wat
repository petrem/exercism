(module
  (memory (export "mem") 1)
  (data (i32.const 190) "black,brown,red,orange,yellow,green,blue,violet,grey,white")
  (global $blackCode i32 (i32.const 0))
  (global $brownCode i32 (i32.const 1))
  (global $redCode i32 (i32.const 2))
  (global $orangeCode i32 (i32.const 3))
  (global $yellowCode i32 (i32.const 4))
  (global $greenCode i32 (i32.const 5))
  (global $blueCode i32 (i32.const 6))
  (global $violetCode i32 (i32.const 7))
  (global $greyCode i32 (i32.const 8))
  (global $whiteCode i32 (i32.const 9))

  (func (export "colors") (result i32 i32)
    (return (i32.const 190) (i32.const 58)))

  (func $matchLetterAt (param $base i32) (param $index i32) (param $match i32) (result i32)
        (i32.eq (i32.load8_u (i32.add (local.get $base) (local.get $index)))
                (local.get $match)))

  ;; Given a valid resistor color, returns the associated value
  ;; Note! this is not actually checking full color names (and does not use $len)
  (func (export "colorCode") (param $offset i32) (param $len i32) (result i32)
        ;; 'b'
        (if (call $matchLetterAt (local.get $offset) (i32.const 0) (i32.const 98))
            (then
             ;; 'bl'
             (if (call $matchLetterAt (local.get $offset) (i32.const 1) (i32.const 108))
                 (then
                  ;; 'bla'
                  (if (call $matchLetterAt (local.get $offset) (i32.const 2) (i32.const 97))
                      (then (return (global.get $blackCode)))
                    ;; must be 'blu'
                    (else (return (global.get $blueCode)))))
               ;; must be 'br'
               (else (return (global.get $brownCode))))))
        ;; 'g'
        (if (call $matchLetterAt (local.get $offset) (i32.const 0) (i32.const 103))
            (then
             ;; 'gree'
             (if (call $matchLetterAt (local.get $offset) (i32.const 3) (i32.const 101))
                 (then (return (global.get $greenCode)))
               ;; must be 'grey'
               (else (return (global.get $greyCode))))))
        ;; 'o'
        (if (call $matchLetterAt (local.get $offset) (i32.const 0) (i32.const 111))
            (then (return (global.get $orangeCode))))
        ;; 'r'
        (if (call $matchLetterAt (local.get $offset) (i32.const 0) (i32.const 114))
            (then (return (global.get $redCode))))
        ;; 'v'
        (if (call $matchLetterAt (local.get $offset) (i32.const 0) (i32.const 118))
            (then (return (global.get $violetCode))))
        ;; 'w'
        (if (call $matchLetterAt (local.get $offset) (i32.const 0) (i32.const 119))
            (then (return (global.get $whiteCode))))
        ;; 'y'
        (if (call $matchLetterAt (local.get $offset) (i32.const 0) (i32.const 121))
            (then (return (global.get $yellowCode))))
        (return (i32.const -1)))
)
