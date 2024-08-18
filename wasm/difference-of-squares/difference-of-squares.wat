(module
  ;;
  ;; Calculate the square of the sum of the first N natural numbers
  ;;
  ;; @param {i32} max - The upper bound (inclusive) of natural numbers to consider
  ;;
  ;; @returns {i32} The square of the sum of the first N natural numbers
  ;;
  (func $squareOfSum (export "squareOfSum") (param $max i32) (result i32)
    (local $top i32)
    
    local.get $max
    local.get $max
    i32.mul
    local.get $max
    i32.add
    i32.const 2
    i32.div_u
    local.tee $top
    local.get $top
    i32.mul
    return
  )

  ;;
  ;; Calculate the sum of the squares of the first N natural numbers
  ;;
  ;; @param {i32} max - The upper bound (inclusive) of natural numbers to consider
  ;;
  ;; @returns {i32} The sum of the squares of the first N natural numbers
  ;;
  (func $sumOfSquares (export "sumOfSquares") (param $max i32) (result i32)
    (local $top i32)
    
    local.get $max
    local.get $max
    i32.const 1
    i32.add
    i32.mul
    local.get $max
    i32.const 2
    i32.mul
    i32.const 1
    i32.add
    i32.mul
    i32.const 6
    i32.div_u
    return
  )

  ;;
  ;; Calculate the difference between the square of the sum and the sum of the 
  ;; squares of the first N natural numbers.
  ;;
  ;; @param {i32} max - The upper bound (inclusive) of natural numbers to consider
  ;;
  ;; @returns {i32} Difference between the square of the sum and the sum of the
  ;;                squares of the first N natural numbers.
  ;;
  (func (export "difference") (param $max i32) (result i32)
    (call $squareOfSum (local.get $max))
    (call $sumOfSquares (local.get $max))
    i32.sub
    return
  )
)
