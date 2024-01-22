(module
  ;;
  ;; Determine if a year is a leap year
  ;;
  ;; @param {i32} year - The year to check
  ;;
  ;; @returns {i32} 1 if leap year, 0 otherwise
  ;;
 (func (export "isLeap") (param $year i32) (result i32)
       (local.get $year)
       (i32.const 4)
       (i32.rem_u)
       (if (result i32)
           (then  ;; not divisible by 4
            (return (i32.const 0)))
           (else  ;; divisible by 4
            (local.get $year)
            (i32.const 100)
            (i32.rem_u)
            (if (result i32)
                (then ;; not divisible by 100
                 (return (i32.const 1)))
                (else  ;; divisible by 100
                 (local.get $year)
                 (i32.const 400)
                 (i32.rem_u)
                 (if (result i32)
                     (then  ;; not divisible by 400
                      (return (i32.const 0)))
                     (else  ;; divisible by 400
                      (return (i32.const 1))))))))
  )
)
