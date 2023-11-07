(module
 (memory (export "mem") 1)
 ;; nucletide characters
 ;;;(global $NUCLEOTIDE_CHARS i32 (i32.const 0))
 (data (i32.const 0) "ACGT")
 ;; nucleotide counts
 ;;;(global $NUCLEOTIDE_COUNTS i32 (i32.const 4))
 (data (i32.const 4) "\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00")

 (func $firstByte (export "fst") (param $val i32) (result i32)
       ;; -- (i32.shr_u (local.get $val) (i32.const 24)))
       (i32.and (local.get $val) (i32.const 0x000000ff)))

 (func $nucleotideAt (export "at") (param $pos i32) (result i32)
       (call $firstByte (i32.load (local.get $pos))))
 
 (func $count (export "count") (param $nucleotide i32) (result i32)
       ;; param 0: expects $nucleotide to be 0x000000NN where NN is the ascii code
       ;; result: found or not
       (local $idx i32)
       (local $countPtr i32)
       
       (block $find_nucleotide_not_found
              (block $find_nucleotide_found
                     (loop $find_nucleotide
                           (br_if $find_nucleotide_found
                                  (i32.eq (call $nucleotideAt (local.get $idx))
                                          (local.get $nucleotide)))
                           (local.set $idx (i32.add (local.get $idx) (i32.const 1)))
                           (br_if $find_nucleotide_not_found
                                  (i32.eq (local.get $idx) (i32.const 4)))
                           (br $find_nucleotide)))
              ;; increment count for nucleotide that was found
              (local.tee $countPtr
                         (i32.add (i32.const 4)
                                  (i32.mul (local.get $idx) (i32.const 4))))
              (local.get $countPtr)
              (i32.add (i32.load) (i32.const 1))
              (i32.store)
              (return (i32.const 1)))
       (return (i32.const 0)))

  (func (export "countNucleotides")
        (param $offset i32)
        (param $length i32)
        (result i32 i32 i32 i32)
        (local $ptr i32)
        (local $last i32)
        
        (local.set $ptr (local.get $offset))
        (local.set $last (i32.add (local.get $offset) (local.get $length)))

        (block $map_nucleotides_done
               (loop $map_nucleotides
                     (br_if $map_nucleotides_done
                            (i32.eq (local.get $ptr) (local.get $last)))
                     (call $count (call $firstByte (i32.load (local.get $ptr))))
                     (if (then nop)
                         (else (return (i32.const -1)
                                       (i32.const -1)
                                       (i32.const -1)
                                       (i32.const -1))))
                     (local.set $ptr (i32.add (local.get $ptr) (i32.const 1)))
                     (br $map_nucleotides)))

        (return (i32.load (i32.const 4))
                (i32.load (i32.const 8))
                (i32.load (i32.const 12))
                (i32.load (i32.const 16))))
)
