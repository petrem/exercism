(module
 (memory (export "mem") 1)
 ;; nucletide characters
 (data (i32.const 0) "ACGT")
 ;; nucleotide counts
 (data (i32.const 4*4) "\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00")
 (func $count (export "count") (param $nucleotide i32) (result i32)
       (local $idx i32)

       (local.set $idx (i32.const 0))

       (block $
         (local.get $nucleotide)
         (i32.const 71)  ;; 'G'
        (i32.eq))


  (func (export "countNucleotides")
        (param $offset i32)
        (param $length i32)
        (local $ptr i32)
        (local $last i32)
        ;;(result i32 i32 i32 i32)
        

        (local.set $ptr (local.get $offset))
        (local.set $last (i32.add (local.get $offset) (local.get $length)))

        (block $map_nucleotides_done
         (loop $map_nucleotides
          (br_if $map_nucleotides_done (i32.eq (local.get $ptr) (local.get $last)))
          (call $count (i32.load (local.get $ptr)))
          (br $map_nucleotides (i32.add (local.get $ptr) (i32.const 1)))))

        (return
         (i32.const -1)
         (i32.const -1)
         (i32.const -1)
         (i32.const -1))
  )
)
