{
    eggs = $0
    count = 0
    for(eggs=$0; eggs > 0; eggs = rshift(eggs, 1)) {
        count = count + and(eggs, 1)
    }
    print count
}
