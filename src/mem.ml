let new_ref v = ref v
let deref r = !r
let assign r v = r := v
let free _r = ()