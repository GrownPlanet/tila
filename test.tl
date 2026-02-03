// global declaration
global string is = "O42 "
global string isnt = "X42 "

global u16 x = 42
global u16 the_other_one = 10

fn do_the_printing() {
    print(is)
}

// main function
fn main() {
    go_home()
    clear_lcd()
    
    if x == 42 {
        do_the_printing()
    }

    if the_other_one == 11 do_the_printing()
    else print(isnt)
}
