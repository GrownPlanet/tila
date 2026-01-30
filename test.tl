// global declaration
global is = "O42"
global isnt = "X42"

global x = 42
global the_other_one = 10

fn do_the_printing() {
    print(is)
}

// main function
fn main() {
    goHome()
    clearLCD()
    
    if x == 42 {
        do_the_printing()
    } else {
        print(isnt)
    }
}
