// global declaration
global is = "O42"
global isnt = "X42"

global x = 42
global the_other_one = 10

// main function
fn main() {
    goHome()
    clearLCD()
    
    if x == 42 {
        print(is)
    } else {
        print(isnt)
    }
}
