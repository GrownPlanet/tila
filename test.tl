// global declaration
global is = "x is 10!"
global isnt = "x isn't 10!"
global x = 10

// main function
fn main() {
    goHome()
    clearLCD()
    
    if x == 10 {
        print(is)
    }
    // else {
    //     print(isnt)
    // }
}
