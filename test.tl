// global declaration
global is = "x is 42!"
global isnt = "x isn't 42!"
global x = 42

// main function
fn main() {
    goHome()
    clearLCD()
    
    if x == 42 {
        print(is)
    }
    // else {
    //     print(isnt)
    // }
}
