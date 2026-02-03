// global variables
global u8 x = 0
global u16 y = 0

// 8x8 sprite
global sprite8x8 player = {
    ..####..,
    .######.,
    ##.##.##,
    ########,
    ##.##.##,
    ###..###,
    .######.,
    ..####..,
}

// strings can only be global
global string message = "Hello world!"

// functions have no input to keep the code efficient, and I am too lazy to implement parameters
fn move_player() {
    if pressed(kUp) { y = y - 1 }
    if pressed(kDown) { y = y + 1 }
    if pressed(kLeft) { x = x - 1 }
    if pressed(kRight) { x = x + 1 }
}

fn main() {
    // local variables
    u8 running = 1

    // calling built-in functions
    clearLCD()
    goHome()

    // built-in functions
    print(message)

    while running {
        move_player()

        if pressed(kEsc) { running = 0 }

        draw_sprite(player, x, y)
    }

    clearLCD()
}
