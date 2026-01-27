// global variables
global x = 0
global y = 0

// 8x8 sprite
global player = @{
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
global message = "Hello world!"

// functions have no input to keep the code efficient, and I am too lazy to implement parameters
func move_player() {
    if pressed(kUp) { y = y - 1 }
    if pressed(kDown) { y = y + 1 }
    if pressed(kLeft) { x = x - 1 }
    if pressed(kRight) { x = x + 1 }
}

func main() {
    // local variables
    let running = 1

    // built-in functions
    print(message)

    sleep(1000) // 1 second
    clearLCD()
    goHome()

    while running {
        move_player()

        if pressed(kEsc) { running = 0 }

        draw_sprite(player, x, y)
    }

    clearLCD()
}
