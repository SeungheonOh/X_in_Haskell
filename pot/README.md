# Pot
[Pot](https://github.com/SeungheonOh/pot) is one of my Go project that prints images and videos on the terminal screen. 
This is simple recreation of Pot in Haskell. It utilizes Haskell's lazy array to stream pixels from FFmpeg. While it doesn't have fancy sub-pixel rendering that original Pot has, this program prints video and images with Haskell Laziness!

## Dependencies
- ffmpeg
- terminal emulator with true color support. (Most modern terminals have it)