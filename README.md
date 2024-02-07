# dotfiles
All dotfiles that need to be tracked.

This particular version uses `GNU Stow`

0. Install `GNU Stow`
1. Clone this repo to a place where you want your config files, e.g., at the moment of writing I like `~/src/dotfiles`.
2. Call `stow */` to create all the links to all the configs or call `stow [config I want to restore]` to restore just one.

For **WHATEVER REASON**, just invoking `stow */` as in the [video I saw](https://www.youtube.com/watch?v=90xMTKml9O0) does not work, so I had to do `stow [config that I'm restoring] --target=$HOME` and that worked 


