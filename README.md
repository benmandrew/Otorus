# Otorus

This is a multithreaded torus ray-tracer, with frontends for rendering to a window, or directly in the terminal in real time.

Explanation of multithreading can be found [here](https://www.benmandrew.com/articles/parallelising-a-software-ray-tracer).

Explanation of terminal rendering can be found [here](https://www.benmandrew.com/articles/terminal-renderer).

## Running

```console
$ dune exec -- otorus --terminal
$ dune exec -- otorus --window
```

The rendered scene can be configured in [bin/config.ml](bin/config.ml), with other parameters in [bin/main.ml](bin/main.ml).

## Terminal

Terminal recordings of realtime renders can be found [here](https://www.benmandrew.com/articles/terminal-renderer).

![Rendering in the terminal](https://benmandrew.s3.eu-west-2.amazonaws.com/terminal-renderer/big.png)

![More rendering in the terminal](https://benmandrew.s3.eu-west-2.amazonaws.com/terminal-renderer/tiny.png)

## Window

![Interlocked tori](https://benmandrew.s3.eu-west-2.amazonaws.com/parallel/interlocked.gif)

![Olympic rings](https://benmandrew.s3.eu-west-2.amazonaws.com/parallel/olympic.gif)

