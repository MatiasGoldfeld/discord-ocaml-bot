# Discord OCaml Bot

This is a OCaml code-eval Discord bot build in OCaml using Jane Street's Async and my fork of Mishio595's Dis.ml library. To install, first download Dis.ml from [my repo](https://gitlab.com/MatiasGoldfeld/disml) and install it as follows.

```
opam pin add disml https://gitlab.com/MatiasGoldfeld/disml.git --dev-repo
```

You must also build the Docker image using the following command at the root of the project.

```
docker build -t ocaml/opam:discord .
```