# Discord OCaml Bot

This is a OCaml code-eval Discord bot build in OCaml using Jane Street's Async and my fork of Mishio595's Dis.ml library. To install, first download Dis.ml from [my repo](https://gitlab.com/MatiasGoldfeld/disml) and install it as follows.

```
opam pin add disml https://gitlab.com/MatiasGoldfeld/disml.git --dev-repo
```

To use the bot, you need to supply it with a bot token as environment variable `DISCORD_OCAML_BOT_TOKEN`. You can also optionally change the default timeout of 15 seconds with the float `DISCORD_OCAML_BOT_TIMEOUT`.
