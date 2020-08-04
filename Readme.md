# NixBot for Telegram

Simple usage
```shell
$ cat config.json

{ 
    "nixInstantiatePath" : "/run/current-system/sw/bin/nix-instantiate",
    "nixPath" : [ "nixpkgs=/nix/store/arm3y37s9k9z5axyya210r0z1fryp2cs-nixpkgs/nixpkgs" ],
    "token" : "<your telegram bot token>"
}

$ nix-build

$ ./result/bin/nixbot-telegram config.json
```
