# json2nix

A json to nix converter written in Haskell

## usage

If you have something like this:

```
{
    "name": "Alice",
    "age": 21,
    "length_in_meters": 1.8,
    "best_friends": [
        {
            "name": "bob",
            "age": 19,
        },
        {
            "name": "Sem",
            "age": 19,
        }
    ],
    "favorite_movie": null
}
```

And you want something like this:

```nix
{
  name = "Alice";
  age = 21;
  length_in_meters = 1.8;
  best_friends = [
    {
      name = "Bob";
      age = 19;
    }
    {
      name = "Sem";
      age = 19;
    }
  ];
  favorite_movie = null;
}
```

You can do so by enabling nix flakes and run the following command in your shell:

```shell
nix run github:sempruijs/json2nix
```

This will install the required tools.
json2nix will ask two question:

1. Choose a file path
2. Choose a file name for nix.

## contributing

json2nix is written in Haskell.
If you find a bug please submit an issue or pull sequest.


