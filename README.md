# json2nix

A json to nix converter written in Haskell.
You can use this easily with a flake.
Unformatted json is also supported.

## Usage

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
    "favorite_movie": null,
    "(some)%wild;key": 5
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
      name = "bob";
      age = 19;
    }
    {
      name = "Sem";
      age = 19;
    }
  ];
  favorite_movie = null;
  "(some)%wild;key" = 5;
}
```

You can do so by enabling nix flakes and run the following command in your shell:

```shell
nix run github:sempruijs/json2nix
```

This will install the required tools.
json2nix will ask two questions:

1. Choose a file path.
This could be something like: ```config.json```.

2. Choose a file name for the new nix file.
You can type something or hit enter for the suggestion.
This would be something like: ```config.nix```

## Contributing

If you find a bug please submit [an issue](https://github.com/sempruijs/json2nix/issues) or [pull request](https://github.com/sempruijs/json2nix/pulls).

MIT license


