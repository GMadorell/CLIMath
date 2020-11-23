
# CLIMath - No Non-sense Arithmetic Operations on the CLI

Do you have problems working with complex arithmetic operations on the CLI?

Are you still using a hacked together bash layer on top of `bc`?

Do you have problems handling different non-standardised numbers such as `123.678,123` and `1,578,632.23` in the same operation?

CLIMath to the rescue!

## Example Usage
```shell
alias math="amm --silent CLIMath.sc"

$ math 10 + 4 + 2
> 16

$ math "10 * (2 + 1.24)"
> 30.24

$ math 100 000. 01 + 345,545.12 + 456.678.213,39
> 457123758.52
```

## Installation
CLIMath is an Ammonite script, so it requires an Ammonite installation to work. Follow the details here: https://ammonite.io/#ScalaScripts.

As CLIMath is a single script. Installation is as easy as downloading it from GitHub and placing it somewhere visible in your path.

```
curl -o CLIMath.sc https://raw.githubusercontent.com/GMadorell/CLIMath/main/CLIMath.sc
```

The command above will download the latest version of the script into a file called 'CLIMath.sc'.


## How to test
```
$ amm Tests.sc
```