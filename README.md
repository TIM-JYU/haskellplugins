## TIM Haskellplugins

A collection of TIM plugins written in Haskell:

* Choices: A plugin for simple multiple choice questions
* MultipleChoices: A plugin for stacked multiple choice questions

Also available plugins (not built by default):

* GraphViz: A plugin for rendering diagrams with GraphViz
* ShortNotePlugin: A plugin for open answer questions and for writing notes

> **Note:** The repository exists for legacy purposes! While TIM still supports these plugins, new plugin types exist to replace these.
> Refer to [TIM content creator guide (Finnish only for now)](https://tim.jyu.fi/view/tim/ohjeita/ohjeet#kysymykset-ja-teht%C3%A4v%C3%A4t-muokkausvalikolla) for information on using new plugin types.

## Building

### Local build

First, install the build tool. The easiest way is to grab them with curl:

```
curl -sSL https://get.haskellstack.org/ | sh
```

If this feels insecure, go to https://docs.haskellstack.org/ and pick a more suitable
option.

Then build the project with `stack build`. 

### Docker

You can build a Docker image with all plugins included using `./build.sh`.

## Use

### Local build

After building this you can start the plugins with:

```
stack exec ChoicesPlugin -- -p<port_number>
```

and 

```
stack exec MultipleChoicesPlugin -- -p<port_number>
```

#### Installation

Sorry, no install script yet. You need to grab all .js, *.html and *.css files as well as the binaries.
The command `stack path --local-install-root` will tell you where the binaries are. You can also build
a docker container for the plugins with `stack image container`

### Docker

You can run all plugins at once using [timimages/haskellplugins]() image:

```
docker run --rm -it -p 5001-5004:5001-5004 timimages/haskellplugins:latest
```
