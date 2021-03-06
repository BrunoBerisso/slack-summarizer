# slack-summarizer

This is intended to be a slash command for Slack to summarize the messages on a channel. The current implementation is an MVP that fetch a number of messages from the current channel, concatenate all the text and call an summarizer algorithm running on [Algorithmia](https://algorithmia.com/algorithms/nlp/Summarizer).

*Example:*
> /summarize 40

Next on the roadmap:
- Summarize the unread messages automatically. Now the user has to apps the number of messages to fetch as a parameter.
- Handle errors grasefully. Now it only fails, the Slack API has a couple of response formats and it will be nice to handle them.
- Improve the summarize algorithm. The one used now is for general propouse text, it will be great if we could use one tailored for conversations.

## Development

The project use [serverless-haskell](http://hackage.haskell.org/package/serverless-haskell) whci is a plugin for serverless that take care of building and deploying the Haskell code. There are already a `start` and `deploy` commands on the [package.json](.package.json) so you can just clone the repo and run
```
$ yarn start
```
to start a local server to test the function and
```
$ yarn deploy
```
to deploy it.

During the development process you can also run
```
$ stack ghci
```
to compile your code, run tests and check types.
