const _ = require('lodash');
const path = require('path');
const cors = require('cors');
const express = require('express');
const bodyParser = require('body-parser-graphql');

const config = require('./config');
const { runJobs } = require('./async');
const { runListeners } = require('./async/listeners');
const events = require('./async/events');
const emitter = require('./async/emitter');
const { apolloServer } = require('./apollo-server');

{
  // Makes sync flows possible
  runJobs();
  runListeners();
}

const app = express();
app.use(cors());

{
  // Returns the client's files
  const fallbackFile = path.resolve(__dirname, '../../client/dist/index.html');
  const distDir = path.resolve(__dirname, '../../client/dist');

  console.log('Fallback file', fallbackFile);
  console.log('Dist dir', distDir);

  // Returns all routes excluding "/graphql", "/hooks", "/env" as static files
  // or returns fallback page.
  app.get(/^((?!(graphql|hooks|env)).)*$/,
    express.static(distDir),
    (req, res) => res.sendFile(fallbackFile),
  );
}

{
  // Set API_URL only in "Netlify.com" to Backend.
  // Do not set API_URL in "Heroku.com" for Staging
  // (this env is used for PR building too).
  // Do not set API_URL in "Heroku.com" for Production.
  app.get(/^\/env\.([0-9a-z]+)\.js$/, (_req, res) => res.send(
    `window.ENV = { ` +
    `API_URL: "${config.API_URL}", ` +
    `AUTH0_DOMAIN: "${config.AUTH0_DOMAIN}", ` +
    `AUTH0_CLIENT_ID: "${config.AUTH0_CLIENT_ID}", ` +
    `}`
  ));
}

{
  // Supporting of GitHub integration
  const { app: subApp } = require('./github/app');
  app.use('/hooks', subApp);
}

{
  // Supporting of Graphql server
  app.use(bodyParser.graphql());
  apolloServer.applyMiddleware({ app });
}

app.listen({ port: config.PORT }, () => {
  console.log(`Server ready at http://localhost:${config.PORT}`);
  emitter.emit(events.SERVER_IS_READY, app);
});
