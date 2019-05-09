/* Create an InMemoryCache */
let inMemoryCache = ApolloInMemoryCache.createInMemoryCache();

type headers = Js.t({. "authorization": string});

[@bs.deriving abstract]
type data = {name: string};

[@bs.scope "JSON"] [@bs.val]
external parseIntoMyData: string => data = "parse";

let storageToHeaders = (jwt: Context.Auth.ServerJwt.t) =>
  Json.Encode.(
    object_([("authorization", Json.Encode.string("Bearer " ++ jwt))])
  );

let httpLink = ApolloLinks.createHttpLink(~uri=Env.serverUrl, ());

let contextLink = (tokens: Context.Auth.ServerJwt.t) =>
  ApolloLinks.createContextLink(() => {"headers": storageToHeaders(tokens)});

let errorLink =
  ApolloLinks.apolloLinkOnError(error => Js.log2("GraphQL Error!", error));

let link = () =>
  switch (Context.Auth.ServerJwt.make_from_storage()) {
  | Some(s) => ApolloLinks.from([|contextLink(s), errorLink, httpLink|])
  | None => ApolloLinks.from([|errorLink, httpLink|])
  };

/* TODO: Don't always load devtools. */
let instance = () =>
  ReasonApollo.createApolloClient(
    ~link=link(),
    ~cache=inMemoryCache,
    ~connectToDevTools=true,
    (),
  );